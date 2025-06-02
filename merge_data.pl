#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;          # gives us ‘say’
no  autovivification;
binmode STDOUT, ':utf8';
use utf8;

# ── CPAN modules ───────────────────────────────────────────────────────────
use Text::CSV 1.99 ();      # fast CSV parser (XS if available)
use HTTP::Tiny;
use JSON::XS;
use Data::Printer;
use File::Spec;             # portable paths & tmp dir

###############################################################################
##  READ THE TWO CDC WONDER FILES – deaths ####################################
###############################################################################

my %data;  # $data{year}{month}{age_group_5}{deaths}

my @death_files = (
    'data/Underlying Cause of Death, 1999-2020.csv',
    'data/Underlying Cause of Death, 2018-2023.csv',
);

my $covid_deaths_file = 'data/Underlying Cause of Death, 2020-2023, COVID Only.csv';

load_all_causes_deaths_file($_) for @death_files;

load_covid_deaths_file($covid_deaths_file);

sub load_all_causes_deaths_file {
    my ($file) = @_;

    my $csv = Text::CSV->new({
        binary      => 1,
        auto_diag   => 1,
        decode_utf8 => 1,
        sep_char    => ',',
    });

    open my $fh, '<:encoding(UTF-8)', $file
        or die "Cannot open '$file': $!";

    my $header = $csv->getline($fh);   # discard header

    while (my $row = $csv->getline($fh)) {
        my ($notes, $age_lbl, $age_code,
            $year,  $year_code,
            $month, $month_code,
            $deaths) = @$row;

        next if $notes eq 'Total';
        next if $age_code eq 'NS';     # “Not Stated”

        # ── collapse all ≥85 into one label ────────────────────────────────
        $age_code = '85+' if $age_code =~ /^(?:85-89|90-94|95-99|100\+)$/;

        (undef, $month) = split '/', $month_code;   # keep MM from MM/CC

        $data{$year_code}{$month}{$age_code}{deaths} += $deaths;
    }
    close $fh;
}

sub load_covid_deaths_file {
    my ($file) = @_;

    my $csv = Text::CSV->new({
        binary      => 1,
        auto_diag   => 1,
        decode_utf8 => 1,
        sep_char    => ',',
    });

    open my $fh, '<:encoding(UTF-8)', $file
        or die "Cannot open '$file': $!";

    my $header = $csv->getline($fh);   # discard header

    while (my $row = $csv->getline($fh)) {
        my ($notes, $age_lbl, $age_code,
            $year,  $year_code,
            $month, $month_code,
            $deaths) = @$row;

        next if $notes eq 'Total';
        next if $age_code eq 'NS';     # “Not Stated”

        # ── collapse all ≥85 into one label ────────────────────────────────
        $age_code = '85+' if $age_code =~ /^(?:85-89|90-94|95-99|100\+)$/;

        (undef, $month) = split '/', $month_code;   # keep MM from MM/CC

        $data{$year_code}{$month}{$age_code}{covid_deaths} += $deaths;
    }
    close $fh;
}

###############################################################################
##  POPULATION LOOK-UP  (single-year CSVs for *all* years) ####################
###############################################################################

# WONDER 5-yr label → array-ref of single-year ages to sum
my %ages_of = (
  '1'     => [0],            '1-4'   => [1 .. 4],  '5-9'   => [5 .. 9],
  '10-14' => [10 .. 14],     '15-19' => [15 .. 19], '20-24' => [20 .. 24],
  '25-29' => [25 .. 29],     '30-34' => [30 .. 34], '35-39' => [35 .. 39],
  '40-44' => [40 .. 44],     '45-49' => [45 .. 49], '50-54' => [50 .. 54],
  '55-59' => [55 .. 59],     '60-64' => [60 .. 64], '65-69' => [65 .. 69],
  '70-74' => [70 .. 74],     '75-79' => [75 .. 79], '80-84' => [80 .. 84],
  '85+'   => [85 .. 100],
);

my %pop_cache;                  # $pop_cache{year}{age_lbl} = population
my $http = HTTP::Tiny->new;

sub get_population {
    my ($year, $age_lbl) = @_;

    $pop_cache{$year} //= fetch_single_year_csv($year);
    return $pop_cache{$year}{$age_lbl};
}

#──────────────────────────────────────────────────────────────────────────────
sub fetch_single_year_csv {
    my ($year) = @_;

    # Two vintages cover 2015-2024
    my %vintage = (
        pre2020 => {
            file   => 'data/nc-est2019-agesex-res.csv',
            urls   => [
                'https://www2.census.gov/programs-surveys/popest/'
                 .'technical-documentation/file-layouts/2010-2019/'
                 .'nc-est2019-agesex-res.csv',
                # fallback (older location)
                'https://www2.census.gov/programs-surveys/popest/'
                 .'datasets/2010-2019/national/asrh/'
                 .'nc-est2019-agesex-res.csv',
            ],
        },
        v2024  => {
            file   => 'data/nc-est2024-agesex-res.csv',
            urls   => [
                'https://www2.census.gov/programs-surveys/popest/'
                 .'technical-documentation/file-layouts/2020-2024/'
                 .'nc-est2024-agesex-res.csv',
                # fallback (older location)
                'https://www2.census.gov/programs-surveys/popest/'
                 .'datasets/2020-2024/national/asrh/'
                 .'nc-est2024-agesex-res.csv',
            ],
        },
    );

    my $key  = $year <= 2019 ? 'pre2020' : 'v2024';
    my $file = $vintage{$key}{file};

    # ── download once, trying each URL until one works ────────────────────
    unless (-f $file) {
        my $ok;
        for my $u ( @{ $vintage{$key}{urls} } ) {
            my $res = $http->get($u);
            next unless $res->{success};
            open my $tmp, '>', $file or die $!;
            print {$tmp} $res->{content};
            close $tmp;
            $ok = 1, last;
        }
        die "All download attempts failed for $file" unless $ok;
    }

    # ── build a big cache per vintage the first time through ──────────────
    state %big;   # {vintage}{age}{yr} = pop
    unless (exists $big{$key}) {
        my $csv = Text::CSV->new({ binary => 1 });
        open my $fh, '<', $file or die $!;
        my $hdr = $csv->getline($fh);
        my %col = map { $hdr->[$_] => $_ } 0 .. $#$hdr;

        while (my $row = $csv->getline($fh)) {
            next unless $row->[ $col{SEX} ] == 0;   # total, not M/F
            my $age = $row->[ $col{AGE} ];
            next if $age == 999;                    # skip “all ages”

            for my $y (2015 .. 2024) {
                my $c = "POPESTIMATE$y"; next unless exists $col{$c};
                $big{$key}{$age}{$y} = $row->[ $col{$c} ];
            }
        }
        close $fh;
    }

    # ── aggregate ages into WONDER buckets, incl. 85+ ─────────────────────
    my %pop;
    for my $lbl (keys %ages_of) {
        $pop{$lbl} = 0;
        $pop{$lbl} += $big{$key}{$_}{$year} // 0 for @{ $ages_of{$lbl} };
    }
    return \%pop;
}


###############################################################################
##  WRITE THE MERGED CSV ######################################################
###############################################################################

open my $out, '>:utf8', 'data/merged_deaths.csv' or die $!;
say $out "year,month,age_group_5,deaths,covid_deaths,population";

for my $year (sort { $a <=> $b } keys %data) {
    for my $month (sort { $a <=> $b } keys %{ $data{$year} }) {
        for my $age (sort keys %{ $data{$year}{$month} }) {

            my $deaths = $data{$year}{$month}{$age}{deaths};
            my $covid_deaths = $data{$year}{$month}{$age}{covid_deaths} // 0;
            my $pop    = get_population($year, $age) // die "No pop for $year $age";

            say $out join ',', $year, $month, $age, $deaths, $covid_deaths, $pop;
        }
    }
}
close $out;

say "Merged file written to data/merged_deaths.csv";
