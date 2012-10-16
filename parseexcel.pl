#!/usr/bin/perl -w
use utf8;
use strict;
use Spreadsheet::ParseExcel;
use Data::Dumper;
use DBI;


$dbh = DBI->connect("dbi:Pg:dbname=$dbname", 'morten', 'hobidor', {AutoCommit => 0});my $stationid=0;

my $filename=$ARGV[0];
if($#ARGV){
    $stationid=$ARGV[1]*1;
}

my $parser   = Spreadsheet::ParseExcel->new();
my $workbook = $parser->parse($filename);

if ( !defined $workbook ) {
   die $parser->error(), ".\n";
}
my %station;
my $noplace = 1;
my $found=0;
for my $worksheet ( $workbook->worksheets() ) {
   my ( $row_min, $row_max ) = $worksheet->row_range();
   my ( $col_min, $col_max ) = $worksheet->col_range();
   for my $row ( $row_min .. $row_max ) {
       if ($noplace){
	   my $cell= $worksheet->get_cell($row,0);#->value();
	   next unless $cell;
	   print($cell->value(),"\n");
	   if ($cell->value() eq 'Stnr')  {
	       $noplace=0;
	       $station{'name'}=$worksheet->get_cell($row+1,1)->value();
	       $station{'nr'}=$worksheet->get_cell($row+1,0)->value();
	       print Dumper(\%station);
	       next;
	   }   
	   die if $row>10;
       }else{
	   my $mndcell;
	   unless($found){
	       $mndcell=$worksheet->get_cell($row-1,2);
	       next unless $mndcell;
	   }
	   $found=$found || ($mndcell->value() eq "Mnd");
	   next if not $found;
	   my $c;
	   my @values;
	   my @uf;
	   for ($c=1;$c<=9;$c++){
	       my $cell=$worksheet->get_cell($row,$c);
	       next unless $cell;
	       $values[$c]=$cell->value();
	   }
	   print($row,"=>",Dumper(\@values));
       }
   }
}

