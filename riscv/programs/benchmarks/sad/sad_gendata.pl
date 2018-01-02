#!/usr/bin/perl -w
#==========================================================================
# sad_gendata.pl
#
# Author : Christopher Batten (cbatten@mit.edu)
# Date   : May 9, 2005
#
(our $usageMsg = <<'ENDMSG') =~ s/^\#//gm;
#
# Simple script which creates an input data set and the reference data 
# for the sad benchmark.
#
ENDMSG

use strict "vars";
use warnings;
no  warnings("once");
use Getopt::Long;

#--------------------------------------------------------------------------
# Command line processing
#--------------------------------------------------------------------------

our %opts;

sub usage()
{

  print "\n";
  print " Usage: sad_gendata.pl [options] \n";
  print "\n";
  print " Options:\n";
  print "  --help  print this message\n";
  print "  --size  size of input data [64]\n";
  print "  --seed  random seed [1]\n";
  print "$usageMsg";

  exit();
}

sub processCommandLine()
{

  $opts{"help"} = 0;
  $opts{"size"} = 64;
  $opts{"seed"} = 1;
  Getopt::Long::GetOptions( \%opts, 'help|?', 'size:i', 'seed:i' ) or usage();
  $opts{"help"} and usage();

}

#--------------------------------------------------------------------------
# Helper Functions
#--------------------------------------------------------------------------

sub printArray
{
  my $arrayType = $_[0];
  my $arrayName = $_[1];
  my $arrayRef  = $_[2];

  my $numCols = 20;
  my $arrayLen = scalar(@{$arrayRef});

  print "".$arrayType." ".$arrayName."[DATA_SIZE] = \n";
  print "{\n";

  if ( $arrayLen <= $numCols ) {
    print "  ";
    for ( my $i = 0; $i < $arrayLen; $i++ ) {
      print sprintf("%3d",$arrayRef->[$i]);
      if ( $i != $arrayLen-1 ) {
        print ", ";
      }
    }
    print "\n";
  }
  
  else {
    my $numRows = int($arrayLen/$numCols);
    for ( my $j = 0; $j < $numRows; $j++ ) {
      print "  ";
      for ( my $i = 0; $i < $numCols; $i++ ) {
        my $index = $j*$numCols + $i;
        print sprintf("%3d",$arrayRef->[$index]);
        if ( $index != $arrayLen-1 ) {
          print ", ";
        }
      }
      print "\n";
    }

    if ( $arrayLen > ($numRows*$numCols) ) {
      print "  ";
      for ( my $i = 0; $i < ($arrayLen-($numRows*$numCols)); $i++ ) {
        my $index = $numCols*$numRows + $i;
        print sprintf("%3d",$arrayRef->[$index]);
        if ( $index != $arrayLen-1 ) {
          print ", ";
        }
      }
      print "\n";
    }

  }

  print  "};\n\n";
}

#--------------------------------------------------------------------------
# Main
#--------------------------------------------------------------------------

sub main()
{

  processCommandLine();
  srand($opts{"seed"});

  my $ssize = $opts{"size"} * $opts{"size"};
  my @values1;
  for ( my $i = 0; $i < $ssize; $i++ ) {
    push( @values1, int(rand(256)) );
  }
  
  my @values2;
  for ( my $i = 0; $i < $ssize; $i++ ) {
    push( @values2, int(rand(256)) );
  }

  my @sad;
  $sad[0] = 0;
  for ( my $i = 0; $i < $ssize; $i++ ) {
    $sad[0] += abs($values1[$i] - $values2[$i]);
  }

  print "\n\#define SAD_SIZE (".$opts{"size"}.")";
  print "\n\#define DATA_SIZE (".$ssize.") \n\n";
  printArray( "unsigned char", "input_data1", \@values1 );
  printArray( "unsigned char", "input_data2", \@values2 );
  printArray( "int",           "verify_data", \@sad );

}

main();

