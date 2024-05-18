#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);
use LibTictactoe;

use Getopt::Long;

# Variables to store the command line arguments
my $x_strength = 0;
my $o_strength = 0;

# Parsing command line arguments
GetOptions(
    'X=i' => \$x_strength,    # Expect an integer value for -X
    'O=i' => \$o_strength     # Expect an integer value for -O
) or die "Error in command line arguments\n";

LibTictactoe::play_game( $x_strength, $o_strength );
