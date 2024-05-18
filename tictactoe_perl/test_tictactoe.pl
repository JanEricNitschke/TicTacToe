#!/usr/bin/perl

use warnings;
use strict;

use File::Basename;
use lib dirname(__FILE__);
use LibTictactoe;

die unless LibTictactoe::change_player('X') eq 'O';
die unless LibTictactoe::change_player('O') eq 'X';
( my $move, my $score ) =
  LibTictactoe::get_perfect_move( [ 'X', 1, 2, 3, 4, 5, 6, 7, 8 ], 'O' );
die unless ( $move == 4 && $score == 0 );
