#!/usr/bin/perl
use warnings;
use strict;

# Enable autoflush for STDOUT
$| = 1;

my @WINNING_COMBINATIONS = (
    [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ],    # Rows
    [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ],    # Columns
    [ 0, 4, 8 ], [ 2, 4, 6 ]                  # Diagonals
);

sub change_player {
    my $player = shift;
    return $player eq 'X' ? 'O' : 'X';
}

sub check_draw {
    my @board = @{ shift() };
    foreach my $cell (@board) {
        if ( $cell ne 'X' && $cell ne 'O' ) {
            return 0;
        }
    }
    return 1;
}

sub check_win {
    my $board  = shift;
    my $player = shift;

    foreach my $combination (@WINNING_COMBINATIONS) {
        my $win = 1;
        foreach my $cell (@$combination) {
            if ( $board->[$cell] ne $player ) {
                $win = 0;
                last;
            }
        }
        if ($win) {
            return 1;
        }
    }
    return 0;
}

sub player_turn {
    my $board  = shift;
    my $player = shift;
    my $cell;

    while (1) {
        print "Player $player, enter your move (0-8): \n";
        print_board($board);
        $cell = <STDIN>;
        chomp $cell;
        unless ( $cell =~ /^[0-8]$/ ) {
            print "Invalid move. Enter a number between 0 and 8.\n";
            next;
        }
        unless ( $board->[$cell] ne 'X' && $board->[$cell] ne 'O' ) {
            print "Invalid move. That space is already taken.\n";
            next;
        }
        last;
    }
    $board->[$cell] = $player;
}

sub print_board {
    my $board = shift;
    print " $board->[0] | $board->[1] | $board->[2]\n";
    print "---+---+---\n";
    print " $board->[3] | $board->[4] | $board->[5]\n";
    print "---+---+---\n";
    print " $board->[6] | $board->[7] | $board->[8]\n";
}

sub play_game {
    my @board  = ( 0, 1, 2, 3, 4, 5, 6, 7, 8 );
    my $player = 'X';

    while (1) {
        player_turn( \@board, $player );

        if ( check_win( \@board, $player ) ) {
            print "Player $player wins!\n";
            last;
        }
        if ( check_draw( \@board ) ) {
            print "It's a draw!\n";
            last;
        }
        $player = change_player($player);
    }
    print_board( \@board );
}

&play_game
