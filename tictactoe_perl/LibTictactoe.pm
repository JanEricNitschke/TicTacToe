
package LibTictactoe;

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

sub empty_cells {
    my $board = shift;
    return grep { $board->[$_] ne 'X' && $board->[$_] ne 'O' } 0 .. 8;
}

sub random_move {
    my $board = shift;
    my @empty = empty_cells($board);
    return $empty[ int( rand(@empty) ) ];
}

sub try_winning_move {
    my $board  = shift;
    my $player = shift;

    foreach my $combination (@WINNING_COMBINATIONS) {
        my @openSpots  = ();
        my $takenSpots = 0;
        foreach my $cell (@$combination) {
            if ( $board->[$cell] eq $player ) {
                $takenSpots++;
            }
            elsif ( $board->[$cell] ne 'X' && $board->[$cell] ne 'O' ) {
                push @openSpots, $cell;
            }
        }
        if ( ( $takenSpots == 2 ) && ( scalar(@openSpots) == 1 ) ) {
            return $openSpots[0];
        }
    }
    return undef;
}

sub winning_move {
    my $board  = shift;
    my $player = shift;
    my $move   = try_winning_move( $board, $player );
    if ( defined $move ) {
        return $move;
    }
    return random_move($board);
}

sub winning_blocking_move {
    my $board  = shift;
    my $player = shift;
    my $move   = try_winning_move( $board, $player );
    if ( defined $move ) {
        return $move;
    }
    $move = try_winning_move( $board, change_player($player) );
    if ( defined $move ) {
        return $move;
    }
    return random_move($board);
}

sub perfect_move {
    my $board  = shift;
    my $player = shift;
    ( my $spot, my $score ) = get_perfect_move( $board, $player );
    return $spot;
}

sub get_perfect_move {
    my $board  = shift;
    my $player = shift;

    my $bestScore = -1;
    my $bestMove;

    if ( check_win( $board, $player ) ) {
        return ( undef, 1 );
    }

    if ( check_win( $board, change_player($player) ) ) {
        return ( undef, -1 );
    }

    my @empty = empty_cells($board);
    if ( scalar(@empty) == 0 ) {
        return ( undef, 0 );
    }
    if ( scalar(@empty) == 9 ) {
        return random_move($board);
    }

    foreach my $cell (@empty) {
        $board->[$cell] = $player;
        ( my $spot, my $score ) =
          get_perfect_move( $board, change_player($player) );
        $board->[$cell] = $cell;

        if ( -$score >= $bestScore ) {
            $bestScore = -$score;
            $bestMove  = $cell;
        }
    }
    return ( $bestMove, $bestScore );
}

sub ai_turn {
    my $board    = shift;
    my $player   = shift;
    my $strength = shift;

    print "AI turn as player $player with strength $strength \n";
    print_board($board);

    my $move;
    if ( $strength == 1 ) {
        $move = random_move($board);
    }
    elsif ( $strength == 2 ) {
        $move = winning_move( $board, $player );
    }
    elsif ( $strength == 3 ) {
        $move = winning_blocking_move( $board, $player );
    }
    elsif ( $strength == 4 ) {
        $move = perfect_move( $board, $player );
    }
    else {
        die "Invalid AI strength $strength\n";
    }
    $board->[$move] = $player;
    sleep(1);
}

sub play_game {
    my $x_strength = shift;
    my $o_strength = shift;
    print("X strength: $x_strength\n");
    print("O strength: $o_strength\n");

    my @board  = ( 0, 1, 2, 3, 4, 5, 6, 7, 8 );
    my $player = 'X';

    while (1) {
        if ( $player eq 'X' && $x_strength > 0 ) {
            ai_turn( \@board, $player, $x_strength );
        }
        elsif ( $player eq 'O' && $o_strength > 0 ) {
            ai_turn( \@board, $player, $o_strength );
        }
        else {
            player_turn( \@board, $player );
        }
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

1;
