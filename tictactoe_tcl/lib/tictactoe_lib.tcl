# Register the package
package provide tictactoe 1.0
package require Tcl       8.6


# Create the namespace
namespace eval ::tictactoe {
    # Export commands
    namespace export play

    variable win_conditions {
        {0 1 2}
        {3 4 5}
        {6 7 8}
        {0 3 6}
        {1 4 7}
        {2 5 8}
        {0 4 8}
        {2 4 6}
    }
}

proc ::tictactoe::play {X_strength O_strength} {
    set board {0 1 2 3 4 5 6 7 8}
    set player "X"
    while {1} {
        if {$player == "X" && $X_strength > 0} {
            ai_turn board $player $X_strength
        } elseif {$player == "O" && $O_strength > 0} {
            ai_turn board $player $O_strength
        } else {
            player_turn board $player
        }

        if {[game_won $board $player]} {
            puts "Player $player wins the game!"
            break
        }
        if {[board_full $board]} {
            puts "Game Drawn!"
            break
        }
        set player [swap_player $player]
    }
    show_board $board
}

proc ::tictactoe::swap_player {player} {
    if {$player == "X"} {
        return "O"
    } else {
        return "X"
    }
}

proc show_board {board} {
    puts "[lindex $board 0] | [lindex $board 1] | [lindex $board 2]"
    puts "---------"
    puts "[lindex $board 3] | [lindex $board 4] | [lindex $board 5]"
    puts "---------"
    puts "[lindex $board 6] | [lindex $board 7] | [lindex $board 8]"
}

proc ::tictactoe::game_won {board player} {
    variable win_conditions
    foreach condition $win_conditions {
        set a [lindex $condition 0]
        set b [lindex $condition 1]
        set c [lindex $condition 2]
        if {[lindex $board $a] == $player && [lindex $board $b] == $player && [lindex $board $c] == $player} {
            return 1
        }
    }
    return 0
}

proc ::tictactoe::board_full {board} {
    foreach cell $board {
        if {$cell != "X" && $cell != "O"} {
            return 0
        }
    }
    return 1
}

proc ::tictactoe::player_turn {board player} {
    upvar $board game_board
    while {1} {
        puts "Player $player, enter your move (0-8): "
        show_board $game_board
        gets stdin input
        if {[string is integer -strict $input] && $input >= 0 && $input <= 8 && [lindex $game_board $input] == $input} {
            lset game_board $input $player
            break
        } else {
            puts "Invalid move. Try again."
        }
    }
}

proc ::tictactoe::get_empty_cells {board} {
    set empty_cells {}
    foreach i $board {
        if {[string is integer -strict $i]} {
            lappend empty_cells $i
        }
    }
    return $empty_cells
}

proc ::tictactoe::ai_random_move {board} {
    set empty_cells [get_empty_cells $board]
    set random_index [expr {int(rand() * [llength $empty_cells])}]
    return [lindex $empty_cells $random_index]
}

proc ::tictactoe::try_win_move {board player} {
    variable win_conditions
    foreach condition $win_conditions {
        set done 0
        set open {}
        foreach spot $condition {
            if {[lindex $board $spot] == $spot} {
                lappend open $spot
            } elseif {[lindex $board $spot] == $player} {
                incr done
            }
        }
        if {$done == 2 && [llength $open] == 1} {
            return [lindex $open 0]
        }
    }
    return ""
}

proc ::tictactoe::ai_win_move {board player} {
    set spot [try_win_move $board $player]
    if {$spot ne ""} {
        return $spot
    }
    return [ai_random_move $board]
}

proc ::tictactoe::ai_win_block_move {board player} {
    set spot [try_win_move $board $player]
    if {$spot ne ""} {
        return $spot
    }
    set spot [try_win_move $board [swap_player $player]]
    if {$spot ne ""} {
        return $spot
    }
    return [ai_random_move $board]
}

proc ::tictactoe::minmax {board player} {
    if {[game_won $board $player]} {
        return [list 1 ""]
    }
    if {[game_won $board [swap_player $player]]} {
        return [list -1 ""]
    }

    set empty_cells [get_empty_cells $board]
    if {[llength $empty_cells] == 0} {
        return [list 0 ""]
    }

    if {[llength $empty_cells] == [llength $board]} {
        return [list 0 [ai_random_move $board]]
    }

    set best_move [list -1 ""]
    foreach spot $empty_cells {
        lset board $spot $player
        set score [expr -1 * [lindex [minmax $board [swap_player $player]] 0]]
        lset board $spot $spot  ;# Reset to original spot
        if {$score >= [lindex $best_move 0]} {
            set best_move [list $score $spot]
        }
    }
    return $best_move
}

proc ::tictactoe::ai_best_move {board player} {
    return [lindex [minmax $board $player] 1]
}

proc ::tictactoe::ai_turn {board player strength} {
    upvar $board game_board
    puts "AI turn as player $player with strength $strength."
    show_board $game_board
    switch $strength {
        1 {
            set move [ai_random_move $game_board]
        }
        2 {
            set move [ai_win_move $game_board $player]
        }
        3 {
            set move [ai_win_block_move $game_board $player]
        }
        default {
            set move [ai_best_move $game_board $player]
        }
    }
    lset game_board $move $player
    after 1000
}
