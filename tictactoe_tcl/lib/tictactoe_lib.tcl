# Register the package
package provide tictactoe 1.0
package require Tcl       8.6


# Create the namespace
namespace eval ::tictactoe {
    # Export commands
    namespace export play
}

proc ::tictactoe::play {} {
    puts "Playing TicTacToe"
}
