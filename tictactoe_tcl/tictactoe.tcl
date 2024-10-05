lappend auto_path "./lib"
package require tictactoe 1.0

set X_strength 0
set O_strength 0
if {[llength $argv] >= 1} {
    set X_strength [lindex $argv 0]
}
if {[llength $argv] >= 2} {
    set O_strength [lindex $argv 1]
}
::tictactoe::play $X_strength $O_strength
