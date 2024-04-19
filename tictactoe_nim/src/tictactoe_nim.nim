
from std/strutils import parseInt
from std/random import randomize
from std/parseopt import getopt


from tictactoe_nimpkg/lib import play_game

const
    Usage = "TicTacToe-Nim " & """

  (c) 2024 Jan-Eric Nitschke
Usage:
  tictactoe [options]
Options:
  --X:N[=0]  Set strength for AI playing 'X' (default: No AI)
  --O:N[=0]  Set strength for AI playing 'O' (default: No AI)
  --help     show this help
"""

proc writeHelp() =
    stdout.write(Usage)
    stdout.flushFile()
    quit(0)


when isMainModule:
    randomize()
    var X, O = 0
    for kind, key, val in getopt():
        case kind
        of cmdArgument: writeHelp()
        of cmdLongOption, cmdShortOption:
            case key
            of "X":
                try:
                    X = parseInt(val)
                except ValueError:
                    writeHelp()
            of "O":
                try:
                    O = parseInt(val)
                except ValueError:
                    writeHelp()
            else: writeHelp()
        of cmdEnd: assert(false) # cannot happen
    play_game(X, O)
