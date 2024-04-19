import unittest

include tictactoe_nimpkg/lib
test "Minmax works at full depth":
  var board: Board = ['X', '2', '3', '4', '5', '6', '7', '8', '9']
  check get_optimal_spot('O', board) ==
      Move(spot: 5, score: Score.Draw)
