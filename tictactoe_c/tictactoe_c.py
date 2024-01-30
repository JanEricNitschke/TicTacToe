#!/usr/bin/env python3

"""Simple module to play tic-tac-toe via C extension."""

import tictactoe_c


if __name__ == "__main__":
    tictactoe_c.play_game(X_strength=0, O_strength=0)
    help(tictactoe_c)
