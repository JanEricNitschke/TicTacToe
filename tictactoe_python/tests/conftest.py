"""Fixtures for tictactoe tests."""

import pytest

from tictactoe.tictactoe_python import TicTacToe


@pytest.fixture
def tictactoe() -> TicTacToe:
    """Return a TicTacToe instance.

    Returns:
        TicTacToe: Return a freashly created TicTacToe instance.
    """
    return TicTacToe()
