from std.testing import assert_equal, TestSuite
from tictactoe.game import Move, BOARD, X, O, OPEN, _swap_player, _minmax


def test_minmax() raises:
    var board = BOARD[16](
        X,
        O,
        OPEN,
        OPEN,
        X,
        O,
        OPEN,
        OPEN,
        X,
        O,
        OPEN,
        OPEN,
        OPEN,
        OPEN,
        OPEN,
        OPEN,
    )
    var result = _minmax[X, 4](board)
    assert_equal(result.score, 1)
    assert_equal(result.spot, 12)


def test_swap_player() raises:
    assert_equal(_swap_player(X), O)
    assert_equal(_swap_player(O), X)

def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
