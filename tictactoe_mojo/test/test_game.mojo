from std.testing import assert_equal, assert_true, TestSuite
from tictactoe.game import (
    Move,
    X,
    O,
    OPEN,
    BoardInt,
    EntryInt,
    PlayerInt,
    _swap_player,
    _minmax,
    _tt_pack,
    _tt_score,
    _tt_spot,
    _tt_flag,
    TT_EXACT,
    TT_LOWER,
    TT_UPPER,
    _is_winner_param,
    _is_winner,
    _is_full,
    _full_mask,
    _player_at,
)


# --- Minimax ---


def test_minmax_x_wins() raises:
    # Board state (4x4):
    #   X O _ _
    #   X O _ _
    #   X O _ _
    #   _ _ _ _
    # X at positions 0, 4, 8 -- one away from winning column 0.
    var x_bits = BoardInt((1 << 0) | (1 << 4) | (1 << 8))
    var o_bits = BoardInt((1 << 1) | (1 << 5) | (1 << 9))

    var result = _minmax[X, 4](x_bits, o_bits)
    assert_equal(result.score, 1)
    assert_equal(result.spot, 12)


def test_minmax_o_perspective() raises:
    # Same board but O to move — O should also find the winning column.
    #   X O _ _
    #   X O _ _
    #   X O _ _
    #   _ _ _ _
    # O at 1, 5, 9 -- one away from winning column 1.
    var x_bits = BoardInt((1 << 0) | (1 << 4) | (1 << 8))
    var o_bits = BoardInt((1 << 1) | (1 << 5) | (1 << 9))

    var result = _minmax[O, 4](x_bits, o_bits)
    assert_equal(result.score, 1)
    assert_equal(result.spot, 13)


def test_minmax_draw() raises:
    # Board near the end with no winning moves for either side.
    #   X O X O
    #   O X O X
    #   X O _ _
    #   _ _ X O
    var x_bits = BoardInt(
        (1 << 0) | (1 << 2) | (1 << 5) | (1 << 7)
        | (1 << 8) | (1 << 14)
    )
    var o_bits = BoardInt(
        (1 << 1) | (1 << 3) | (1 << 4) | (1 << 6)
        | (1 << 9) | (1 << 15)
    )
    # Empty: 10, 11, 12, 13.  No one can complete a line.
    var result = _minmax[X, 4](x_bits, o_bits)
    assert_equal(result.score, 0)


def test_minmax_forced_loss() raises:
    # O has a fork: two lines each needing one more piece.
    #   O O O _       Row 0 needs pos 3.
    #   X O X X       Diagonal (0,5,10,15) needs pos 15.
    #   X _ O _       X can block one, O takes the other.
    #   _ O X X
    var x_bits = BoardInt(
        (1 << 4) | (1 << 6) | (1 << 7) | (1 << 9) | (1 << 11) | (1 << 14)
    )
    var o_bits = BoardInt(
        (1 << 0) | (1 << 1) | (1 << 2) | (1 << 5) | (1 << 10) | (1 << 13)
    )
    var result = _minmax[X, 4](x_bits, o_bits)
    assert_equal(result.score, -1)


# --- Minimax on 3x3 ---


def test_minmax_3x3_draw() raises:
    # Empty 3x3 board — perfect play is a draw.
    var result = _minmax[X, 3](BoardInt(0), BoardInt(0))
    assert_equal(result.score, 0)


def test_minmax_3x3_x_wins() raises:
    # 3x3: X has row 0 nearly complete.
    #   X X _
    #   O O _
    #   _ _ _
    var x_bits = BoardInt((1 << 0) | (1 << 1))
    var o_bits = BoardInt((1 << 3) | (1 << 4))
    var result = _minmax[X, 3](x_bits, o_bits)
    assert_equal(result.score, 1)
    assert_equal(result.spot, 2)


# --- swap_player ---


def test_swap_player() raises:
    assert_equal(_swap_player(X), O)
    assert_equal(_swap_player(O), X)


# --- player_at ---


def test_player_at() raises:
    var x_bits = BoardInt(1 << 5)
    var o_bits = BoardInt(1 << 3)
    assert_equal(_player_at(x_bits, o_bits, 5), X)
    assert_equal(_player_at(x_bits, o_bits, 3), O)
    assert_equal(_player_at(x_bits, o_bits, 0), OPEN)


# --- is_winner (runtime dispatch) ---


def test_is_winner_runtime() raises:
    # X wins column 0 on 4x4
    var x = BoardInt((1 << 0) | (1 << 4) | (1 << 8) | (1 << 12))
    assert_true(_is_winner[4](x, 0, X))
    assert_true(not _is_winner[4](x, 0, O))

    # O wins row 3
    var o = BoardInt((1 << 12) | (1 << 13) | (1 << 14) | (1 << 15))
    assert_true(_is_winner[4](0, o, O))
    assert_true(not _is_winner[4](0, o, X))


# --- TT packing roundtrip tests ---


def test_tt_pack_min_values() raises:
    # score=-1, spot=-1 (no move), flag=EXACT
    var packed = _tt_pack(-1, -1, TT_EXACT)
    assert_equal(_tt_score(packed), -1)
    assert_equal(_tt_spot(packed), -1)
    assert_equal(_tt_flag(packed), TT_EXACT)


def test_tt_pack_max_values() raises:
    # score=+1, spot=15 (max cell on a 4x4 board), flag=UPPER
    var packed = _tt_pack(1, 15, TT_UPPER)
    assert_equal(_tt_score(packed), 1)
    assert_equal(_tt_spot(packed), 15)
    assert_equal(_tt_flag(packed), TT_UPPER)


def test_tt_pack_all_flags() raises:
    for flag in range(3):
        var packed = _tt_pack(0, 5, EntryInt(flag))
        assert_equal(_tt_flag(packed), EntryInt(flag))


# --- Win checks (4x4) ---


def test_win_row_4x4() raises:
    var x = BoardInt((1 << 8) | (1 << 9) | (1 << 10) | (1 << 11))
    assert_true(_is_winner_param[4, X](x, 0))


def test_win_col_4x4() raises:
    var o = BoardInt((1 << 3) | (1 << 7) | (1 << 11) | (1 << 15))
    assert_true(_is_winner_param[4, O](0, o))


def test_win_diag_4x4() raises:
    var x = BoardInt((1 << 0) | (1 << 5) | (1 << 10) | (1 << 15))
    assert_true(_is_winner_param[4, X](x, 0))


def test_win_anti_diag_4x4() raises:
    var o = BoardInt((1 << 3) | (1 << 6) | (1 << 9) | (1 << 12))
    assert_true(_is_winner_param[4, O](0, o))


def test_no_win_incomplete_4x4() raises:
    # 3 of 4 in row 0 — not a win
    var x = BoardInt((1 << 0) | (1 << 1) | (1 << 2))
    assert_true(not _is_winner_param[4, X](x, 0))
    # 3 of 4 in column 0 — not a win
    var y = BoardInt((1 << 0) | (1 << 4) | (1 << 8))
    assert_true(not _is_winner_param[4, X](y, 0))
    # 3 of 4 on diagonal — not a win
    var z = BoardInt((1 << 0) | (1 << 5) | (1 << 10))
    assert_true(not _is_winner_param[4, X](z, 0))


# --- Board helpers ---


def test_full_mask_4x4() raises:
    var mask = _full_mask[4]()
    assert_equal(mask, ~BoardInt(0) >> BoardInt(16 - 16))


def test_full_mask_3x3() raises:
    var mask = _full_mask[3]()
    # 9 lowest bits set
    assert_equal(mask, BoardInt((1 << 9) - 1))


def test_is_full_4x4() raises:
    var x: BoardInt = 0
    var o: BoardInt = 0
    for i in range(16):
        if i % 2 == 0:
            x |= BoardInt(1) << BoardInt(i)
        else:
            o |= BoardInt(1) << BoardInt(i)
    assert_true(_is_full[4](x, o))
    # Remove one piece — no longer full
    assert_true(not _is_full[4](x & ~BoardInt(1), o))


def main() raises:
    TestSuite.discover_tests[__functions_in_module()]().run()
