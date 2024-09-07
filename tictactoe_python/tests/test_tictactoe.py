"""Tests tictactoe.tictactoe_python.py."""

from unittest.mock import MagicMock, patch

from tictactoe.tictactoe_python import Board, EndState, Move, TicTacToe


def test_init(tictactoe: TicTacToe):
    """Tests initialization.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    assert isinstance(tictactoe.board, Board)
    assert isinstance(tictactoe.empty_indicator, str)
    assert tictactoe.empty_indicator == "-"


def test_create_board(tictactoe: TicTacToe):
    """Tests create_board.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    assert len(tictactoe.board) == 3
    assert isinstance(tictactoe.board[0, 0], str)
    assert tictactoe.board[0, 0] == tictactoe.empty_indicator
    assert tictactoe.board._board == [
        [
            tictactoe.empty_indicator,
            tictactoe.empty_indicator,
            tictactoe.empty_indicator,
        ],
        [
            tictactoe.empty_indicator,
            tictactoe.empty_indicator,
            tictactoe.empty_indicator,
        ],
        [
            tictactoe.empty_indicator,
            tictactoe.empty_indicator,
            tictactoe.empty_indicator,
        ],
    ]


def test_fix_spot(tictactoe: TicTacToe):
    """Tests fix_spot.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    assert not tictactoe.fix_spot(-1, -1, "X")
    assert not tictactoe.fix_spot(3, 3, "X")
    assert tictactoe.fix_spot(1, 1, "X")
    assert tictactoe.board[1, 1] == "X"
    assert not tictactoe.fix_spot(1, 1, "X")


def test_is_player_win(tictactoe: TicTacToe):
    """Tests is_player_win.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    # Row win X
    board = Board([["X", "X", "X"], ["-", "-", "-"], ["-", "-", "-"]])
    assert tictactoe.is_player_win("X", board)
    assert not tictactoe.is_player_win("O", board)
    # Row no win
    board = Board([["X", "X", "O"], ["-", "-", "-"], ["-", "-", "-"]])
    assert not tictactoe.is_player_win("X", board)
    assert not tictactoe.is_player_win("O", board)
    # Col win X
    board = Board([["X", "O", "O"], ["X", "-", "-"], ["X", "-", "-"]])
    assert tictactoe.is_player_win("X", board)
    assert not tictactoe.is_player_win("O", board)
    # Col no win
    board = Board([["X", "O", "O"], ["X", "-", "-"], ["O", "-", "-"]])
    assert not tictactoe.is_player_win("X", board)
    assert not tictactoe.is_player_win("O", board)
    # Diagonal win O
    board = Board([["O", "X", "O"], ["X", "O", "-"], ["X", "-", "O"]])
    assert tictactoe.is_player_win("O", board)
    assert not tictactoe.is_player_win("X", board)
    # Diagonal no win
    board = Board([["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]])
    assert not tictactoe.is_player_win("O", board)
    assert not tictactoe.is_player_win("X", board)
    # Antidiagonal win O
    board = Board([["O", "X", "O"], ["X", "O", "-"], ["O", "O", "-"]])
    assert tictactoe.is_player_win("O", board)
    assert not tictactoe.is_player_win("X", board)
    # Antidiagonal no win
    board = Board([["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]])
    assert not tictactoe.is_player_win("O", board)
    assert not tictactoe.is_player_win("X", board)


def test_is_board_filled(tictactoe: TicTacToe):
    """Tests is_board_filled.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    tictactoe.board = Board([["X", "X", "X"], ["X", "X", "X"], ["X", "X", "X"]])
    assert tictactoe.is_board_filled()
    tictactoe.board = Board(
        [
            [tictactoe.empty_indicator, "X", "X"],
            ["X", "X", "X"],
            ["X", "X", "X"],
        ]
    )
    assert not tictactoe.is_board_filled()
    tictactoe.board = Board(
        [
            [tictactoe.empty_indicator, "X", "X"],
            ["X", "X", "X"],
            ["X", "X", "X"],
        ]
    )
    assert not tictactoe.is_board_filled()
    tictactoe.board = Board(
        [
            ["X", "X", "X"],
            ["X", tictactoe.empty_indicator, "X"],
            ["X", "X", "X"],
        ]
    )
    assert not tictactoe.is_board_filled()
    tictactoe.board = Board(
        [
            ["X", "X", "X"],
            ["X", "X", "X"],
            ["X", "X", tictactoe.empty_indicator],
        ]
    )
    assert not tictactoe.is_board_filled()


def test_swap_player_turn(tictactoe: TicTacToe):
    """Tests swap_player_turn.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    assert tictactoe.swap_player_turn("X") == "O"
    assert tictactoe.swap_player_turn("O") == "X"


@patch("builtins.input")
def test_get_player_input(input_mock: MagicMock, tictactoe: TicTacToe):
    """Tests get_player_input.

    Args:
        input_mock (MagicMock): Mock of the input function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    input_mock.side_effect = ["1 0", "1", "", "1 1 1", "-1 1", "3 1", "a 1", "2 2"]
    assert tictactoe.get_player_input() == (1, 0)
    assert tictactoe.get_player_input() is None
    assert tictactoe.get_player_input() is None
    assert tictactoe.get_player_input() is None
    assert tictactoe.get_player_input() == (-1, 1)
    assert tictactoe.get_player_input() == (3, 1)
    assert tictactoe.get_player_input() is None
    assert tictactoe.get_player_input() == (2, 2)


def test_empty_cells(tictactoe: TicTacToe):
    """Tests empty_cells.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    board = Board([["X", "X", "X"], ["X", "X", "X"], ["X", "X", "X"]])
    assert tictactoe.empty_cells(board) == []
    board = Board(
        [
            ["X", "O", tictactoe.empty_indicator],
            ["O", tictactoe.empty_indicator, "X"],
            [tictactoe.empty_indicator, "X", "X"],
        ]
    )
    assert tictactoe.empty_cells(board) == [(0, 2), (1, 1), (2, 0)]


@patch("builtins.input")
def test_get_player_number(input_mock: MagicMock, tictactoe: TicTacToe):
    """Tests get_player_number.

    Args:
        input_mock (MagicMock): Mock of the input function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    input_mock.side_effect = ["y", "n", "", "Y", 1, "N"]
    tictactoe.get_player_number()
    assert tictactoe.ai_opponent is True
    assert input_mock.call_count == 1
    tictactoe.get_player_number()
    assert tictactoe.ai_opponent is False
    assert input_mock.call_count == 2
    tictactoe.get_player_number()
    assert tictactoe.ai_opponent is True
    assert input_mock.call_count == 4
    tictactoe.get_player_number()
    assert tictactoe.ai_opponent is False
    assert input_mock.call_count == 6


@patch("builtins.input")
def test_get_ai_start(input_mock: MagicMock, tictactoe: TicTacToe):
    """Tests get_ai_start.

    Args:
        input_mock (MagicMock): Mock of the input function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    input_mock.side_effect = ["y", "n", "", "Y", 1, "N"]
    tictactoe.get_ai_start()
    assert tictactoe.ai_marker == "X"
    assert input_mock.call_count == 1
    tictactoe.get_ai_start()
    assert tictactoe.ai_marker == "O"
    assert input_mock.call_count == 2
    tictactoe.get_ai_start()
    assert tictactoe.ai_marker == "X"
    assert input_mock.call_count == 4
    tictactoe.get_ai_start()
    assert tictactoe.ai_marker == "O"
    assert input_mock.call_count == 6


@patch("builtins.input")
def test_get_ai_strength(input_mock: MagicMock, tictactoe: TicTacToe):
    """Tests get_ai_start.

    Args:
        input_mock (MagicMock): Mock of the input function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    input_mock.side_effect = ["1", "2", "", "3", "X", "4"]
    tictactoe.get_ai_strength()
    assert tictactoe.ai_function == tictactoe.random_move
    assert input_mock.call_count == 1
    tictactoe.get_ai_strength()
    assert tictactoe.ai_function == tictactoe.win_move
    assert input_mock.call_count == 2
    tictactoe.get_ai_strength()
    assert tictactoe.ai_function == tictactoe.block_win_move
    assert input_mock.call_count == 4
    tictactoe.get_ai_strength()
    assert tictactoe.ai_function == tictactoe.minmax
    assert input_mock.call_count == 6


def test_minmax(tictactoe: TicTacToe):
    """Tests minmax.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    board = Board([["X", "X", "X"], ["X", "X", "X"], ["X", "X", "X"]])
    assert tictactoe.minmax(board, "X") == Move(-1, -1, EndState.WIN)
    assert tictactoe.minmax(board, "O") == Move(-1, -1, EndState.LOSS)

    board = Board(
        [
            ["X", "X", tictactoe.empty_indicator],
            ["O", "X", "O"],
            ["X", "O", "O"],
        ]
    )
    assert tictactoe.minmax(board, "X") == Move(0, 2, EndState.WIN)
    assert tictactoe.minmax(board, "O") == Move(0, 2, EndState.WIN)
    board = Board(
        [
            ["O", "O", "X"],
            ["X", tictactoe.empty_indicator, "O"],
            [tictactoe.empty_indicator, "O", "X"],
        ]
    )
    assert tictactoe.minmax(board, "X") == Move(1, 1, EndState.DRAW)
    board = Board(
        [
            ["O", "O", "X"],
            ["X", tictactoe.empty_indicator, tictactoe.empty_indicator],
            [tictactoe.empty_indicator, "O", "X"],
        ]
    )
    assert tictactoe.minmax(board, "O") == Move(1, 1, EndState.WIN)
    board = Board(
        [
            [
                "O",
                tictactoe.empty_indicator,
                tictactoe.empty_indicator,
            ],
            [
                tictactoe.empty_indicator,
                tictactoe.empty_indicator,
                tictactoe.empty_indicator,
            ],
            [
                tictactoe.empty_indicator,
                tictactoe.empty_indicator,
                tictactoe.empty_indicator,
            ],
        ]
    )
    assert tictactoe.minmax(board, "X") == Move(1, 1, EndState.DRAW)


@patch("tictactoe.tictactoe_python.TicTacToe.minmax")
def test_ai_turn(minmax_mock: MagicMock, tictactoe: TicTacToe):
    """Tests ai_turn.

    Args:
        minmax_mock (MagicMock): Mock of minmax function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    minmax_mock.side_effect = [
        Move(0, 0, EndState.WIN),
        Move(1, 0, EndState.DRAW),
        Move(2, 2, EndState.WIN),
    ]
    tictactoe.ai_function = minmax_mock

    assert tictactoe.board[0, 0] == tictactoe.empty_indicator
    assert tictactoe.board[1, 0] == tictactoe.empty_indicator
    assert tictactoe.board[2, 2] == tictactoe.empty_indicator
    tictactoe.ai_turn("X")
    assert minmax_mock.call_count == 1
    assert tictactoe.board[0, 0] == "X"
    tictactoe.ai_turn("X")
    assert minmax_mock.call_count == 2
    assert tictactoe.board[1, 0] == "X"
    tictactoe.ai_turn("O")
    assert minmax_mock.call_count == 3
    assert tictactoe.board[2, 2] == "O"


@patch("tictactoe.tictactoe_python.TicTacToe.fix_spot")
@patch("tictactoe.tictactoe_python.TicTacToe.get_player_input")
def test_player_turn(
    input_mock: MagicMock,
    fix_mock: MagicMock,
    tictactoe: TicTacToe,
):
    """Tests player_turn.

    Args:
        input_mock (MagicMock): Mock of the input function.
        fix_mock (MagicMock): Mock of the fix_spot function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    input_mock.side_effect = [False, (1, 1), (3, 3)]
    fix_mock.side_effect = [False, True]
    tictactoe.player_turn("X")
    assert input_mock.call_count == 3
    assert fix_mock.call_count == 2


def test_random_move(tictactoe: TicTacToe):
    """Tests random_move.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    random_move = tictactoe.random_move(
        Board(
            [
                [
                    tictactoe.empty_indicator,
                    tictactoe.empty_indicator,
                    tictactoe.empty_indicator,
                ],
                [
                    tictactoe.empty_indicator,
                    tictactoe.empty_indicator,
                    tictactoe.empty_indicator,
                ],
                [
                    tictactoe.empty_indicator,
                    tictactoe.empty_indicator,
                    tictactoe.empty_indicator,
                ],
            ]
        ),
        "X",
    )
    assert isinstance(random_move, Move)
    assert random_move.row in range(3)
    assert random_move.col in range(3)
    assert random_move.score == EndState.DRAW
    assert tictactoe.random_move(
        Board(
            [
                ["X", "X", tictactoe.empty_indicator],
                ["O", "X", "O"],
                ["X", "O", "O"],
            ]
        ),
        "X",
    ) == Move(0, 2, EndState.DRAW)


@patch("tictactoe.tictactoe_python.TicTacToe._get_winning_move")
@patch("tictactoe.tictactoe_python.TicTacToe.random_move")
def test_win_move(
    random_move_mock: MagicMock, winning_move_mock: MagicMock, tictactoe: TicTacToe
):
    """Tests win_move.

    Args:
        random_move_mock (MagicMock): Mock of the random_move function.
        winning_move_mock (MagicMock): Mock of the _get_winning_move function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    winning_move_mock.side_effect = [Move(0, 0, EndState.WIN), None]
    random_move_mock.side_effect = [Move(1, 1, EndState.WIN)]
    assert tictactoe.win_move([], "X") == Move(0, 0, EndState.WIN)
    assert winning_move_mock.call_count == 1
    assert random_move_mock.call_count == 0
    assert tictactoe.win_move([], "X") == Move(1, 1, EndState.WIN)
    assert winning_move_mock.call_count == 2
    assert random_move_mock.call_count == 1


def test_get_winning_move(tictactoe: TicTacToe):
    """Tests get_winning_move.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    # Finds win on row
    board = Board([["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]])
    assert tictactoe._get_winning_move(board, "X") == Move(2, 2, EndState.DRAW)
    # Finds win on col
    board = Board([["O", "X", "-"], ["-", "O", "-"], ["O", "X", "X"]])
    assert tictactoe._get_winning_move(board, "O") == Move(1, 0, EndState.DRAW)
    # Finds win on diagonal
    board = Board([["O", "-", "-"], ["-", "O", "-"], ["-", "-", "-"]])
    assert tictactoe._get_winning_move(board, "O") == Move(2, 2, EndState.DRAW)
    # Finds win on antidiagonal
    board = Board([["-", "-", "-"], ["-", "X", "-"], ["X", "-", "-"]])
    assert tictactoe._get_winning_move(board, "X") == Move(0, 2, EndState.DRAW)
    # Finds no win
    board = Board([["O", "X", "X"], ["-", "O", "-"], ["O", "X", "-"]])
    assert tictactoe._get_winning_move(board, "X") is None


@patch("tictactoe.tictactoe_python.TicTacToe._get_winning_move")
@patch("tictactoe.tictactoe_python.TicTacToe._get_blocking_move")
@patch("tictactoe.tictactoe_python.TicTacToe.random_move")
def test_block_win_move(
    random_move_mock: MagicMock,
    blocking_move_mock: MagicMock,
    winning_move_mock: MagicMock,
    tictactoe: TicTacToe,
):
    """Tests block_win_move.

    Args:
        random_move_mock (MagicMock): Mock of the random_move function.
        blocking_move_mock (MagicMock): Mock of the _get_blocking_move function.
        winning_move_mock (MagicMock): Mock of the _get_winning_move function.
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    winning_move_mock.side_effect = [Move(0, 0, EndState.WIN), None, None]
    blocking_move_mock.side_effect = [Move(2, 2, EndState.LOSS), None]
    random_move_mock.side_effect = [Move(1, 1, EndState.DRAW)]
    assert tictactoe.block_win_move([], "X") == Move(0, 0, EndState.WIN)
    assert winning_move_mock.call_count == 1
    assert blocking_move_mock.call_count == 0
    assert random_move_mock.call_count == 0
    assert tictactoe.block_win_move([], "X") == Move(2, 2, EndState.LOSS)
    assert winning_move_mock.call_count == 2
    assert blocking_move_mock.call_count == 1
    assert random_move_mock.call_count == 0
    assert tictactoe.block_win_move([], "X") == Move(1, 1, EndState.DRAW)
    assert winning_move_mock.call_count == 3
    assert blocking_move_mock.call_count == 2
    assert random_move_mock.call_count == 1


def test_get_blocking_move(tictactoe: TicTacToe):
    """Tests get_blocking_move.

    Args:
        tictactoe (TicTacToe): TicTacToe instance to test against.
    """
    # Finds block on row
    board = Board([["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]])
    assert tictactoe._get_blocking_move(board, "O") == Move(2, 2, EndState.DRAW)
    # Finds block on col
    board = Board([["O", "-", "-"], ["-", "-", "-"], ["O", "X", "X"]])
    assert tictactoe._get_blocking_move(board, "X") == Move(1, 0, EndState.DRAW)
    # Finds block on diagonal
    board = Board([["O", "-", "-"], ["-", "O", "-"], ["-", "-", "-"]])
    assert tictactoe._get_blocking_move(board, "X") == Move(2, 2, EndState.DRAW)
    # Finds block on antidiagonal
    board = Board([["-", "-", "-"], ["-", "X", "-"], ["X", "-", "-"]])
    assert tictactoe._get_blocking_move(board, "O") == Move(0, 2, EndState.DRAW)
    # Finds no block
    board = Board([["O", "X", "X"], ["-", "O", "-"], ["O", "X", "-"]])
    assert tictactoe._get_blocking_move(board, "O") is None
