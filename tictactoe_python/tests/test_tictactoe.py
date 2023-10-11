"""Tests tictactoe.tictactoe_python.py."""

from unittest.mock import MagicMock, patch

from tictactoe.tictactoe_python import TicTacToe


class TestTicTacToe:
    """Class to test TicTacToe."""

    def setup_class(self):
        """Setup class getting a TicTacToe object."""
        self.tictactoe = TicTacToe()

    def teardown_class(self):
        """Set tictactoe to None."""
        self.tictactoe = None

    def test_init(self):
        """Tests initialization."""
        assert isinstance(self.tictactoe.board, list)
        assert not self.tictactoe.board
        assert isinstance(self.tictactoe.empty_indicator, str)
        assert self.tictactoe.empty_indicator == "-"

    def test_create_board(self):
        """Tests create_board."""
        self.tictactoe.create_board()
        assert len(self.tictactoe.board) == 3
        assert isinstance(self.tictactoe.board[0], list)
        assert isinstance(self.tictactoe.board[0][0], str)
        assert self.tictactoe.board[0][0] == self.tictactoe.empty_indicator
        assert self.tictactoe.board == [
            [
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
            ],
            [
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
            ],
            [
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
            ],
        ]

    def test_fix_spot(self):
        """Tests fix_spot."""
        assert not self.tictactoe.fix_spot(-1, -1, "X")
        assert not self.tictactoe.fix_spot(3, 3, "X")
        assert self.tictactoe.fix_spot(1, 1, "X")
        assert self.tictactoe.board[1][1] == "X"
        assert not self.tictactoe.fix_spot(1, 1, "X")

    def test_is_player_win(self):
        """Tests is_player_win."""
        # Row win X
        board = [["X", "X", "X"], ["-", "-", "-"], ["-", "-", "-"]]
        assert self.tictactoe.is_player_win("X", board)
        assert not self.tictactoe.is_player_win("O", board)
        # Row no win
        board = [["X", "X", "O"], ["-", "-", "-"], ["-", "-", "-"]]
        assert not self.tictactoe.is_player_win("X", board)
        assert not self.tictactoe.is_player_win("O", board)
        # Col win X
        board = [["X", "O", "O"], ["X", "-", "-"], ["X", "-", "-"]]
        assert self.tictactoe.is_player_win("X", board)
        assert not self.tictactoe.is_player_win("O", board)
        # Col no win
        board = [["X", "O", "O"], ["X", "-", "-"], ["O", "-", "-"]]
        assert not self.tictactoe.is_player_win("X", board)
        assert not self.tictactoe.is_player_win("O", board)
        # Diagonal win O
        board = [["O", "X", "O"], ["X", "O", "-"], ["X", "-", "O"]]
        assert self.tictactoe.is_player_win("O", board)
        assert not self.tictactoe.is_player_win("X", board)
        # Diagonal no win
        board = [["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]]
        assert not self.tictactoe.is_player_win("O", board)
        assert not self.tictactoe.is_player_win("X", board)
        # Antidiagonal win O
        board = [["O", "X", "O"], ["X", "O", "-"], ["O", "O", "-"]]
        assert self.tictactoe.is_player_win("O", board)
        assert not self.tictactoe.is_player_win("X", board)
        # Antidiagonal no win
        board = [["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]]
        assert not self.tictactoe.is_player_win("O", board)
        assert not self.tictactoe.is_player_win("X", board)

    def test_is_board_filled(self):
        """Tests is_board_filled."""
        self.tictactoe.board = [["X", "X", "X"], ["X", "X", "X"], ["X", "X", "X"]]
        assert self.tictactoe.is_board_filled()
        self.tictactoe.board = [
            [self.tictactoe.empty_indicator, "X", "X"],
            ["X", "X", "X"],
            ["X", "X", "X"],
        ]
        assert not self.tictactoe.is_board_filled()
        self.tictactoe.board = [
            [self.tictactoe.empty_indicator, "X", "X"],
            ["X", "X", "X"],
            ["X", "X", "X"],
        ]
        assert not self.tictactoe.is_board_filled()
        self.tictactoe.board = [
            ["X", "X", "X"],
            ["X", self.tictactoe.empty_indicator, "X"],
            ["X", "X", "X"],
        ]
        assert not self.tictactoe.is_board_filled()
        self.tictactoe.board = [
            ["X", "X", "X"],
            ["X", "X", "X"],
            ["X", "X", self.tictactoe.empty_indicator],
        ]
        assert not self.tictactoe.is_board_filled()

    def test_swap_player_turn(self):
        """Tests swap_player_turn."""
        assert self.tictactoe.swap_player_turn("X") == "O"
        assert self.tictactoe.swap_player_turn("O") == "X"

    @patch("builtins.print")
    def test_show_board(self, print_mock: MagicMock):
        """Tests show_board."""
        self.tictactoe.show_board()
        assert print_mock.call_count == 3 * 3 + 3 + 1

    @patch("builtins.input")
    def test_get_player_input(self, input_mock: MagicMock):
        """Tests get_player_input."""
        input_mock.side_effect = ["1 0", "1", "", "1 1 1", "-1 1", "3 1", "a 1", "2 2"]
        assert self.tictactoe.get_player_input() == (1, 0)
        assert self.tictactoe.get_player_input() is None
        assert self.tictactoe.get_player_input() is None
        assert self.tictactoe.get_player_input() is None
        assert self.tictactoe.get_player_input() == (-1, 1)
        assert self.tictactoe.get_player_input() == (3, 1)
        assert self.tictactoe.get_player_input() is None
        assert self.tictactoe.get_player_input() == (2, 2)

    def test_empty_cells(self):
        """Tests empty_cells."""
        board = [["X", "X", "X"], ["X", "X", "X"], ["X", "X", "X"]]
        assert self.tictactoe.empty_cells(board) == []
        board = [
            ["X", "O", self.tictactoe.empty_indicator],
            ["O", self.tictactoe.empty_indicator, "X"],
            [self.tictactoe.empty_indicator, "X", "X"],
        ]
        assert self.tictactoe.empty_cells(board) == [(0, 2), (1, 1), (2, 0)]

    @patch("builtins.input")
    def test_get_player_number(self, input_mock: MagicMock):
        """Tests get_player_number."""
        input_mock.side_effect = ["y", "n", "", "Y", 1, "N"]
        self.tictactoe.get_player_number()
        assert self.tictactoe.ai_opponent is True
        assert input_mock.call_count == 1
        self.tictactoe.get_player_number()
        assert self.tictactoe.ai_opponent is False
        assert input_mock.call_count == 2
        self.tictactoe.get_player_number()
        assert self.tictactoe.ai_opponent is True
        assert input_mock.call_count == 4
        self.tictactoe.get_player_number()
        assert self.tictactoe.ai_opponent is False
        assert input_mock.call_count == 6

    @patch("builtins.input")
    def test_get_ai_start(self, input_mock: MagicMock):
        """Tests get_ai_start."""
        input_mock.side_effect = ["y", "n", "", "Y", 1, "N"]
        self.tictactoe.get_ai_start()
        assert self.tictactoe.ai_marker == "X"
        assert input_mock.call_count == 1
        self.tictactoe.get_ai_start()
        assert self.tictactoe.ai_marker == "O"
        assert input_mock.call_count == 2
        self.tictactoe.get_ai_start()
        assert self.tictactoe.ai_marker == "X"
        assert input_mock.call_count == 4
        self.tictactoe.get_ai_start()
        assert self.tictactoe.ai_marker == "O"
        assert input_mock.call_count == 6

    @patch("builtins.input")
    def test_get_ai_strength(self, input_mock: MagicMock):
        """Tests get_ai_start."""
        input_mock.side_effect = ["1", "2", "", "3", "X", "4"]
        self.tictactoe.get_ai_strength()
        assert self.tictactoe.ai_function == self.tictactoe.random_move
        assert input_mock.call_count == 1
        self.tictactoe.get_ai_strength()
        assert self.tictactoe.ai_function == self.tictactoe.win_move
        assert input_mock.call_count == 2
        self.tictactoe.get_ai_strength()
        assert self.tictactoe.ai_function == self.tictactoe.block_win_move
        assert input_mock.call_count == 4
        self.tictactoe.get_ai_strength()
        assert self.tictactoe.ai_function == self.tictactoe.minmax
        assert input_mock.call_count == 6

    def test_minmax(self):
        """Tests minmax."""
        board = [["X", "X", "X"], ["X", "X", "X"], ["X", "X", "X"]]
        assert self.tictactoe.minmax(board, "X") == [-1, -1, 1]
        assert self.tictactoe.minmax(board, "O") == [-1, -1, -1]

        board = [
            ["X", "X", self.tictactoe.empty_indicator],
            ["O", "X", "O"],
            ["X", "O", "O"],
        ]
        assert self.tictactoe.minmax(board, "X") == [0, 2, 1]
        assert self.tictactoe.minmax(board, "O") == [0, 2, 1]
        board = [
            ["O", "O", "X"],
            ["X", self.tictactoe.empty_indicator, "O"],
            [self.tictactoe.empty_indicator, "O", "X"],
        ]
        assert self.tictactoe.minmax(board, "X") == [1, 1, 0]
        board = [
            ["O", "O", "X"],
            ["X", self.tictactoe.empty_indicator, self.tictactoe.empty_indicator],
            [self.tictactoe.empty_indicator, "O", "X"],
        ]
        assert self.tictactoe.minmax(board, "O") == [1, 1, 1]
        board = [
            [
                "O",
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
            ],
            [
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
            ],
            [
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
                self.tictactoe.empty_indicator,
            ],
        ]
        assert self.tictactoe.minmax(board, "X") == [1, 1, 0]

    @patch("tictactoe.tictactoe_python.TicTacToe.minmax")
    @patch("tictactoe.tictactoe_python.TicTacToe.show_board")
    def test_ai_turn(self, show_mock: MagicMock, minmax_mock: MagicMock):
        """Tests ai_turn."""
        minmax_mock.side_effect = [[0, 0, 1], [1, 0, 0], [2, 2, 1]]
        self.tictactoe.ai_function = minmax_mock
        self.tictactoe.create_board()
        assert self.tictactoe.board[0][0] == self.tictactoe.empty_indicator
        assert self.tictactoe.board[1][0] == self.tictactoe.empty_indicator
        assert self.tictactoe.board[2][2] == self.tictactoe.empty_indicator
        self.tictactoe.ai_turn("X")
        assert show_mock.call_count == 1
        assert minmax_mock.call_count == 1
        assert self.tictactoe.board[0][0] == "X"
        self.tictactoe.ai_turn("X")
        assert show_mock.call_count == 2
        assert minmax_mock.call_count == 2
        assert self.tictactoe.board[1][0] == "X"
        self.tictactoe.ai_turn("O")
        assert show_mock.call_count == 3
        assert minmax_mock.call_count == 3
        assert self.tictactoe.board[2][2] == "O"

    @patch("tictactoe.tictactoe_python.TicTacToe.fix_spot")
    @patch("tictactoe.tictactoe_python.TicTacToe.get_player_input")
    @patch("tictactoe.tictactoe_python.TicTacToe.show_board")
    def test_player_turn(
        self, show_mock: MagicMock, input_mock: MagicMock, fix_mock: MagicMock
    ):
        """Tests player_turn."""
        input_mock.side_effect = [False, (1, 1), (3, 3)]
        fix_mock.side_effect = [False, True]
        self.tictactoe.player_turn("X")
        assert show_mock.call_count == 3
        assert input_mock.call_count == 3
        assert fix_mock.call_count == 2

    def test_random_move(self):
        """Tests random_move."""
        random_move = self.tictactoe.random_move(
            [
                [
                    self.tictactoe.empty_indicator,
                    self.tictactoe.empty_indicator,
                    self.tictactoe.empty_indicator,
                ],
                [
                    self.tictactoe.empty_indicator,
                    self.tictactoe.empty_indicator,
                    self.tictactoe.empty_indicator,
                ],
                [
                    self.tictactoe.empty_indicator,
                    self.tictactoe.empty_indicator,
                    self.tictactoe.empty_indicator,
                ],
            ],
            "X",
        )
        assert len(random_move) == 3
        assert random_move[0] in range(3)
        assert random_move[1] in range(3)
        assert random_move[2] == 0
        assert self.tictactoe.random_move(
            [
                ["X", "X", self.tictactoe.empty_indicator],
                ["O", "X", "O"],
                ["X", "O", "O"],
            ],
            "X",
        ) == [0, 2, 0]

    @patch("tictactoe.tictactoe_python.TicTacToe._get_winning_move")
    @patch("tictactoe.tictactoe_python.TicTacToe.random_move")
    def test_win_move(self, random_move_mock: MagicMock, winning_move_mock: MagicMock):
        """Tests win_move."""
        winning_move_mock.side_effect = [[0, 0, 1], None]
        random_move_mock.side_effect = [[1, 1, 1]]
        assert self.tictactoe.win_move([], "X") == [0, 0, 1]
        assert winning_move_mock.call_count == 1
        assert random_move_mock.call_count == 0
        assert self.tictactoe.win_move([], "X") == [1, 1, 1]
        assert winning_move_mock.call_count == 2
        assert random_move_mock.call_count == 1

    def test_get_winning_move(self):
        """Tests get_winning_move."""
        # Finds win on row
        board = [["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]]
        assert self.tictactoe._get_winning_move(board, "X") == [2, 2, 0]
        # Finds win on col
        board = [["O", "X", "-"], ["-", "O", "-"], ["O", "X", "X"]]
        assert self.tictactoe._get_winning_move(board, "O") == [1, 0, 0]
        # Finds win on diagonal
        board = [["O", "-", "-"], ["-", "O", "-"], ["-", "-", "-"]]
        assert self.tictactoe._get_winning_move(board, "O") == [2, 2, 0]
        # Finds win on antidiagonal
        board = [["-", "-", "-"], ["-", "X", "-"], ["X", "-", "-"]]
        assert self.tictactoe._get_winning_move(board, "X") == [0, 2, 0]
        # Finds no win
        board = [["O", "X", "X"], ["-", "O", "-"], ["O", "X", "-"]]
        assert self.tictactoe._get_winning_move(board, "X") is None

    @patch("tictactoe.tictactoe_python.TicTacToe._get_winning_move")
    @patch("tictactoe.tictactoe_python.TicTacToe._get_blocking_move")
    @patch("tictactoe.tictactoe_python.TicTacToe.random_move")
    def test_block_win_move(
        self,
        random_move_mock: MagicMock,
        blocking_move_mock: MagicMock,
        winning_move_mock: MagicMock,
    ):
        """Tests block_win_move."""
        winning_move_mock.side_effect = [[0, 0, 1], None, None]
        blocking_move_mock.side_effect = [[2, 2, -1], None]
        random_move_mock.side_effect = [[1, 1, 0]]
        assert self.tictactoe.block_win_move([], "X") == [0, 0, 1]
        assert winning_move_mock.call_count == 1
        assert blocking_move_mock.call_count == 0
        assert random_move_mock.call_count == 0
        assert self.tictactoe.block_win_move([], "X") == [2, 2, -1]
        assert winning_move_mock.call_count == 2
        assert blocking_move_mock.call_count == 1
        assert random_move_mock.call_count == 0
        assert self.tictactoe.block_win_move([], "X") == [1, 1, 0]
        assert winning_move_mock.call_count == 3
        assert blocking_move_mock.call_count == 2
        assert random_move_mock.call_count == 1

    def test_get_blocking_move(self):
        """Tests get_blocking_move."""
        # Finds block on row
        board = [["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]]
        assert self.tictactoe._get_blocking_move(board, "O") == [2, 2, 0]
        # Finds block on col
        board = [["O", "-", "-"], ["-", "-", "-"], ["O", "X", "X"]]
        assert self.tictactoe._get_blocking_move(board, "X") == [1, 0, 0]
        # Finds block on diagonal
        board = [["O", "-", "-"], ["-", "O", "-"], ["-", "-", "-"]]
        assert self.tictactoe._get_blocking_move(board, "X") == [2, 2, 0]
        # Finds block on antidiagonal
        board = [["-", "-", "-"], ["-", "X", "-"], ["X", "-", "-"]]
        assert self.tictactoe._get_blocking_move(board, "O") == [0, 2, 0]
        # Finds no block
        board = [["O", "X", "X"], ["-", "O", "-"], ["O", "X", "-"]]
        assert self.tictactoe._get_blocking_move(board, "O") is None
