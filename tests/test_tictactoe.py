"""Tests tictactoe_python.py"""

from unittest.mock import patch
from tictactoe_python import TicTacToe


class TestTicTacToe:
    """Class to test TicTacToe"""

    def setup_class(self):
        """Setup class getting a TicTacToe object"""
        self.tictactoe = TicTacToe()

    def teardown_class(self):
        """Set tictactoe to None"""
        self.tictactoe = None

    def test_init(self):
        """Tests initialization"""
        assert isinstance(self.tictactoe.board, list)
        assert not self.tictactoe.board
        assert isinstance(self.tictactoe.empty_indicator, str)
        assert self.tictactoe.empty_indicator == "-"

    def test_create_board(self):
        """Tests create_board"""
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

    def test_get_random_first_player(self):
        """Tets get_random_first_player"""
        random_value = self.tictactoe.get_random_first_player()
        assert isinstance(random_value, int)
        assert random_value in range(2)

    def test_fix_spot(self):
        """Tests fix_spot"""
        assert not self.tictactoe.fix_spot(-1, -1, "X")
        assert not self.tictactoe.fix_spot(3, 3, "X")
        assert self.tictactoe.fix_spot(1, 1, "X")
        assert self.tictactoe.board[1][1] == "X"
        assert not self.tictactoe.fix_spot(1, 1, "X")

    def test_is_player_win(self):
        """Tests is_player_win"""
        self.tictactoe.board = [["X", "X", "X"], ["-", "-", "-"], ["-", "-", "-"]]
        assert self.tictactoe.is_player_win("X")
        assert not self.tictactoe.is_player_win("O")
        self.tictactoe.board = [["X", "X", "O"], ["-", "-", "-"], ["-", "-", "-"]]
        assert not self.tictactoe.is_player_win("X")
        assert not self.tictactoe.is_player_win("O")
        self.tictactoe.board = [["X", "O", "O"], ["X", "-", "-"], ["X", "-", "-"]]
        assert self.tictactoe.is_player_win("X")
        assert not self.tictactoe.is_player_win("O")
        self.tictactoe.board = [["X", "O", "O"], ["X", "-", "-"], ["O", "-", "-"]]
        assert not self.tictactoe.is_player_win("X")
        assert not self.tictactoe.is_player_win("O")

        self.tictactoe.board = [["O", "X", "O"], ["X", "O", "-"], ["X", "-", "O"]]
        assert self.tictactoe.is_player_win("O")
        assert not self.tictactoe.is_player_win("X")
        self.tictactoe.board = [["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]]
        assert not self.tictactoe.is_player_win("O")
        assert not self.tictactoe.is_player_win("X")

        self.tictactoe.board = [["O", "X", "O"], ["X", "O", "-"], ["O", "O", "-"]]
        assert self.tictactoe.is_player_win("O")
        assert not self.tictactoe.is_player_win("X")
        self.tictactoe.board = [["O", "X", "O"], ["X", "O", "-"], ["X", "X", "-"]]
        assert not self.tictactoe.is_player_win("O")
        assert not self.tictactoe.is_player_win("X")

    def test_is_board_filled(self):
        """Tests is_board_filled"""
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
        """Tests swap_player_turn"""
        assert self.tictactoe.swap_player_turn("X") == "O"
        assert self.tictactoe.swap_player_turn("O") == "X"

    @patch("builtins.print")
    def test_show_board(self, print_mock):
        """Tests show_board"""
        self.tictactoe.show_board()
        assert print_mock.call_count == 3 * 3 + 3

    @patch("builtins.input")
    def test_get_player_input(self, input_mock):
        """Tests get_player_input"""
        input_mock.side_effect = ["1 0", "1", "", "1 1 1", "-1 1", "3 1", "a 1", "2 2"]
        assert self.tictactoe.get_player_input() == (1, 0)
        assert self.tictactoe.get_player_input() is False
        assert self.tictactoe.get_player_input() is False
        assert self.tictactoe.get_player_input() is False
        assert self.tictactoe.get_player_input() == (-1, 1)
        assert self.tictactoe.get_player_input() == (3, 1)
        assert self.tictactoe.get_player_input() is False
        assert self.tictactoe.get_player_input() == (2, 2)
