#!/usr/bin/env python
"""Script to play ticatactoe."""

import itertools
import random
import sys
from collections.abc import Callable, Iterable, Iterator, Sized
from dataclasses import dataclass
from enum import Enum
from time import sleep
from typing import Self

from typing_extensions import override


class Board(Iterable[list[str]], Sized):
    """Class that represents a TicTacToe board."""

    def __init__(
        self,
        board: list[list[str]] | None = None,
        empty_indicator: str = "-",
        size: int = 3,
    ) -> None:
        """Initialize a board as a list of lists.

        Args:
            board (list[list[str]] | None): 2D list that contains the ticatactoe board. (Default value = None)
            empty_indicator (str): String to indicate an unoccupied spot on the board.
                (Default value = '-')
            size (int): Size of the board. (Default value = 3)
        """
        self._empty_indicator = empty_indicator
        self._size = size
        self._board: list[list[str]] = board or [
            [self._empty_indicator] * self._size for _ in range(self._size)
        ]

    @override
    def __str__(self) -> str:
        """Return the board as a string.

        Returns:
            str: Nice string representation of the board
        """
        line_separator = "---------------"
        lines = [line_separator]
        for row in self._board:
            row_str = "".join(f"| {item} |" for item in row)
            lines.append(row_str)
            lines.append(line_separator)
        return "\n".join(lines)

    @override
    def __repr__(self) -> str:
        """Return the board as a string.

        Returns:
            str: Raw representation of the board data.
        """
        return repr(self._board)

    def __getitem__(self, positions: tuple[int, int]) -> str:
        """Get element from board.

        Args:
            positions (tuple[int, int]): Tuple of row and column index to get from the board.

        Returns:
            str: Item at the given position on the board
        """
        return self._board[positions[0]][positions[1]]

    def __setitem__(self, positions: tuple[int, int], value: str) -> None:
        """Set element on board.

        Args:
            positions (tuple[int, int]): Tuple of row and column index to set on the board.
            value (str): Value to set.
        """
        self._board[positions[0]][positions[1]] = value

    @override
    def __len__(self) -> int:
        """Return the size of the board.

        Returns:
            int: Size of the board.
        """
        return self._size

    @override
    def __iter__(self) -> Iterator[list[str]]:
        """Return an iterator over the board.

        Returns:
            Iterator[list[str]]: Iterator over the board.
        """
        return iter(self._board)


class OrderedEnum(Enum):
    """Enum with order relations propagated from the values."""

    def __ge__(self, other: Self) -> bool:
        """Greater or equal comparison by value.

        Args:
            other (Self): Other value to compare against.

        Returns:
            bool: True if the value is greater or equal to the other value.
        """
        if self.__class__ is other.__class__:
            return self.value >= other.value
        return NotImplemented

    def __gt__(self, other: Self) -> bool:
        """Greater than comparison by value.

        Args:
            other (Self): Other value to compare against.

        Returns:
            bool: True if the value is greater than the other value.
        """
        if self.__class__ is other.__class__:
            return self.value > other.value
        return NotImplemented

    def __le__(self, other: Self) -> bool:
        """Less or equal comparison by value.

        Args:
            other (Self): Other value to compare against.

        Returns:
            bool: True if the value is less or equal to the other value.
        """
        if self.__class__ is other.__class__:
            return self.value <= other.value
        return NotImplemented

    def __lt__(self, other: Self) -> bool:
        """Less than equal comparison by value.

        Args:
            other (Self): Other value to compare against.

        Returns:
            bool: True if the value is less then the other value.
        """
        if self.__class__ is other.__class__:
            return self.value < other.value
        return NotImplemented


class EndState(OrderedEnum):
    """Enum for possible end states of a game."""

    LOSS = -1
    DRAW = 0
    WIN = 1

    def __neg__(self) -> "EndState":
        """Get the opposite outcome of a game.

        Returns:
            'EndState': Opposite outcome of the game.
        """
        return EndState(-self.value)


@dataclass
class Move:
    """Dataclass for an AI function move with position and final game outcome."""

    row: int
    col: int
    score: EndState


AIFunction = Callable[[Board, str], Move]


class TicTacToe:
    """Class that contains the TicTacToe game logic.

    Attributes:
        board (Board): 2D list that contains the ticatactoe board.
        empty_indicator (str): String to indicate that a position has
            not been taken by either player
        ai_opponent (bool): Whether to play alone vs ai (true) or not (false).
            Default is False
        ai_marker (str): Which player the AI is playing as
        ai_function (AIFunction): Which function to use for ai turns
    """

    def __init__(self) -> None:
        """Initialize a game of TicTacToe."""
        self.empty_indicator: str = "-"
        self.board = Board(empty_indicator=self.empty_indicator)
        self.ai_opponent: bool = False
        self.ai_marker: str = "X"
        self.ai_function: AIFunction = self.minmax

    def fix_spot(self, row: int, col: int, player: str) -> bool:
        """Tries to set a given position on the board to the value of the given player.

        Does so if possible and then returns True.
        If the position is already taken or out of bounds
        then no action is taken and False is returned

        Args:
            row (int): Index of the row where the player made their move
            col (int): Index of the column where the player made their move
            player (str): Symbol of the player making the move

        Returns:
            bool: Board modified in place
        """
        if row not in range(3) or col not in range(3):
            print(
                f"Row {row+1} or column {col+1} are out of bounds."
                " They have to be between 1 and 3 inclusive. Try again!"
            )
            return False
        if self.board[row, col] != self.empty_indicator:
            print(
                f"The position ({row+1}, {col+1}) has already been taken by a player!"
                " Please do your move on an empty position."
            )
            return False
        self.board[row, col] = player
        return True

    def _is_row_win(self, player: str, board: Board) -> bool:
        """Checks if the given player has won via row.

        Args:
            player (str): Player for which to check if they have won the game
            board (Board): Board as a list of lists

        Returns:
            bool: Whether the player won via a row
        """
        board_length = len(board)
        for i in range(board_length):
            if win := all(board[i, j] == player for j in range(board_length)):
                return win
        return False

    def _is_column_win(self, player: str, board: Board) -> bool:
        """Checks if the given player has won via column.

        Args:
            player (str): Player for which to check if they have won the game
            board (Board): Board as a list of lists

        Returns:
            bool: Whether the player won via a column
        """
        board_length = len(board)
        for i in range(board_length):
            if win := all(board[j, i] == player for j in range(board_length)):
                return win
        return False

    def _is_diagonal_win(self, player: str, board: Board) -> bool:
        """Checks if the given player has won via diagonals.

        Args:
            player (str): Player for which to check if they have won the game
            board (Board): Board as a list of lists

        Returns:
            bool: Whether the player won via a diagonal
        """
        board_length = len(board)
        if win := all(board[i, i] == player for i in range(board_length)):
            return win

        if win := all(
            board[i, board_length - 1 - i] == player for i in range(board_length)
        ):
            return win
        return False

    def is_player_win(self, player: str, board: Board) -> bool:
        """Checks if the given player has won the game.

        Args:
            player (str): Player for which to check if they have won the game
            board (Board): Board as a list of lists

        Returns:
            bool: Whether the player won
        """
        if self._is_row_win(player, board):
            return True

        if self._is_column_win(player, board):
            return True

        return self._is_diagonal_win(player, board)

    def is_board_filled(self) -> bool:
        """Checks if the board is completely filled (indicating a tie).

        Returns:
            bool: WHether the board is filled.
        """
        for row in self.board:
            for item in row:
                if item == self.empty_indicator:
                    return False
        return True

    def swap_player_turn(self, player: str) -> str:
        """Swaps the active player.

        Args:
            player (str): String of the current player

        Returns:
            str: The new player
        """
        return "X" if player == "O" else "O"

    def get_player_input(self) -> tuple[int, int] | None:
        """Asks the player for input and validates it.

        Returns:
            tuple[int, int] | None: Tuple of whether input is valid
                and if so also the input
        """
        # taking user input
        player_input = input("Enter row and column numbers to fix spot: ").split()
        if len(player_input) != 2:  # noqa: PLR2004
            print(
                f"You entered {len(player_input)} "
                f"value{'' if len(player_input) == 1 else 's'}."
                " You need to enter exactly two integers to defined your position!"
            )
            return None
        try:
            row, col = list(map(int, player_input))
        except ValueError:
            print(
                "At least one of your entered inputs of "
                f"({player_input[0]}, {player_input[1]}) could not be "
                "converted to an integer. Try again!"
            )
            return None
        return (row, col)

    def ai_turn(self, player: str) -> None:
        """Logic for AI taking a turn.

        Args:
            player (str): Which side the AI is playing on
        """
        print(f"AI turn as {player}.")
        print(self.board)
        print(flush=True)
        move = self.ai_function(self.board, player)
        self.board[move.row, move.col] = player
        sleep(1)

    def empty_cells(self, board: Board) -> list[tuple[int, int]]:
        """Get all the empty cells on a given board.

        Args:
            board (Board): Board as a list of lists

        Returns:
            list[tuple[int, int]]: Empty board coordinates
        """
        return [
            (row, col)
            for row, col in itertools.product(range(len(board)), range(len(board)))
            if board[row, col] == self.empty_indicator
        ]

    # Move could probably become a dataclass
    def minmax(self, board: Board, player: str) -> Move:
        """Takes a board state and return the optimal move for the given player.

        Args:
            board (Board): Board as a list of lists
            player (str): The player whose move it currently is

        Returns:
            Move: [row, col, value] of best move
        """
        best_move = Move(-1, -1, EndState.LOSS)
        if self.is_player_win(player, board):
            best_move.score = EndState.WIN
            return best_move
        if self.is_player_win(self.swap_player_turn(player), board):
            best_move.score = EndState.LOSS
            return best_move
        empty_cells = self.empty_cells(board)
        if not empty_cells:
            best_move.score = EndState.DRAW
            return best_move
        if len(empty_cells) == len(board) ** 2:
            return Move(random.randint(0, 2), random.randint(0, 2), EndState.DRAW)
        for row, col in empty_cells:
            board[row, col] = player
            value = self.minmax(board, self.swap_player_turn(player)).score
            if -value >= best_move.score:
                best_move = Move(row, col, -value)
            board[row, col] = self.empty_indicator
        return best_move

    def random_move(self, board: Board, _: str) -> Move:
        """Takes a board state and returns the coordinates of a valid random move.

        Args:
            board (Board): Board as a list of lists
            _ (str): The player whose move it currently is.
                To have a consistent signature between all
                AI move functions

        Returns:
            Move: [row, col, value] of a random valid move
        """
        empty_cells = self.empty_cells(board)
        cell = random.choice(empty_cells)
        return Move(cell[0], cell[1], EndState.DRAW)

    def win_move(self, board: Board, player: str) -> Move:
        """Take a board state and return either a winning or random move.

        Args:
            board (Board): Board as a list of lists
            player (str): The player whose move it currently is.

        Returns:
            Move: [row, col, value] of best move
        """
        winning_move = self._get_winning_move(board, player)
        if winning_move is None:
            return self.random_move(board, player)
        return winning_move

    def _get_winning_move(  # noqa: C901
        self, board: Board, player: str
    ) -> Move | None:
        """Takes a board state and returns the coordinates of a winning move or None.

        Args:
            board (Board): Board as a list of lists
            player (str): The player whose move it currently is.

        Returns:
            Move | None: [row, col, value] of a winning move
                or None if there is none.
        """
        win_conditions: dict[str, set[tuple[int, int]]] = {
            "row0": {(0, 0), (0, 1), (0, 2)},
            "row1": {(1, 0), (1, 1), (1, 2)},
            "row2": {(2, 0), (2, 1), (2, 2)},
            "col0": {(0, 0), (1, 0), (2, 0)},
            "col1": {(0, 1), (1, 1), (2, 1)},
            "col2": {(0, 2), (1, 2), (2, 2)},
            "diag": {(0, 0), (1, 1), (2, 2)},
            "antidiag": {(0, 2), (1, 1), (2, 0)},
        }
        for row_id, row in enumerate(board):
            for col_id, value in enumerate(row):
                if value == player:
                    coords = (row_id, col_id)
                    if coords in (
                        current_condition := win_conditions[f"row{row_id!s}"]
                    ):
                        current_condition.remove(coords)
                    if coords in (
                        current_condition := win_conditions[f"col{col_id!s}"]
                    ):
                        current_condition.remove(coords)
                    if row_id == col_id and coords in (
                        current_condition := win_conditions["diag"]
                    ):
                        current_condition.remove(coords)
                    if row_id == (len(board) - 1 - col_id) and coords in (
                        current_condition := win_conditions["antidiag"]
                    ):
                        current_condition.remove(coords)
                if value == self.swap_player_turn(player):
                    win_conditions[f"row{row_id!s}"].clear()
                    win_conditions[f"col{col_id!s}"].clear()
                    if row_id == col_id:
                        win_conditions["diag"].clear()
                    if row_id == (len(board) - 1 - col_id):
                        win_conditions["antidiag"].clear()
        for moves in win_conditions.values():
            if len(moves) == 1:
                winning_move = moves.pop()
                return Move(winning_move[0], winning_move[1], EndState.DRAW)
        return None

    def block_win_move(self, board: Board, player: str) -> Move:
        """Take a board state and return either a winning, blocking or random move.

        Args:
            board (Board): Board as a list of lists
            player (str): The player whose move it currently is.

        Returns:
            Move: [row, col, value] of best move
        """
        winning_move = self._get_winning_move(board, player)
        if winning_move is not None:
            return winning_move
        blocking_move = self._get_blocking_move(board, player)
        if blocking_move is not None:
            return blocking_move
        return self.random_move(board, player)

    def _get_blocking_move(self, board: Board, player: str) -> Move | None:
        """Takes a board state and returns the coordinates of a blocking move or None.

        Args:
            board (Board): Board as a list of lists
            player (str): The player whose move it currently is.

        Returns:
            Move | None: [row, col, value] of a blocking move
                or None if there is none.
        """
        return self._get_winning_move(board, self.swap_player_turn(player))

    def player_turn(self, player: str) -> None:
        """Logic for AI taking a turn.

        Args:
            player (str): Which side the user is playing on
        """
        valid_move = False
        while not valid_move:
            print(f"Player {player} turn")

            print(self.board)

            player_input = self.get_player_input()
            if not player_input:
                continue
            row, col = player_input

            print()
            # fixing the spot
            valid_move = self.fix_spot(row - 1, col - 1, player)

    def get_player_number(self) -> None:
        """Get input from the player(s) whether it is two real players or one vs AI."""
        self.ai_opponent = self.get_player_yes_no("Play alone vs AI?[y/n]: ")

    def get_ai_start(self) -> None:
        """Get input from the player whether the AI should make the first move."""
        if self.get_player_yes_no("Should the AI make the first move?[y/n]: "):
            self.ai_marker = "X"
        else:
            self.ai_marker = "O"

    def get_player_yes_no(self, question: str) -> bool:
        """Function get a yes/no response from the user.

        Args:
            question (str): The question to ask the user

        Returns:
            bool: Whether the user answered in the affirmative or not
        """
        response = ""
        while response not in ["Y", "N"]:
            try:
                response = input(question).upper()
            except (EOFError, KeyboardInterrupt):
                print("Bye")
                sys.exit()
            except (KeyError, ValueError, AttributeError):
                print("Bad choice")
        return response == "Y"

    def get_ai_strength(self) -> None:
        """Get input regarding the strength of the AI opponent."""
        response = 0
        print("AI strength settings:")
        print("1: Easy")
        print("2: Medium")
        print("3: Hard")
        print("4: Impossible")
        ai_mapping_dict: dict[int, AIFunction] = {
            1: self.random_move,
            2: self.win_move,
            3: self.block_win_move,
            4: self.minmax,
        }
        while response not in ai_mapping_dict:
            try:
                response = int(
                    input(f"How strong should the AI be?{list(ai_mapping_dict)}: ")
                )
            except (EOFError, KeyboardInterrupt):
                print("Bye")
                sys.exit()
            except (KeyError, ValueError, AttributeError):
                print("Bad choice")
        self.ai_function = ai_mapping_dict[response]

    def start(self) -> None:
        """Starts the game and contains the main game loop."""
        self.get_player_number()
        if self.ai_opponent:
            self.get_ai_strength()
            self.get_ai_start()

        player = "X"
        while True:
            if self.ai_opponent and player == self.ai_marker:
                self.ai_turn(player)
            else:
                self.player_turn(player)

            # checking whether current player is won or not
            if self.is_player_win(player, self.board):
                print(f"Player {player} wins the game!")
                break

            # checking whether the game is draw or not
            if self.is_board_filled():
                print("Match Draw!")
                break

            # swapping the turn
            player = self.swap_player_turn(player)

        # showing the final view of board
        print()
        print(self.board)


# starting the game
if __name__ == "__main__":
    tic_tac_toe = TicTacToe()
    tic_tac_toe.start()
