"""Script to play ticatactoe"""
#!/usr/bin/env python

import random
from typing import Union, Literal


class TicTacToe:
    """Class that contains the TicTacToe game logic

    Attributes:
        board (list[list[string]]): 2D list that contains the ticatactoe board.
        empty_indicator (str): String to indicate that a position has not been taken by either player
    """

    def __init__(self):
        self.board = []
        self.empty_indicator = "-"

    def create_board(self) -> None:
        """Initializes the board as a list of lists containting only '-'

        Args:
            None
        Returns:
            None (board modified in place)
        """
        for _ in range(3):
            row = []
            for _ in range(3):
                row.append(self.empty_indicator)
            self.board.append(row)

    def get_random_first_player(self) -> int:
        """Randomly choses which player should make the first move

        Args:
            None

        Returns:
            int (0 or 1 randomly)
        """
        return random.randint(0, 1)

    def fix_spot(self, row: int, col: int, player: str) -> bool:
        """Tries to set a given position on the board to the value of the given player.

        Does so if possible and then returns True.
        If the position is already taken or out of bounds then no action is taken and False is returned

        Args:
            row (int): Index of the row where the player made their move
            col (int): Index of the column where the player made their move
            player (str): Symbol of the player making the move

        Returns:
            bool (board modified in place)
        """
        if row not in range(3) or col not in range(3):
            print(
                f"Row {row+1} or column {col+1} are out of bounds. They have to be between 1 and 3 inclusive. Try again!"
            )
            return False
        if self.board[row][col] != self.empty_indicator:
            print(
                f"The position ({row+1}, {col+1}) has already been taken by a player! Please do your move on an empty position."
            )
            return False
        self.board[row][col] = player
        return True

    def is_player_win(self, player: str) -> bool:
        """Checks if the given player has won the game

        Args:
            player (str): Player for which to check if they have won the game

        Returns:
            bool
        """
        win = None

        n = len(self.board)

        # checking rows
        for i in range(n):
            win = True
            for j in range(n):
                if self.board[i][j] != player:
                    win = False
                    break
            if win:
                return win

        # checking columns
        for i in range(n):
            win = True
            for j in range(n):
                if self.board[j][i] != player:
                    win = False
                    break
            if win:
                return win

        # checking diagonals
        win = True
        for i in range(n):
            if self.board[i][i] != player:
                win = False
                break
        if win:
            return win

        win = True
        for i in range(n):
            if self.board[i][n - 1 - i] != player:
                win = False
                break
        if win:
            return win
        return False

    def is_board_filled(self) -> bool:
        """Checks if the board is completely filled (indicating a tie)

        Args:
            None

        Returns:
            bool
        """
        for row in self.board:
            for item in row:
                if item == self.empty_indicator:
                    return False
        return True

    def swap_player_turn(self, player: str) -> str:
        """Swaps the active player

        Args:
            player (str): String of the current player

        Returns:
            string (of the new player)
        """
        return "X" if player == "O" else "O"

    def show_board(self) -> None:
        """Prints out the current board

        Args:
            None

        Returns:
            None
        """
        for row in self.board:
            for item in row:
                print(item, end=" ")
            print()

    def get_player_input(self) -> Union[Literal[False], tuple[int]]:
        """Asks the player for input and validates it

        Args:
            None

        Returns:
            Tuple of whether input is valid and if so also the input
        """
        # taking user input
        player_input = input("Enter row and column numbers to fix spot: ").split()
        if len(player_input) != 2:
            print(
                f"You entered {len(player_input)} value{'' if len(player_input) == 1 else 's'}. You need to enter exactly two integers to defined your position!"
            )
            return False
        try:
            row, col = list(map(int, player_input))
        except ValueError:
            print(
                f"At least one of your entered inputs of ({player_input[0]}, {player_input[1]}) could not be converted to an integer. Try again!"
            )
            return False
        return (row, col)

    def start(self) -> None:
        """Starts the game and contains the main game loop

        Args:
            None

        Returns:
            None"""
        self.create_board()

        player = "X" if self.get_random_first_player() == 1 else "O"
        while True:

            valid_move = False
            while not valid_move:
                print(f"Player {player} turn")

                self.show_board()

                player_input = self.get_player_input()
                if not player_input:
                    continue
                row, col = player_input

                print()
                # fixing the spot
                valid_move = self.fix_spot(row - 1, col - 1, player)

            # checking whether current player is won or not
            if self.is_player_win(player):
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
        self.show_board()


# starting the game
if __name__ == "__main__":
    tic_tac_toe = TicTacToe()
    tic_tac_toe.start()
