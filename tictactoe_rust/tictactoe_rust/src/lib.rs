/*!
# `tictactoe_rust`

`tictactoe_rust` is a collection of utilities to
allow you to play `TicTacToe` alone or with a friend.
*/

use rand::prelude::SliceRandom;
use rand::Rng;
use std::collections;
use std::ops::Neg;
use std::ops::Not;
use std::thread;
use std::time::Duration;

#[macro_use]
extern crate scan_rules;

/// Defines `Board` as an alias for the 3x3 array of chars used to represent game board.
type Board = [[char; 3]; 3];

/// Defines a struct for the moves being made in the game.
#[derive(Debug, Copy, Clone, Default)]
struct Move {
    /// Row and col attributes for the position where the move should be made.
    /// Positive integers expected to be in the range 0..=2.
    row: usize,
    col: usize,
    /// end_state attribute for the outcome of this move given perfect follow up play by both sides.
    end_state: EndState,
}

impl std::cmp::PartialEq for Move {
    /// Define equalitiy between moves if all three attributes are equal.
    ///
    /// # Arguments
    ///
    /// * `self` - The Move on the left side of the `==`
    /// * `other` - The Move on the right side
    fn eq(&self, other: &Self) -> bool {
        self.row == other.row && self.col == other.col && self.end_state == other.end_state
    }
}

/// Define enum to hold game `EndState` possibilities
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Default)]
enum EndState {
    #[default]
    Loss,
    Draw,
    Win,
}

impl Not for EndState {
    type Output = Self;

    /// Define logical negation of the `EndState`
    fn not(self) -> Self::Output {
        match self {
            Self::Draw => Self::Draw,
            Self::Loss => Self::Win,
            Self::Win => Self::Loss,
        }
    }
}

impl Neg for EndState {
    type Output = Self;

    /// Define -`EndState` to be the same as !`EndState`
    fn neg(self) -> Self::Output {
        !self
    }
}

/// Representing a game of tictactoe
#[derive(Debug, Copy, Clone, Default)]
pub struct TicTacToe {
    // Contains the given board state of the game
    board: Board,
    /// Holds the difficulty setting for the AI
    ai_x_difficulty: usize,
    ai_o_difficulty: usize,
}

/// Returns the char for the other player
///
/// # Arguments
///
/// * `player` - A char of the current player
const fn swap_player(player: char) -> char {
    if player == 'X' {
        return 'O';
    }
    'X'
}

impl TicTacToe {
    /// Returns a `TicTacToe` game with an empty board
    ///
    /// # Examples
    ///
    /// ```
    /// use tictactoe_rust::TicTacToe;
    /// let tictactoe = TicTacToe::new();
    /// ```
    #[must_use]
    pub const fn new() -> Self {
        Self {
            board: [['-'; 3]; 3],
            ai_x_difficulty: 0,
            ai_o_difficulty: 0,
        }
    }

    /// Returns a `TicTacToe` game with an empty board and specified as strengths.
    ///
    /// # Examples
    ///
    /// ```
    /// use tictactoe_rust::TicTacToe;
    /// let tictactoe = TicTacToe::with_ai_strengths(4, 4);
    /// ```
    #[must_use]
    pub const fn with_ai_strengths(x_strength: usize, o_strength: usize) -> Self {
        Self {
            board: [['-'; 3]; 3],
            ai_x_difficulty: x_strength,
            ai_o_difficulty: o_strength,
        }
    }

    /// Starts a game of tictactoe on the instance it is called on
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use tictactoe_rust::TicTacToe;
    /// let mut tictactoe = TicTacToe::new();
    /// tictactoe.play();
    /// ```
    pub fn play(&mut self) {
        let mut player = 'X';
        loop {
            if player == 'X' && self.ai_x_difficulty > 0 {
                self.ai_turn(player, self.ai_x_difficulty);
            } else if player == 'O' && self.ai_o_difficulty > 0 {
                self.ai_turn(player, self.ai_o_difficulty);
            } else {
                self.player_turn(player);
            }

            if self.is_player_win(player) {
                println!("Player {player} wins the game!");
                break;
            }

            if self.is_board_filled() {
                println!("Match Draw!");
                break;
            }

            player = swap_player(player);
        }

        self.show_board();
    }

    /// Performs an optimal AI turn on the current game state
    ///
    /// # Arguments
    ///
    /// * `player` - A char of the currently active player
    /// * `ai_difficulty` - An unsigned integer of the AI difficulty setting
    fn ai_turn(&mut self, player: char, ai_difficulty: usize) {
        println!("AI turn as {player}.");
        self.show_board();

        let Move {
            row,
            col,
            end_state: _,
        } = match ai_difficulty {
            0 => unreachable!("AI difficulty should never be 0!"),
            1 => self.random_move(),
            2 => self.win_move(player),
            3 => self.block_win_move(player),
            _ => self.minmax(player),
        };
        self.board[row][col] = player;
        thread::sleep(Duration::from_secs(1));
    }

    /// Performs a human turn on the current game state
    /// Asks a user for input via the command line
    ///
    /// # Arguments
    ///
    /// * `player` - A char of the currently active player
    fn player_turn(&mut self, player: char) {
        println!("Player {player} turn.");

        self.show_board();
        loop {
            println!("Enter row and column number to fix spot: ");
            // Get user input
            let result = try_readln! {
                (let row: usize, let col: usize) => (row, col)
            };
            // Checks if the read was valid
            match result {
                // Reading worked
                // Then check if it was a valid and open spot
                // If reading worked and input was valid exit the loop
                // If input was invalid continue the loop
                Ok((row, col)) => {
                    if self.fix_spot(player, row - 1, col - 1) {
                        break;
                    }
                }
                // Reading was unsuccessful
                // Continue loop and let user enter new input
                Err(e) => println!("Failed to parse input: {e}"),
            }
        }
    }

    /// Checks if the given player has won the game
    ///
    /// # Arguments
    ///
    /// * `player` - A char of the player to check the win for
    fn is_player_win(&self, player: char) -> bool {
        // Hash maps to store how often the player shows up
        // in each row, column and diagonals
        let mut rows_map = collections::HashMap::new();
        let mut cols_map = collections::HashMap::new();
        let mut diag_map = collections::HashMap::new();
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                if self.board[row][col] == player {
                    // Player shows up +1 time in this row
                    let count = rows_map.entry(row).or_insert(0);
                    *count += 1;
                    // and column
                    let count = cols_map.entry(col).or_insert(0);
                    *count += 1;
                    if row == col {
                        // and on the main diagonal
                        let count = diag_map.entry(0).or_insert(0);
                        *count += 1;
                    }
                    if row == (self.board.len() - col - 1) {
                        // and on the anti-diagonal
                        let count = diag_map.entry(1).or_insert(0);
                        *count += 1;
                    }
                }
            }
        }
        // Check if the player has 3 entries in any row
        for (_, value) in rows_map {
            if value == 3 {
                return true;
            }
        }
        // or column
        for (_, value) in cols_map {
            if value == 3 {
                return true;
            }
        }
        // or diagonal
        for (_, value) in diag_map {
            if value == 3 {
                return true;
            }
        }
        // if not he has not won the game
        false
    }

    /// Checks if the board is filled
    ///
    /// After a negative check if either player has won
    /// this indicates a draw
    fn is_board_filled(&self) -> bool {
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                if self.board[row][col] == '-' {
                    return false;
                }
            }
        }
        true
    }

    /// Prints the current game board to stdout
    ///
    /// # Examples
    ///
    /// ```
    /// use tictactoe_rust::TicTacToe;
    /// let tictactoe = TicTacToe::new();
    /// tictactoe.show_board();
    /// ```
    pub fn show_board(&self) {
        let str_line = "---------------";

        println!("{str_line}");
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                print!("| {} |", self.board[row][col]);
            }
            println!("\n{str_line}");
        }
    }

    /// Fixes the given spot for the given player
    ///
    /// Places the players char in the given spot if it was valid (open and in bounds)
    /// Then returns true
    /// If the spot was invalid a message is printed instead and false returned
    ///
    /// # Arguments
    ///
    /// * `player` - A char of the player making the move
    /// * `row` - An unsigned integer of the row number to make the move on (0 indexed)
    /// * `col` - An unsigned integer of the column number to make the move on (0 indexed)
    fn fix_spot(&mut self, player: char, row: usize, col: usize) -> bool {
        if row >= 3 || col >= 3 {
            println!("Row {} or column {} are out of bounds. They have to be between 1 and 3 inclusive. Try again!", row+1, col+1);
            return false;
        }
        if self.board[row][col] != '-' {
            println!("The position ({}, {}) has already been taken by a player! Please do your move on an empty position.", row+1, col+1);
            return false;
        }
        self.board[row][col] = player;
        true
    }

    /// Gathers all empty cells in the current game state
    fn empty_cells(&self) -> Vec<[usize; 2]> {
        let mut empty_cells_vector = Vec::new();
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                if self.board[row][col] == '-' {
                    empty_cells_vector.push([row, col]);
                }
            }
        }
        empty_cells_vector
    }

    /// Finds the optimal move to make for the given player
    /// for the current game state. Does so via the [minmax algorithm](https://en.wikipedia.org/wiki/Minimax)
    ///
    /// # Arguments
    ///
    /// * `player` - A char of the player to find the best move for
    fn minmax(&mut self, player: char) -> Move {
        let mut best_move = Move {
            row: 0,
            col: 0,
            end_state: EndState::Loss,
        };
        if self.is_player_win(player) {
            best_move.end_state = EndState::Win;
            return best_move;
        }
        if self.is_player_win(swap_player(player)) {
            best_move.end_state = EndState::Loss;
            return best_move;
        }
        let empty_cells = self.empty_cells();
        if empty_cells.is_empty() {
            best_move.end_state = EndState::Draw;
            return best_move;
        }
        if empty_cells.len() == 9 {
            best_move = Move {
                row: rand::thread_rng().gen_range(0..=2),
                col: rand::thread_rng().gen_range(0..=2),
                end_state: EndState::Draw,
            };
            return best_move;
        }
        // return best_move;
        for [row, col] in empty_cells {
            self.board[row][col] = player;
            let Move {
                row: _,
                col: _,
                end_state,
            } = self.minmax(swap_player(player));
            if -end_state >= best_move.end_state {
                best_move = Move {
                    row,
                    col,
                    end_state: -end_state,
                };
            }
            self.board[row][col] = '-';
        }
        best_move
    }

    // Perform a random valid move
    fn random_move(&mut self) -> Move {
        // Get all empty cells
        let empty_cells = self.empty_cells();
        // Pick a random one
        // This should never be called when there are no empty cells
        let cell = empty_cells
            .choose(&mut rand::thread_rng())
            .expect("random_move should never be called with a full board!");
        Move {
            row: cell[0],
            col: cell[1],
            end_state: EndState::Draw,
        }
    }

    fn check_win_conditions(
        &mut self,
        player: char,
        win_conditions: &mut std::collections::HashMap<
            String,
            std::collections::HashSet<(usize, usize)>,
        >,
    ) {
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                /*
                If the given player occupies this cell
                then that reduces the required positions
                in that line by one
                */
                if self.board[row][col] == player {
                    win_conditions
                        .entry("row".to_owned() + &row.to_string())
                        .and_modify(|set| {
                            _ = set.remove(&(row, col));
                        });
                    win_conditions
                        .entry("col".to_owned() + &col.to_string())
                        .and_modify(|set| {
                            _ = set.remove(&(row, col));
                        });
                    if row == col {
                        win_conditions.entry("diag".to_string()).and_modify(|set| {
                            _ = set.remove(&(row, col));
                        });
                    }
                    if row == (self.board.len() - 1 - col) {
                        win_conditions
                            .entry("antidiag".to_string())
                            .and_modify(|set| {
                                _ = set.remove(&(row, col));
                            });
                    }
                }
                // If the opposing player occupies this cell
                // then all lines that contain it become useless
                if self.board[row][col] == swap_player(player) {
                    win_conditions
                        .entry("row".to_owned() + &row.to_string())
                        .and_modify(|set| {
                            set.clear();
                        });
                    win_conditions
                        .entry("col".to_owned() + &col.to_string())
                        .and_modify(|set| {
                            set.clear();
                        });
                    if row == col {
                        win_conditions.entry("diag".to_string()).and_modify(|set| {
                            set.clear();
                        });
                    }
                    if row == (self.board.len() - 1 - col) {
                        win_conditions
                            .entry("antidiag".to_string())
                            .and_modify(|set| {
                                set.clear();
                            });
                    }
                }
            }
        }
    }

    /*
    Tries to find a move where the given player wins on the
    given board. So any line that contains the player twice
    and an empty cell as the last slot
    */
    fn get_winning_move(&mut self, player: char) -> Option<Move> {
        // Build  all of the possible lines
        let mut win_conditions: std::collections::HashMap<
            String,
            std::collections::HashSet<(usize, usize)>,
        > = collections::HashMap::from([
            (
                "row0".to_string(),
                collections::HashSet::from_iter([(0, 0), (0, 1), (0, 2)]),
            ),
            (
                "row1".to_string(),
                collections::HashSet::from_iter([(1, 0), (1, 1), (1, 2)]),
            ),
            (
                "row2".to_string(),
                collections::HashSet::from_iter([(2, 0), (2, 1), (2, 2)]),
            ),
            (
                "col0".to_string(),
                collections::HashSet::from_iter([(0, 0), (1, 0), (2, 0)]),
            ),
            (
                "col1".to_string(),
                collections::HashSet::from_iter([(0, 1), (1, 1), (2, 1)]),
            ),
            (
                "col2".to_string(),
                collections::HashSet::from_iter([(0, 2), (1, 2), (2, 2)]),
            ),
            (
                "diag".to_string(),
                collections::HashSet::from_iter([(0, 0), (1, 1), (2, 2)]),
            ),
            (
                "antidiag".to_string(),
                collections::HashSet::from_iter([(0, 2), (1, 1), (2, 0)]),
            ),
        ]);
        self.check_win_conditions(player, &mut win_conditions);
        // Check if any line requires exactly one more
        // position from the player to be fulfilled
        for value in win_conditions.values() {
            if value.len() == 1 {
                let cell = value
                    .iter()
                    .next()
                    .expect("This is only called when there is exactly one element in the set.");
                return Some(Move {
                    row: cell.0,
                    col: cell.1,
                    end_state: EndState::Draw,
                });
            }
        }
        // There was no winning move
        None
    }

    // Try to perform a winning move
    // If there is none return a random one instead
    fn win_move(&mut self, player: char) -> Move {
        self.get_winning_move(player)
            .map_or_else(|| self.random_move(), |win_move| win_move)
    }

    // Tries to find a move that would block the opponent
    // winning on their next move
    fn get_blocking_move(&mut self, player: char) -> Option<Move> {
        // Just find a move that would make the opponent win
        self.get_winning_move(swap_player(player))
    }

    // Try to find a winning or blocking move
    // If neither exists do a random one instead
    fn block_win_move(&mut self, player: char) -> Move {
        if let Some(win_move) = self.get_winning_move(player) {
            return win_move;
        }
        self.get_blocking_move(player)
            .map_or_else(|| self.random_move(), |block_move| block_move)
    }

    /// Gets the game settings from the user via the command line
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use tictactoe_rust::TicTacToe;
    /// let mut tictactoe = TicTacToe::new();
    /// tictactoe.get_settings();
    /// ```
    pub fn get_settings(&mut self) {
        self.get_ai_difficulty('X');
        self.get_ai_difficulty('O');
    }

    /// Gets settings for AI difficulty
    fn get_ai_difficulty(&mut self, player: char) {
        println!("AI strength settings:");
        println!("1: Easy");
        println!("2: Medium");
        println!("3: Hard");
        println!("4: Impossible");
        loop {
            println!("How strong should the AI be?[1-4]: ");
            // Get user input
            let result = try_readln! {
                (let strength: usize) => strength
            };
            // Checks if the read was valid
            match result {
                // Reading worked
                // Then check if it was a valid and open spot
                // If reading worked and input was valid exit the loop
                // If input was invalid continue the loop
                Ok(strength) => {
                    if strength < 5 {
                        if player == 'X' {
                            self.ai_x_difficulty = strength;
                        } else {
                            self.ai_o_difficulty = strength;
                        }
                        break;
                    }
                }
                // Reading was unsuccessful
                // Continue loop and let user enter new input
                Err(e) => println!("Failed to parse input: {e}"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn move_equality_works() {
        let move1 = Move {
            row: 0,
            col: 1,
            end_state: EndState::Draw,
        };
        let move2 = Move {
            row: 1,
            col: 1,
            end_state: EndState::Loss,
        };
        let move3 = Move {
            row: 0,
            col: 1,
            end_state: EndState::Draw,
        };
        assert_eq!(move1, move3);
        assert_ne!(move2, move3);
        assert_ne!(move1, move2);
    }

    #[test]
    fn endstate_ordering_works() {
        assert!(EndState::Win > EndState::Draw);
        assert!(EndState::Draw > EndState::Loss);
        assert!(EndState::Win > EndState::Loss);
    }

    #[test]
    fn endstate_not_works() {
        assert_eq!(!EndState::Win, EndState::Loss);
        assert_eq!(!EndState::Draw, EndState::Draw);
        assert_eq!(!EndState::Loss, EndState::Win);
    }

    #[test]
    fn endstate_neg_works() {
        assert_eq!(-EndState::Win, EndState::Loss);
        assert_eq!(-EndState::Draw, EndState::Draw);
        assert_eq!(-EndState::Loss, EndState::Win);
    }

    #[test]
    fn new_creates_empty_board() {
        let tictactoe = TicTacToe::new();
        assert_eq!(tictactoe.board, [['-'; 3]; 3]);
        assert_eq!(tictactoe.ai_x_difficulty, 0);
        assert_eq!(tictactoe.ai_o_difficulty, 0);
    }

    #[test]
    fn with_strengths_sets_strengths() {
        let tictactoe = TicTacToe::with_ai_strengths(2, 3);
        assert_eq!(tictactoe.board, [['-'; 3]; 3]);
        assert_eq!(tictactoe.ai_x_difficulty, 2);
        assert_eq!(tictactoe.ai_o_difficulty, 3);
    }

    #[test]
    fn fix_spot_fixes_valid_spot_once() {
        let mut tictactoe = TicTacToe::new();
        assert!(tictactoe.fix_spot('X', 0, 0));
        assert_eq!(tictactoe.board[0][0], 'X');
        assert!(!tictactoe.fix_spot('X', 0, 0));
    }

    #[test]
    fn fix_spot_rejects_invalid_spot() {
        let mut tictactoe = TicTacToe::new();
        assert!(!tictactoe.fix_spot('O', 7, 1));
        assert!(!tictactoe.fix_spot('X', 2, 16));
    }

    #[test]
    fn is_player_win_recognizes_no_winner() {
        let tictactoe = TicTacToe::new();
        assert!(!tictactoe.is_player_win('X'));
        assert!(!tictactoe.is_player_win('O'));
    }

    #[test]
    fn is_player_win_accepts_row() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'X', 'X'], ['-', '-', '-'], ['-', '-', '-']];
        assert!(tictactoe.is_player_win('X'));
        assert!(!tictactoe.is_player_win('O'));
        tictactoe.board = [['X', 'X', 'O'], ['-', '-', '-'], ['O', 'O', 'O']];
        assert!(!tictactoe.is_player_win('X'));
        assert!(tictactoe.is_player_win('O'));
    }

    #[test]
    fn is_player_win_accepts_column() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', '-', '-'], ['X', '-', '-'], ['X', '-', '-']];
        assert!(tictactoe.is_player_win('X'));
        assert!(!tictactoe.is_player_win('O'));
        tictactoe.board = [['X', 'X', 'O'], ['-', '-', 'O'], ['X', 'O', 'O']];
        assert!(!tictactoe.is_player_win('X'));
        assert!(tictactoe.is_player_win('O'));
    }

    #[test]
    fn is_player_win_accepts_diagonals() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', '-', '-'], ['-', 'X', '-'], ['O', '-', 'X']];
        assert!(tictactoe.is_player_win('X'));
        assert!(!tictactoe.is_player_win('O'));
        tictactoe.board = [['X', 'X', 'O'], ['-', 'O', 'X'], ['O', 'X', 'O']];
        assert!(!tictactoe.is_player_win('X'));
        assert!(tictactoe.is_player_win('O'));
        tictactoe.board = [['X', 'O', 'X'], ['O', 'X', 'A'], ['X', 'A', 'O']];
        assert!(tictactoe.is_player_win('X'));
    }

    #[test]
    fn is_board_filled_recognizes_empty_board() {
        let tictactoe = TicTacToe::new();
        assert!(!tictactoe.is_board_filled());
    }

    #[test]
    fn is_board_filled_recognizes_partially_filled_board() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', '-', '-'], ['-', 'X', '-'], ['O', '-', 'X']];
        assert!(!tictactoe.is_board_filled());
        tictactoe.board = [['X', 'X', 'O'], ['-', 'X', 'X'], ['O', 'O', 'X']];
        assert!(!tictactoe.is_board_filled());
    }

    #[test]
    fn is_board_filled_recognizes_filled_board() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'X', 'X'], ['X', 'X', 'X'], ['O', 'X', 'X']];
        assert!(tictactoe.is_board_filled());
        tictactoe.board = [['X', 'X', 'O'], ['O', 'X', 'X'], ['O', 'O', 'X']];
        assert!(tictactoe.is_board_filled());
    }

    #[test]
    fn swap_player_swaps_player() {
        assert_eq!(swap_player('O'), 'X');
        assert_eq!(swap_player('X'), 'O');
    }

    #[test]
    fn empty_cells_works_on_empty_board() {
        let tictactoe = TicTacToe::new();
        let mut found_empty_cells = tictactoe.empty_cells();
        found_empty_cells.sort_unstable();
        let mut expected_empty_cells = vec![
            [0, 0],
            [0, 1],
            [0, 2],
            [2, 0],
            [2, 1],
            [2, 2],
            [1, 0],
            [1, 1],
            [1, 2],
        ];
        expected_empty_cells.sort_unstable();
        assert_eq!(found_empty_cells, expected_empty_cells);
    }

    #[test]
    fn empty_cells_works_on_partially_filled_board() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'O', '-'], ['O', '-', 'X'], ['-', 'X', 'X']];
        let mut found_empty_cells = tictactoe.empty_cells();
        found_empty_cells.sort_unstable();
        let mut expected_empty_cells = vec![[0, 2], [2, 0], [1, 1]];
        expected_empty_cells.sort_unstable();
        assert_eq!(found_empty_cells, expected_empty_cells);
    }

    #[test]
    fn empty_cells_works_on_filled_board() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'O', 'X'], ['O', 'O', 'X'], ['O', 'X', 'X']];
        assert_eq!(tictactoe.empty_cells(), Vec::<[usize; 2]>::new());
    }

    #[test]
    fn minmax_gives_random_first_move() {
        let mut tictactoe = TicTacToe::new();
        let valid_moves: [Move; 9] = [
            Move {
                row: 0,
                col: 0,
                end_state: EndState::Draw,
            },
            Move {
                row: 0,
                col: 1,
                end_state: EndState::Draw,
            },
            Move {
                row: 0,
                col: 2,
                end_state: EndState::Draw,
            },
            Move {
                row: 1,
                col: 0,
                end_state: EndState::Draw,
            },
            Move {
                row: 1,
                col: 1,
                end_state: EndState::Draw,
            },
            Move {
                row: 1,
                col: 2,
                end_state: EndState::Draw,
            },
            Move {
                row: 2,
                col: 0,
                end_state: EndState::Draw,
            },
            Move {
                row: 2,
                col: 1,
                end_state: EndState::Draw,
            },
            Move {
                row: 2,
                col: 2,
                end_state: EndState::Draw,
            },
        ];
        assert!(valid_moves.contains(&tictactoe.minmax('X')));
        assert!(valid_moves.contains(&tictactoe.minmax('X')));
        assert!(valid_moves.contains(&tictactoe.minmax('O')));
        assert!(valid_moves.contains(&tictactoe.minmax('O')));
    }

    #[test]
    fn minmax_determines_correct_final_state() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'X', 'X'], ['X', 'X', 'X'], ['X', 'X', 'X']];
        assert!(tictactoe.is_player_win('X'));
        assert_eq!(
            tictactoe.minmax('X'),
            Move {
                row: 0,
                col: 0,
                end_state: EndState::Win
            }
        );
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 0,
                col: 0,
                end_state: EndState::Loss
            }
        );
        tictactoe.board = [['X', 'O', 'X'], ['O', 'X', 'O'], ['O', 'X', 'O']];
        assert_eq!(
            tictactoe.minmax('X'),
            Move {
                row: 0,
                col: 0,
                end_state: EndState::Draw
            }
        );
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 0,
                col: 0,
                end_state: EndState::Draw
            }
        );
    }

    #[test]
    fn minmax_makes_best_move() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'X', '-'], ['O', 'X', 'O'], ['X', 'O', 'O']];
        assert_eq!(
            tictactoe.minmax('X'),
            Move {
                row: 0,
                col: 2,
                end_state: EndState::Win
            }
        );
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 0,
                col: 2,
                end_state: EndState::Win
            }
        );

        tictactoe.board = [['O', 'O', 'X'], ['X', '-', 'O'], ['-', 'O', 'X']];
        assert_eq!(
            tictactoe.minmax('X'),
            Move {
                row: 1,
                col: 1,
                end_state: EndState::Draw
            }
        );

        tictactoe.board = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']];
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 1,
                col: 1,
                end_state: EndState::Win
            }
        );

        tictactoe.board = [['X', '-', 'X'], ['O', '-', 'A'], ['X', 'A', 'O']];
        let mut result = tictactoe.minmax('O');
        assert_eq!(result.end_state, EndState::Loss);

        tictactoe.board = [['X', '-', '-'], ['-', '-', '-'], ['-', '-', 'O']];
        result = tictactoe.minmax('X');
        assert_eq!(result.end_state, EndState::Win);

        tictactoe.board = [['X', '-', '-'], ['-', '-', '-'], ['-', '-', '-']];
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 1,
                col: 1,
                end_state: EndState::Draw
            }
        );
    }

    #[test]
    fn get_winning_move_finds_row() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', 'X', 'O'], ['X', 'O', '-'], ['X', 'X', '-']];
        assert_eq!(
            tictactoe.get_winning_move('X'),
            Some(Move {
                row: 2,
                col: 2,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_winning_move_finds_col() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', '-', '-'], ['-', '-', '-'], ['O', 'X', 'X']];
        assert_eq!(
            tictactoe.get_winning_move('O'),
            Some(Move {
                row: 1,
                col: 0,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_winning_move_finds_diagonal() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', '-', '-'], ['-', 'O', '-'], ['-', '-', '-']];
        assert_eq!(
            tictactoe.get_winning_move('O'),
            Some(Move {
                row: 2,
                col: 2,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_winning_move_finds_antidiagonal() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['-', '-', '-'], ['-', 'X', '-'], ['X', '-', '-']];
        assert_eq!(
            tictactoe.get_winning_move('X'),
            Some(Move {
                row: 0,
                col: 2,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_winning_move_works_with_no_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', 'X', 'X'], ['-', 'O', '-'], ['O', 'X', '-']];
        assert_eq!(tictactoe.get_winning_move('X'), None);
    }

    #[test]
    fn get_blocking_move_finds_row() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', 'X', 'O'], ['X', 'O', '-'], ['X', 'X', '-']];
        assert_eq!(
            tictactoe.get_blocking_move('O'),
            Some(Move {
                row: 2,
                col: 2,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_blocking_move_finds_col() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', '-', '-'], ['-', '-', '-'], ['O', 'X', 'X']];
        assert_eq!(
            tictactoe.get_blocking_move('X'),
            Some(Move {
                row: 1,
                col: 0,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_blocking_move_finds_diagonal() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', '-', '-'], ['-', 'O', '-'], ['-', '-', '-']];
        assert_eq!(
            tictactoe.get_blocking_move('X'),
            Some(Move {
                row: 2,
                col: 2,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_blocking_move_finds_antidiagonal() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['-', '-', '-'], ['-', 'X', '-'], ['X', '-', '-']];
        assert_eq!(
            tictactoe.get_blocking_move('O'),
            Some(Move {
                row: 0,
                col: 2,
                end_state: EndState::Draw
            })
        );
    }

    #[test]
    fn get_blocking_move_works_with_no_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', 'X', 'X'], ['-', 'O', '-'], ['O', 'X', '-']];
        assert_eq!(tictactoe.get_blocking_move('O'), None);
    }

    #[test]
    fn random_move_is_in_range() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']];
        let random_move: Move = tictactoe.random_move();
        assert!(random_move.row < 3);
        assert!(random_move.col < 3);
        assert!(random_move.end_state == EndState::Draw);
    }

    #[test]
    fn random_move_finds_valid_spot() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'X', '-'], ['O', 'X', 'O'], ['X', 'O', 'O']];
        assert_eq!(
            tictactoe.random_move(),
            Move {
                row: 0,
                col: 2,
                end_state: EndState::Draw
            }
        );
    }

    #[test]
    fn win_move_prioritizes_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['O', '-', '-'], ['-', '-', '-'], ['O', 'X', 'X']];
        assert_eq!(
            tictactoe.win_move('O'),
            Move {
                row: 1,
                col: 0,
                end_state: EndState::Draw
            }
        );
    }

    #[test]
    fn win_move_works_without_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']];
        let result_move: Move = tictactoe.win_move('X');
        assert!(result_move.row < 3);
        assert!(result_move.col < 3);
        assert!(result_move.end_state == EndState::Draw);
    }

    #[test]
    fn block_win_move_prioritizes_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', '-', 'O'], ['-', '-', '-'], ['X', '-', 'O']];
        assert_eq!(
            tictactoe.block_win_move('X'),
            Move {
                row: 1,
                col: 0,
                end_state: EndState::Draw
            }
        );
    }

    #[test]
    fn block_win_move_blocks_when_no_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', '-', 'O'], ['-', '-', '-'], ['-', '-', 'O']];
        assert_eq!(
            tictactoe.block_win_move('X'),
            Move {
                row: 1,
                col: 2,
                end_state: EndState::Draw
            }
        );
    }

    #[test]
    fn block_win_move_works_without_win() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['-', '-', '-'], ['-', '-', '-'], ['-', '-', '-']];
        let result_move: Move = tictactoe.block_win_move('X');
        assert!(result_move.row < 3);
        assert!(result_move.col < 3);
        assert!(result_move.end_state == EndState::Draw);
    }
}
