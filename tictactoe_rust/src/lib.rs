use rand::Rng;
use std::collections;
use std::io;
use std::thread;
use std::time::Duration;

#[macro_use]
extern crate scan_rules;

type Board = [[char; 3]; 3];

#[derive(Debug, Copy, Clone, Default)]
struct Move {
    row: usize,
    col: usize,
    value: isize,
}

impl std::cmp::PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        self.row == other.row && self.col == other.col && self.value == other.value
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct TicTacToe {
    board: Board,
    ai_opponent: bool,
    ai_player: char,
}

impl TicTacToe {
    pub fn new() -> TicTacToe {
        TicTacToe {
            board: [['-'; 3]; 3],
            ai_opponent: false,
            ai_player: 'O',
        }
    }

    pub fn play(&mut self) {
        let mut player = 'X';
        loop {
            if self.ai_opponent && self.ai_player == player {
                self.ai_turn(player)
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

            player = self.swap_player(player);
        }

        self.show_board();
    }

    fn ai_turn(&mut self, player: char) {
        println!("AI turn as {player}.");
        self.show_board();
        let Move { row, col, value: _ } = self.minmax(player);
        self.board[row][col] = player;
        thread::sleep(Duration::from_secs(1));
    }

    fn player_turn(&mut self, player: char) {
        println!("Player {player} turn.");

        self.show_board();
        loop {
            println!("Enter row and column number to fix spot: ");
            let result = try_readln! {
                (let row: usize, let col: usize) => (row, col)
            };
            match result {
                Ok((row, col)) => match self.fix_spot(player, row - 1, col - 1) {
                    true => break,
                    false => (),
                },
                Err(e) => println!("Failed to parse input: {}", e),
            }
        }
    }

    fn is_player_win(&self, player: char) -> bool {
        let mut rows_map = collections::HashMap::new();
        let mut cols_map = collections::HashMap::new();
        let mut diag_map = collections::HashMap::new();
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                if self.board[row][col] == player {
                    let count = rows_map.entry(row).or_insert(0);
                    *count += 1;
                    let count = cols_map.entry(col).or_insert(0);
                    *count += 1;
                    if row == col {
                        let count = diag_map.entry(0).or_insert(0);
                        *count += 1;
                    }
                    if row == (self.board.len() - col - 1) {
                        let count = diag_map.entry(1).or_insert(0);
                        *count += 1;
                    }
                }
            }
        }
        for (_, value) in rows_map {
            if value == 3 {
                return true;
            }
        }
        for (_, value) in cols_map {
            if value == 3 {
                return true;
            }
        }
        for (_, value) in diag_map {
            if value == 3 {
                return true;
            }
        }
        false
    }

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

    fn swap_player(&self, player: char) -> char {
        if player == 'X' {
            return 'O';
        }
        'X'
    }

    fn show_board(&self) {
        let str_line = "---------------";

        println!("{str_line}");
        for row in 0..self.board.len() {
            for col in 0..self.board[0].len() {
                print!("| {} |", self.board[row][col]);
            }
            println!("\n{str_line}")
        }
    }

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

    fn minmax(&mut self, player: char) -> Move {
        let mut best_move = Move {
            row: 0,
            col: 0,
            value: -1,
        };
        if self.is_player_win(player) {
            best_move.value = 1;
            return best_move;
        }
        if self.is_player_win(self.swap_player(player)) {
            best_move.value = -1;
            return best_move;
        }
        let empty_cells = self.empty_cells();
        if empty_cells.is_empty() {
            best_move.value = 0;
            return best_move;
        }
        if empty_cells.len() == 9 {
            best_move = Move {
                row: rand::thread_rng().gen_range(0..=2),
                col: rand::thread_rng().gen_range(0..=2),
                value: 0,
            };
            return best_move;
        }
        // return best_move;
        for [row, col] in empty_cells {
            self.board[row][col] = player;
            let Move {
                row: _,
                col: _,
                value,
            } = self.minmax(self.swap_player(player));
            if -value >= best_move.value {
                best_move = Move {
                    row,
                    col,
                    value: -value,
                };
            }
            self.board[row][col] = '-'
        }
        best_move
    }

    pub fn get_settings(&mut self) {
        self.get_ai_opponent_setting()
    }

    fn get_ai_opponent_setting(&mut self) {
        loop {
            let mut choice = String::new();
            println!("Play alone vs AI?[y/n]");
            io::stdin()
                .read_line(&mut choice)
                .expect("Failed to read line");
            choice = choice.trim().to_uppercase();
            if choice == "Y" {
                self.ai_opponent = true;
                self.get_ai_opponent_start();
                break;
            }
            if choice == "N" {
                self.ai_opponent = false;
                break;
            }
        }
    }

    fn get_ai_opponent_start(&mut self) {
        loop {
            let mut choice = String::new();
            println!("Should the AI make the first move?[y/n]");
            io::stdin()
                .read_line(&mut choice)
                .expect("Failed to read line");
            choice = choice.trim().to_uppercase();
            if choice == "Y" {
                self.ai_player = 'X';
                break;
            }
            if choice == "N" {
                self.ai_player = 'O';
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_creates_empty_board() {
        let tictactoe = TicTacToe::new();
        assert_eq!(tictactoe.board, [['-'; 3]; 3]);
        assert!(!tictactoe.ai_opponent);
        assert_eq!(tictactoe.ai_player, 'O');
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
        assert!(tictactoe.is_player_win('X'))
    }

    #[test]
    fn is_board_filled_recognizes_empty_board() {
        let tictactoe = TicTacToe::new();
        assert!(!tictactoe.is_board_filled())
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
        let tictactoe = TicTacToe::new();
        assert_eq!(tictactoe.swap_player('O'), 'X');
        assert_eq!(tictactoe.swap_player('X'), 'O');
    }

    #[test]
    fn empty_cells_works_on_empty_board() {
        let tictactoe = TicTacToe::new();
        let mut found_empty_cells = tictactoe.empty_cells();
        found_empty_cells.sort();
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
        expected_empty_cells.sort();
        assert_eq!(found_empty_cells, expected_empty_cells);
    }

    #[test]
    fn empty_cells_works_on_partially_filled_board() {
        let mut tictactoe = TicTacToe::new();
        tictactoe.board = [['X', 'O', '-'], ['O', '-', 'X'], ['-', 'X', 'X']];
        let mut found_empty_cells = tictactoe.empty_cells();
        found_empty_cells.sort();
        let mut expected_empty_cells = vec![[0, 2], [2, 0], [1, 1]];
        expected_empty_cells.sort();
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
                value: 0,
            },
            Move {
                row: 0,
                col: 1,
                value: 0,
            },
            Move {
                row: 0,
                col: 2,
                value: 0,
            },
            Move {
                row: 1,
                col: 0,
                value: 0,
            },
            Move {
                row: 1,
                col: 1,
                value: 0,
            },
            Move {
                row: 1,
                col: 2,
                value: 0,
            },
            Move {
                row: 2,
                col: 0,
                value: 0,
            },
            Move {
                row: 2,
                col: 1,
                value: 0,
            },
            Move {
                row: 2,
                col: 2,
                value: 0,
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
                value: 1
            }
        );
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 0,
                col: 0,
                value: -1
            }
        );
        tictactoe.board = [['X', 'O', 'X'], ['O', 'X', 'O'], ['O', 'X', 'O']];
        assert_eq!(
            tictactoe.minmax('X'),
            Move {
                row: 0,
                col: 0,
                value: 0
            }
        );
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 0,
                col: 0,
                value: 0
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
                value: 1
            }
        );
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 0,
                col: 2,
                value: 1
            }
        );

        tictactoe.board = [['O', 'O', 'X'], ['X', '-', 'O'], ['-', 'O', 'X']];
        assert_eq!(
            tictactoe.minmax('X'),
            Move {
                row: 1,
                col: 1,
                value: 0
            }
        );

        tictactoe.board = [['O', 'O', 'X'], ['X', '-', '-'], ['-', 'O', 'X']];
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 1,
                col: 1,
                value: 1
            }
        );

        tictactoe.board = [['X', '-', 'X'], ['O', '-', 'A'], ['X', 'A', 'O']];
        let mut result = tictactoe.minmax('O');
        assert_eq!(result.value, -1);

        tictactoe.board = [['X', '-', '-'], ['-', '-', '-'], ['-', '-', 'O']];
        result = tictactoe.minmax('X');
        assert_eq!(result.value, 1);

        tictactoe.board = [['X', '-', '-'], ['-', '-', '-'], ['-', '-', '-']];
        assert_eq!(
            tictactoe.minmax('O'),
            Move {
                row: 1,
                col: 1,
                value: 0
            }
        );
    }
}
