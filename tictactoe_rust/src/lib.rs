type Board = [[char; 3]; 3];

#[derive(Debug, Copy, Clone, Default)]
pub struct TicTacToe {
    board: Board,
    ai_opponent: bool,
}

impl TicTacToe {
    pub fn new() -> TicTacToe {
        TicTacToe {
            board: [['-'; 3]; 3],
            ai_opponent: false,
        }
    }

    pub fn play(&self) {
        println!(
            "Board: {:?}, AI Opponent?: {}",
            self.board, self.ai_opponent
        );
    }
}
