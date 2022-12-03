use tictactoe_rust::TicTacToe;

fn main() {
    let tictactoe = TicTacToe::new();
    println!("{:?}", tictactoe);
    tictactoe.play();
}
