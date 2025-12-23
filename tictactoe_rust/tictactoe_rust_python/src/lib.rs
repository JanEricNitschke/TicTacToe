use pyo3::prelude::*;
use tictactoe_rust::TicTacToe;
/// Play the game with the `x_strength` and `o_strength` for the AI.
#[pyfunction]
fn play_game(x_strength: usize, o_strength: usize) {
    let mut tictactoe = TicTacToe::with_ai_strengths(x_strength, o_strength);
    tictactoe.play();
}

/// A Python module implemented in Rust.
#[pymodule]
fn tictactoe_rust_python(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(play_game, m)?)?;
    Ok(())
}
