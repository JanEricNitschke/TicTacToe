defmodule TictactoeElixirTest do
  use ExUnit.Case
  doctest TictactoeElixir

  test "Identifies free spots" do
    assert TictactoeElixir.spot_free?(["0", "X", "O"], 0) == true
    assert TictactoeElixir.spot_free?(["0", "X", "O"], 1) == false
    assert TictactoeElixir.spot_free?(["0", "X", "O"], 2) == false
  end

  test "Swaps player" do
    assert TictactoeElixir.swap_player("X") == "O"
    assert TictactoeElixir.swap_player("O") == "X"
  end
end
