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

  test "Finds optimal move at max depth" do
    assert TictactoeElixir.minmax(["X", "1", "2", "3", "4", "5", "6", "7", "8"], "O") ==
             %TictactoeElixir.Move{
               score: 0,
               index: 4
             }
  end
end
