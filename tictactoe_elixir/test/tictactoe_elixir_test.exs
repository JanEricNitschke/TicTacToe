defmodule TictactoeElixirTest do
  use ExUnit.Case
  doctest TictactoeElixir

  test "greets the world" do
    assert TictactoeElixir.hello() == :world
  end
end
