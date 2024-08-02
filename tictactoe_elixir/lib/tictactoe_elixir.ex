defmodule TictactoeElixir do
  @moduledoc """
  A simple Tic Tac Toe game in Elixir.
  """

  @board_size 3
  @winning_combinations [
    # Rows
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    # Columns
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    # Diagonals
    [0, 4, 8],
    [2, 4, 6]
  ]

  @doc """
  Check if the spot is unoccupied.

  ## Examples

      iex> TictactoeElixir.spot_free?(["0", "X", "O"], 0)
      true
      iex> TictactoeElixir.spot_free?(["0", "X", "O"], 1)
      false
      iex> TictactoeElixir.spot_free?(["0", "X", "O"], 2)
      false
  """
  def spot_free?(board, move) do
    Enum.at(board, move) not in ["X", "O"]
  end

  @doc """
  Make the actual move on the board.

  ## Examples

      iex> TictactoeElixir.make_move(["0", "X", "O"], 0, "Y")
      ["Y", "X", "O"]
  """
  def make_move(board, move, player) do
    List.replace_at(board, move, player)
  end

  @doc """
  Swap the player.

  ## Examples

      iex> TictactoeElixir.swap_player("X")
      "O"
      iex> TictactoeElixir.swap_player("O")
      "X"
  """
  def swap_player("X"), do: "O"
  def swap_player("O"), do: "X"

  def display_board(board) do
    IO.puts("--+---+--")

    for row <- Enum.chunk_every(board, @board_size) do
      IO.puts(Enum.join(row, " | "))
      IO.puts("--+---+--")
    end
  end

  @doc """
  Check if the player has won the game

  ## Examples

      iex> TictactoeElixir.winner?(["X", "X", "X", "3", "4", "5", "6", "7", "8"], "X")
      true
      iex> TictactoeElixir.winner?(["O", "1", "O", "3", "4", "5", "6", "7", "8"], "X")
      false
  """
  def winner?(board, player) do
    Enum.any?(@winning_combinations, fn [a, b, c] ->
      Enum.at(board, a) == player and Enum.at(board, b) == player and Enum.at(board, c) == player
    end)
  end

  @doc """
  Check if the board is full.

  ## Examples

      iex> TictactoeElixir.board_full?(["X", "X", "X", "X", "X", "X", "X", "X", "X"])
      true
      iex> TictactoeElixir.board_full?(["X", "X", "X", "X", "X", "X", "X", "X", "8"])
      false
  """
  def board_full?(board) do
    Enum.all?(board, &(&1 in ["X", "O"]))
  end

  def player_turn(board, player) do
    IO.puts("Player #{player}, enter your move (0-8): ")
    display_board(board)
    input = String.trim(IO.gets("> "))

    case Integer.parse(input) do
      :error ->
        IO.puts("Invalid input! Please enter a number.")
        player_turn(board, player)

      {move, _} when move < 0 or move > 8 ->
        IO.puts("Out of bounds! Please enter a number between 0 and 8.")
        player_turn(board, player)

      {move, _} ->
        if spot_free?(board, move) do
          make_move(board, move, player)
        else
          IO.puts("Spot taken! Please choose another spot.")
          player_turn(board, player)
        end
    end
  end

  def loop(board, player) do
    board = player_turn(board, player)

    cond do
      winner?(board, player) ->
        IO.puts("Player #{player} wins!")
        display_board(board)

      board_full?(board) ->
        IO.puts("It's a draw!")
        display_board(board)

      true ->
        loop(board, swap_player(player))
    end
  end

  def play do
    board = ["0", "1", "2", "3", "4", "5", "6", "7", "8"]
    player = "X"
    loop(board, player)
  end
end
