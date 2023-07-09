# frozen_string_literal: true

require "test_helper"
##
# This class tests tictactoe_ruby.
class TestTictactoeRuby < Minitest::Test
  include TictactoeRuby
  def test_that_it_has_a_version_number
    refute_nil VERSION
  end
end

##
# This class tests TicTacToe.
class TestTicTacToe < Minitest::Test
  include TictactoeRuby
  def test_it_does_something_useful_tictactoe
    refute_nil TicTacToe.new
  end
end

##
# This class tests Move.
class TestMove < Minitest::Test
  include TictactoeRuby
  def test_it_does_something_useful_move
    refute_nil Move.new
  end
end

##
# This class tests Board.
class TestBoard < Minitest::Test
  include TictactoeRuby
  def test_it_does_something_useful_board
    refute_nil Board.new
  end

  def test_create_board
    board = Board.new
    board.create_board
    assert_equal([["-", "-", "-"], ["-", "-", "-"], ["-", "-", "-"]], board.board)
  end

  def test_empty_cells
    board = Board.new([%i[X X X], ["-", :X, :X], ["-", :X, "-"]])
    assert_equal([[1, 0], [2, 0], [2, 2]], board.empty_cells)
  end

  def test_board_filled
    board = Board.new([%i[X X X], ["-", :X, :X], ["-", :X, "-"]])
    refute board.board_filled?
    board = Board.new([%i[X X X], %i[O X X], %i[O X O]])
    assert board.board_filled?
  end

  def test_player_win_rows
    board = Board.new([%i[X X X], ["-", "-", "-"], ["-", "-", "-"]])
    assert board.player_win?(:X)
    refute board.player_win?(:O)
    board = Board.new([["-", "-", "-"], %i[X X X], ["-", "-", "-"]])
    assert board.player_win?(:X)
    refute board.player_win?(:O)
    board = Board.new([["-", "-", "-"], ["-", "-", "-"], %i[O O O]])
    assert board.player_win?(:O)
    refute board.player_win?(:X)
  end

  def test_player_win_cols
    board = Board.new([["-", :X, "-"], ["-", :X, "-"], ["-", :X, "-"]])
    assert board.player_win?(:X)
    refute board.player_win?(:O)
    board = Board.new([[:X, "-", "-"], [:X, "-", "-"], [:X, "-", "-"]])
    assert board.player_win?(:X)
    refute board.player_win?(:O)
    board = Board.new([["-", "-", :O], ["-", "-", :O], ["-", "-", :O]])
    assert board.player_win?(:O)
    refute board.player_win?(:X)
  end

  def test_player_win_diags
    board = Board.new([[:X, "-", "-"], ["-", :X, "-"], ["-", "-", :X]])
    assert board.player_win?(:X)
    refute board.player_win?(:O)
    board = Board.new([["-", "-", :O], ["-", :O, "-"], [:O, "-", "-"]])
    assert board.player_win?(:O)
    refute board.player_win?(:X)
  end

  def test_fix_spot
    board = Board.new
    board.create_board
    assert board.fix_spot?(1, 1, :X)
    assert_equal([[:X, "-", "-"], ["-", "-", "-"], ["-", "-", "-"]], board.board)
    refute board.fix_spot?(1, 1, :X)
    refute board.fix_spot?(-1, 1, :O)
    refute board.fix_spot?(0, nil, :O)
    refute board.fix_spot?(0, 16, :O)
  end

  def test_clear_spot
    board = Board.new([[:X, "-", "-"], ["-", "-", "-"], ["-", "-", "-"]])
    assert board.clear_spot(0, 0)
    assert_equal([["-", "-", "-"], ["-", "-", "-"], ["-", "-", "-"]], board.board)
    assert board.clear_spot(0, 0)
    refute board.fix_spot?(-1, 1, :O)
    refute board.fix_spot?(0, nil, :O)
    refute board.fix_spot?(0, 16, :O)
  end

  def test_check_wincondition
    board = Board.new([[:X, "-", "-"], ["-", "-", "-"], ["-", :X, "-"]])
    done_spots, open_spots = board.check_win_condition([[0, 0], [0, 1], [2, 1]], :X)
    assert_equal(2, done_spots)
    assert_equal([[0, 1]], open_spots)
  end
end

##
# This class tests Player.
class TestPlayerGeneral < Minitest::Test
  include TictactoeRuby
  def test_it_does_something_useful_player
    refute_nil Player.new
  end

  def test_player_move_raises
    assert_raises RuntimeError do
      Player.new.make_move
    end
  end

  def test_swap_player
    assert_equal(:X,  Player.swap_player(:O))
    assert_equal(:O,  Player.swap_player(:X))
  end

  def test_it_does_something_useful_humanplayer
    refute_nil HumanPlayer.new
  end

  def test_it_does_something_useful_aiplayer
    refute_nil AIPlayer.new
  end
end

##
# This class tests random ai move.
class TestAIPlayerMoveRandom < Minitest::Test
  include TictactoeRuby
  def test_ai_move_random
    ai_player = AIPlayer.new(1, :X)
    board = Board.new([[:X, "-", "-"], ["-", "-", "-"], ["-", :X, "-"]])
    open_cells = board.empty_cells
    100.times do
      ai_move = ai_player.ai_move(board, :X)
      assert_includes(open_cells, [ai_move.row, ai_move.col])
    end
  end
end

##
# This class tests winning ai move.
class TestAIPlayerMoveWinning < Minitest::Test
  include TictactoeRuby

  def test_ai_move_winning_diag
    # Main diagonal
    ai_player = AIPlayer.new(2, :X)
    board = Board.new([[:X, "-", "-"], ["-", :X, "-"], ["-", "-", "-"]])
    ai_move = ai_player.ai_move(board, :X)
    assert_equal([2, 2], [ai_move.row, ai_move.col])
    # Off-diagonal
    ai_player = AIPlayer.new(2, :O)
    board = Board.new([["-", "-", "-"], ["-", :O, "-"], [:O, "-", "-"]])
    ai_move = ai_player.ai_move(board, :O)
    assert_equal([0, 2], [ai_move.row, ai_move.col])
  end

  def test_ai_move_winning_row_col
    # Row
    ai_player = AIPlayer.new(2, :O)
    board = Board.new([["-", "-", "-"], ["-", "-", "-"], [:O, "-", :O]])
    ai_move = ai_player.ai_move(board, :O)
    assert_equal([2, 1], [ai_move.row, ai_move.col])
    # Col
    ai_player = AIPlayer.new(2, :X)
    board = Board.new([["-", :X, "-"], ["-", :X, "-"], ["-", "-", "-"]])
    ai_move = ai_player.ai_move(board, :X)
    assert_equal([2, 1], [ai_move.row, ai_move.col])
  end

  def test_ai_move_winning_random
    ai_player = AIPlayer.new(2, :X)
    board = Board.new([[:X, "-", "-"], ["-", "-", "-"], ["-", :X, "-"]])
    open_cells = board.empty_cells
    100.times do
      ai_move = ai_player.ai_move(board, :X)
      assert_includes(open_cells, [ai_move.row, ai_move.col])
    end
  end
end

##
# This class tests blocking/winning ai move.
class TestAIPlayerMoveBlockingWinning < Minitest::Test
  include TictactoeRuby
  def test_ai_move_blocking_winning_diag
    # Main diagonal
    ai_player = AIPlayer.new(3, :X)
    board = Board.new([[:X, "-", "-"], ["-", :X, "-"], ["-", "-", "-"]])
    ai_move = ai_player.ai_move(board, :X)
    assert_equal([2, 2], [ai_move.row, ai_move.col])
    # Off-diagonal
    ai_player = AIPlayer.new(3, :O)
    board = Board.new([["-", "-", "-"], ["-", :O, "-"], [:O, "-", "-"]])
    ai_move = ai_player.ai_move(board, :O)
    assert_equal([0, 2], [ai_move.row, ai_move.col])
  end

  def test_ai_move_blocking_winning_row
    # Win if possible
    ai_player = AIPlayer.new(3, :O)
    board = Board.new([[:X, :X, "-"], ["-", "-", "-"], [:O, "-", :O]])
    ai_move = ai_player.ai_move(board, :O)
    assert_equal([2, 1], [ai_move.row, ai_move.col])
    # Block if needed
    ai_player = AIPlayer.new(3, :O)
    board = Board.new([[:X, :X, "-"], ["-", "-", "-"], [:O, "-", "-"]])
    ai_move = ai_player.ai_move(board, :O)
    assert_equal([0, 2], [ai_move.row, ai_move.col])
  end

  def test_ai_move_blocking_winning_col
    # Win if possible
    ai_player = AIPlayer.new(3, :X)
    board = Board.new([["-", :X, :O], ["-", :X, :O], ["-", "-", "-"]])
    ai_move = ai_player.ai_move(board, :X)
    assert_equal([2, 1], [ai_move.row, ai_move.col])
    # Block if needed
    ai_player = AIPlayer.new(3, :X)
    board = Board.new([["-", :X, :O], ["-", "-", :O], ["-", "-", "-"]])
    ai_move = ai_player.ai_move(board, :X)
    assert_equal([2, 2], [ai_move.row, ai_move.col])
  end

  def test_ai_move_blocking_winning_random
    ai_player = AIPlayer.new(3, :X)
    board = Board.new([[:X, "-", "-"], ["-", "-", "-"], ["-", :X, "-"]])
    open_cells = board.empty_cells
    100.times do
      ai_move = ai_player.ai_move(board, :X)
      assert_includes(open_cells, [ai_move.row, ai_move.col])
    end
  end
end

##
# This class tests minmax ai move.
class TestAIPlayerMoveMinMax < Minitest::Test
  include TictactoeRuby

  def test_ai_move_minmax_base_cases
    ai_player = AIPlayer.new(4, :X)
    board = Board.new([%i[X X X], %i[X X X], %i[X X X]])
    assert_equal(Move.new(-1, -1, 1), ai_player.ai_move(board, :X))
    assert_equal(Move.new(-1, -1, -1), ai_player.ai_move(board, :O))
    board = Board.new([%i[X O X], %i[X O O], %i[O X X]])
    assert_equal(Move.new(-1, -1, 0), ai_player.ai_move(board, :O))
  end

  def test_ai_move_minmax_0_levels
    ai_player = AIPlayer.new(4, :X)
    board = Board.new([[:X, :X, "-"], %i[O X O], %i[X O O]])
    assert_equal(Move.new(0, 2, 1), ai_player.ai_move(board, :X))
    assert_equal(Move.new(0, 2, 1), ai_player.ai_move(board, :O))
  end

  def test_ai_move_minmax_1_levels
    ai_player = AIPlayer.new(4, :X)
    board = Board.new([%i[O O X], [:X, "-", :O], ["-", :O, :X]])
    assert_equal(Move.new(1, 1, 1), ai_player.ai_move(board, :O))
    assert_equal(Move.new(1, 1, 0), ai_player.ai_move(board, :X))
  end

  def test_ai_move_minmax_deep
    ai_player = AIPlayer.new(4, :X)
    board = Board.new([[:O, "-", "-"], ["-", "-", "-"], ["-", "-", "-"]])
    assert_equal(Move.new(1, 1, 0), ai_player.ai_move(board, :X))
  end
end
