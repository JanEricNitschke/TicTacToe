# frozen_string_literal: true

require_relative "tictactoe_ruby/version"

module TictactoeRuby
  class Error < StandardError; end

  ##
  # This class store and handles actions on a board of tictactoe.
  class Board
    def initialize
      @board = []
      @win_conditions = [[[0, 0], [0, 1], [0, 2]], [[1, 0], [1, 1], [1, 2]], [[2, 0], [2, 1], [2, 2]],
                         [[0, 0], [1, 0], [2, 0]], [[0, 1], [1, 1], [2, 1]], [[0, 2], [1, 2], [2, 2]],
                         [[0, 0], [1, 1], [2, 2]], [[0, 2], [1, 1], [2, 0]]]
    end

    def create_board
      @board = []
      3.times do
        row = []
        3.times do
          row.append("-")
        end
        @board.append(row)
      end
    end

    def show_board
      line_separater = "---------------"
      puts line_separater
      @board.each do |row|
        row.each { |cell| print "| #{cell} |" }
        puts "\n#{line_separater}"
      end
    end

    def board_filled
      @board.each do |row|
        row.each { |cell| return false if cell == "-" }
      end
      true
    end

    def player_win(player)
      @win_conditions.each do |winning_line|
        won = true
        winning_line.each do |row, col|
          won = false unless @board[row][col] == player
        end
        return true if won
      end
      false
    end

    def in_bounds(row, col)
      unless row.between?(0, 2) && col.between?(0, 2)
        puts "Row #{row + 1} or column #{col + 1} are out of bounds." \
          " They have to be between 1 and 3 inclusive. Try again!"
        return false
      end
      true
    end

    def spot_open(row, col)
      unless @board[row][col] == "-"
        puts "The position (#{row + 1}, #{col + 1}) has already been taken by a player!"\
          " Please do your move on an empty position."
        return false
      end
      true
    end

    def fix_spot(row, col, player)
      if in_bounds(row, col) && spot_open(row, col)
        @board[row][col] = player
        return true
      end
      false
    end
  end

  ##
  # This class represents a game of tictactoe.
  class TicTacToe
    # Create the object
    def initialize
      @board = Board.new
    end

    def game_over(player)
      if @board.player_win(player)
        puts "Player #{player} wins the game!"
        return true
      end
      if @board.board_filled
        puts "Match Draw!"
        return true
      end
      false
    end

    def player_turn(player)
      loop do
        puts "Player #{player} turn"
        @board.show_board
        row, col = gets.split.map(&:to_i)
        if row.nil? || col.nil?
          puts "Invalid input"
          next
        end
        break if @board.fix_spot(row - 1, col - 1, player)
      end
    end

    def flush_stdout
      old_sync = $stdout.sync
      $stdout.sync = true
      old_sync
    end

    def start_game
      old_sync = flush_stdout
      @board.create_board
      player = :X
      loop do
        player_turn(player)
        break if game_over(player)

        player = swap_player(player)
      end
      @board.show_board
      $stdout.sync = old_sync
    end

    def swap_player(player)
      player == :X ? :O : :X
    end
  end
end
