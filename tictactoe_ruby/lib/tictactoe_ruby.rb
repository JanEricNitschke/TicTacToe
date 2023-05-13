# frozen_string_literal: true

require_relative "tictactoe_ruby/version"

##
# Module for playing TicTacToe
module TictactoeRuby
  class Error < StandardError; end

  ##
  # Abstract base class for Humans and AI players
  class Player
    def make_move
      raise "Only call this on a child class"
    end

    def self.swap_player(player)
      player == :X ? :O : :X
    end
  end

  ##
  # Concrete class for human players
  class HumanPlayer < Player
    def make_move(board, marker)
      loop do
        puts "Player #{marker} turn"
        board.show_board
        puts "Enter row and column numbers to fix spot: "
        row, col = gets.split.map(&:to_i)
        break if board.fix_spot(row, col, marker)
      end
    end
  end

  ##
  # Concrete class for AI players
  class AIPlayer < Player
    attr_reader :marker

    def initialize(ai_strength = 1, ai_marker = :X)
      super()
      @strength = ai_strength
      @marker = ai_marker
    end

    def make_move(board, marker)
      puts "AI turn as #{marker}"
      board.show_board
      best_move = random_move(board)
      # best_move = case @strength when 1
      #                              random_move(board, marker)
      #                            else
      #                              random_move(board, marker)
      #             end
      raise "AI did an oopsie" unless board.fix_spot(best_move[0] + 1, best_move[1] + 1, marker)

      sleep(1)
    end


    private

    def random_move(board)
      board.empty_cells.sample
    end
  end

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

    def empty_cells
      empty_cells = []
      @board.each_with_index do |row, row_index|
        row.each_with_index { |val, col_index| empty_cells.append([row_index, col_index]) if val == "-" }
      end
      empty_cells
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

    def input_nil(row, col)
      if row.nil? || col.nil?
        puts "Invalid input!"
        return true
      end
      false
    end

    def fix_spot(row, col, player)
      return false if input_nil(row, col)

      row -= 1
      col -= 1
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
      @human_player = HumanPlayer.new
      @ai_player = nil
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

    def flush_stdout
      old_sync = $stdout.sync
      $stdout.sync = true
      old_sync
    end

    def ask_player_yes_no(question)
      loop do
        puts question
        response = gets.chomp
        return true if %w[Y y].include? response
        return false if %w[N n].include? response

        puts "Bad choice"
      end
    end

    def ask_player_number
      ask_player_yes_no("Play alone vs AI?[y/n]: ")
    end

    def print_ai_options
      puts "AI strength settings:"
      puts "1: Easy"
      puts "2: Medium"
      puts "3: Hard"
      puts "4: Impossible"
    end

    def ask_ai_strength
      print_ai_options
      loop do
        puts "How strong should the AI be? [1,2,3,4]"
        response = gets.chomp.to_i
        return response unless response.nil? || !([1, 2, 3, 4].include? response)

        puts "Bad choice"
      end
    end

    def ask_ai_configuration
      return unless ask_player_number

      @ai_player = AIPlayer.new(ask_ai_strength, ask_ai_start ? :X : :O)
    end

    def ask_ai_start
      ask_player_yes_no("Should the AI make the first move?[y/n]: ")
    end

    def setup_game
      @board.create_board
      ask_ai_configuration
      :X
    end

    def take_move(player)
      if @ai_player.nil? || player != @ai_player.marker
        @human_player.make_move(@board, player)
      else
        @ai_player.make_move(@board, player)
      end
    end

    def start_game
      old_sync = flush_stdout
      player = setup_game
      loop do
        take_move(player)
        break if game_over(player)

        player = Player.swap_player(player)
      end
      @board.show_board
      $stdout.sync = old_sync
    end
  end
end
