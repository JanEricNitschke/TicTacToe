# frozen_string_literal: true

require_relative "tictactoe_ruby/version"

##
# Module for playing TicTacToe
module TictactoeRuby
  class Error < StandardError; end

  ##
  # Class to hold position and outcome information about a move
  class Move
    attr_accessor :row, :col, :end_state

    def initialize(row = -1, col = -1, end_state = -1)
      @row = row
      @col = col
      @end_state = end_state
    end

    def ==(other)
      self.class == other.class &&
        @row == other.row &&
        @col == other.col &&
        @end_state == other.end_state
    end
  end

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
        puts "Enter row (1-3) and column (1-3) numbers to fix spot: "
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
      move = ai_move(board, marker)
      raise "AI did an oopsie" unless board.fix_spot(move.row + 1, move.col + 1, marker)

      sleep(1)
    end

    def ai_move(board, marker)
      case @strength
      when 1 then random_move(board)
      when 2 then winning_move(board, marker)
      when 3 then winning_blocking_move(board, marker)
      else minmax(board, marker)
      end
    end

    private

    def random_move(board)
      row, col = board.empty_cells.sample
      Move.new(row, col)
    end

    def winning_move(board, player)
      win_move = find_winning_move(board, player)
      return win_move unless win_move.nil?

      random_move(board)
    end

    def winning_blocking_move(board, player)
      win_move = find_winning_move(board, player)
      return win_move unless win_move.nil?

      block_move = find_winning_move(board, Player.swap_player(player))
      return block_move unless block_move.nil?

      random_move(board)
    end

    def find_winning_move(board, player)
      board.win_conditions.each do |win_condition|
        done_spots, open_spots = board.check_win_condition(win_condition, player)
        return Move.new(open_spots[0][0], open_spots[0][1], 1) if done_spots == 2 && open_spots.length == 1
      end
      nil
    end

    def minmax(board, player)
      minmax_base_cases(board, player) || minmax_recursive_case(board, player)
    end

    def minmax_recursive_case(board, player)
      best_move = Move.new(-1, -1, -1)
      board.empty_cells.each do |row, col|
        board.fix_spot(row + 1, col + 1, player)
        current_move = minmax(board, Player.swap_player(player))
        best_move = Move.new(row, col, -current_move.end_state) if -current_move.end_state >= best_move.end_state
        board.clear_spot(row, col)
      end
      best_move
    end

    def minmax_base_cases(board, player)
      if board.player_win(player) then return Move.new(-1, -1, 1)
      elsif board.player_win(Player.swap_player(player)) then return Move.new(-1, -1, -1) end

      empty_cells = board.empty_cells
      if empty_cells.empty? then return Move.new(-1, -1, 0)
      elsif empty_cells.length == 9 then return random_move(board) end

      nil
    end
  end

  ##
  # This class store and handles actions on a board of tictactoe.
  class Board
    attr_accessor :win_conditions
    attr_reader :board

    def initialize(board = [])
      @board = board
      @win_conditions = [[[0, 0], [0, 1], [0, 2]], [[1, 0], [1, 1], [1, 2]], [[2, 0], [2, 1], [2, 2]],
                         [[0, 0], [1, 0], [2, 0]], [[0, 1], [1, 1], [2, 1]], [[0, 2], [1, 2], [2, 2]],
                         [[0, 0], [1, 1], [2, 2]], [[0, 2], [1, 1], [2, 0]]]
    end

    def create_board
      @board = []
      3.times do
        row = []
        3.times { row.append("-") }
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

    def fix_spot(row, col, player)
      return false if input_nil(row, col)

      if in_bounds(row - 1, col - 1) && spot_open(row - 1, col - 1)
        @board[row - 1][col - 1] = player
        return true
      end
      false
    end

    def clear_spot(row, col)
      if !input_nil(row, col) && in_bounds(row, col)
        @board[row][col] = "-"
        return true
      end
      false
    end

    def check_win_condition(win_condition, player)
      done_spots = 0
      open_spots = []
      win_condition.each do |row, col|
        if @board[row][col] == player
          done_spots += 1
        elsif @board[row][col] == "-"
          open_spots.append([row, col])
        end
      end
      [done_spots, open_spots]
    end

    private

    def in_bounds(row, col)
      unless (row.is_a? Integer) && (col.is_a? Integer) &&
             row.between?(0, @board.length - 1) && col.between?(0, @board[0].length - 1)
        puts "Row #{row + 1} or column #{col + 1} are out of bounds. " \
             "They have to be between 1 and 3 inclusive. Try again!"
        return false
      end
      true
    end

    def spot_open(row, col)
      unless @board[row][col] == "-"
        puts "The position (#{row + 1}, #{col + 1}) has already been taken by a player! "\
             "Please do your move on an empty position."
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
