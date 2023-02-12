# frozen_string_literal: true

require_relative "tictactoe_ruby/version"

module TictactoeRuby
  class Error < StandardError; end

  ##
  # This class represents a game of tictactoe.
  class TicTacToe
    # Create the object
    def initialize
      @board = [["-", "-", "-"], ["-", "-", "-"], ["-", "-", "-"]]
    end

    def show_board
      line_separater = "---------------"
      puts line_separater
      @board.each do |row|
        row.each { |cell| print "| #{cell} |" }
        puts "\n#{line_separater}"
      end
    end
  end
end
