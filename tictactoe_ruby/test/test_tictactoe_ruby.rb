# frozen_string_literal: true

require "test_helper"

class TestTictactoeRuby < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil ::TictactoeRuby::VERSION
  end

  def test_it_does_something_useful
    refute_nil ::TictactoeRuby::TicTacToe.new
  end
end