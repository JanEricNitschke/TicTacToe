# TODO: Write documentation for `TictactoeCrystal`
#
require "option_parser"
require "./tictactoe_crystal_lib"
include TictactoeCrystalLib

module TictactoeCrystal
  VERSION = "0.1.0"

  x_strength = nil
  o_strength = nil
  OptionParser.parse do |parser|
    parser.banner = "Welcome to The Beatles App!"

    parser.on "-x X", "--X-strength=X", "Strength of AI playing as X" do |x|
      x_strength = x.to_i
    end
    parser.on "-o=O", "--O-strength=O", "Strength of AI playing as O" do |o|
      o_strength = o.to_i
    end
    parser.missing_option do |option_flag|
      STDERR.puts "ERROR: #{option_flag} is missing something."
      STDERR.puts ""
      STDERR.puts parser
      exit(1)
    end
    parser.invalid_option do |option_flag|
      STDERR.puts "ERROR: #{option_flag} is not a valid option."
      STDERR.puts parser
      exit(1)
    end
  end
  puts "Hello World"
  puts typeof(x_strength), x_strength
  puts typeof(o_strength), o_strength
  puts add(1, 2)
end
