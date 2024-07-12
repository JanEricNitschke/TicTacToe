require "./spec_helper"
include TictactoeCrystalLib

describe TictactoeCrystalLib do
  it "Swap player swaps" do
    swap_player('X').should eq('O')
    swap_player('O').should eq('X')
  end

  it "Minmax works on wide board" do
    board = ['X', '1', '2', '3', '4', '5', '6', '7', '8']
    expected_move = Move.new(Score::DRAW, 4)
    minmax(board, 'O').should eq(expected_move)
  end
end
