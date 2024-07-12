require "./spec_helper"
include TictactoeCrystalLib

describe TictactoeCrystalLib do

  it "Swap player swaps" do
    swap_player('X').should eq('O')
    swap_player('O').should eq('X')
  end
end
