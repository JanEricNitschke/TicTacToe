using Test
using TicTacToe_julia: check_win, TicTacToe, get_optimal_move, Move

@testset "Test TicTacToe constructor" begin
    t = TicTacToe(0, 0)
    @test t.board.board == ['1', '2', '3', '4', '5', '6', '7', '8', '9']
    @test t.player == 'X'
end

@testset "Test player win row" begin
    @test check_win(['X', 'X', 'X', '4', '5', '6', '7', '8', '9'], 'X')
    @test !check_win(['X', 'X', 'X', '4', '5', '6', '7', '8', '9'], 'O')

    @test check_win(['1', '2', '3', 'O', 'O', 'O', '7', '8', '9'], 'O')
    @test !check_win(['1', '2', '3', 'O', 'O', 'O', '7', '8', '9'], 'X')

    @test check_win(['1', '2', '3', '4', '5', '6', 'X', 'X', 'X'], 'X')
    @test !check_win(['1', '2', '3', '4', '5', '6', 'X', 'X', 'X'], 'O')
end

@testset "Test minmax" begin
    optimal_move = get_optimal_move(['X', '2', '3', '4', '5', '6', '7', '8', '9'], 'O')
    @test optimal_move.position == 5
    @test optimal_move.score == 0
end
