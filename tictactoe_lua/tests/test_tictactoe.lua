package.path = package.path .. ";../?.lua"
require("tictactoe_lua")

Lu = require('luaunit')

TestGeneral = {}

function TestGeneral:setUp()
    TicTacToe.board = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
end

function TestGeneral:test_swap_player()
    Lu.assertEquals(TicTacToe.swap_player("X"), "O")
    Lu.assertEquals(TicTacToe.swap_player("O"), "X")
end

function TestGeneral:test_is_board_filled()
    Lu.assertFalse(TicTacToe.is_board_filled())
    TicTacToe.board = { "X", "X", "X", "O", "O", "O", "O", "X", "X" }
    Lu.assertTrue(TicTacToe.is_board_filled())
end

function TestGeneral:test_check_win_condition()
    TicTacToe.board = { "X", "X", "3", "4", "5", "6", "7", "8", "X" }
    Lu.assertEquals(TicTacToe.check_win_condition({ 1, 2, 3 }, "X"), { spots_open = { 3 }, spots_done = 2 })
    Lu.assertEquals(TicTacToe.check_win_condition({ 1, 2, 9 }, "X"), { spots_open = {}, spots_done = 3 })
    Lu.assertEquals(TicTacToe.check_win_condition({ 1, 2, 9 }, "O"), { spots_open = {}, spots_done = 0 })
end

function TestGeneral:Test_isplayer_win_row()
    TicTacToe.board = {"X", "X", "X", "4", "5", "6", "7", "8", "X"}
    Lu.assertFalse(TicTacToe.is_player_win("O"))
    Lu.assertTrue(TicTacToe.is_player_win("X"))
end

function TestGeneral:Test_isplayer_win_col()
    TicTacToe.board = {"1", "O", "X", "4", "O", "6", "7", "O", "X"}
    Lu.assertFalse(TicTacToe.is_player_win("X"))
    Lu.assertTrue(TicTacToe.is_player_win("O"))
end

function TestGeneral:Test_isplayer_win_diag()
    TicTacToe.board = {"1", "2", "X", "4", "X", "6", "X", "O", "O"}
    Lu.assertFalse(TicTacToe.is_player_win("O"))
    Lu.assertTrue(TicTacToe.is_player_win("X"))
end

function TestGeneral:test_fix_spot()
    TicTacToe.board = {"1", "2", "X", "4", "X", "6", "X", "O", "O"}
    Lu.assertTrue(TicTacToe.fix_spot(1, "X"))
    -- Now occupied
    Lu.assertFalse(TicTacToe.fix_spot(1, "X"))
    --  Too small
    Lu.assertFalse(TicTacToe.fix_spot(-3, "O"))
    --  Too big
    Lu.assertFalse(TicTacToe.fix_spot(13, "O"))
end



os.exit(Lu.LuaUnit.run())
