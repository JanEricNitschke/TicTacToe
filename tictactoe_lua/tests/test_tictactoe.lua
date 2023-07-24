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
    TicTacToe.board = { "X", "X", "O", "O", "X", "O", "X", "O", "O" }
    Lu.assertEquals(TicTacToe.check_win_condition({ 3, 6, 9 }, "O"), { spots_open = {}, spots_done = 3 })
end

function TestGeneral:Test_isplayer_win_row()
    TicTacToe.board = { "X", "X", "X", "4", "5", "6", "7", "8", "X" }
    Lu.assertFalse(TicTacToe.is_player_win("O"))
    Lu.assertTrue(TicTacToe.is_player_win("X"))
    TicTacToe.board = { "X", "X", "O", "O", "X", "O", "X", "O", "O" }
    Lu.assertTrue(TicTacToe.is_player_win("O"))
end

function TestGeneral:Test_isplayer_win_col()
    TicTacToe.board = { "1", "O", "X", "4", "O", "6", "7", "O", "X" }
    Lu.assertFalse(TicTacToe.is_player_win("X"))
    Lu.assertTrue(TicTacToe.is_player_win("O"))
end

function TestGeneral:Test_isplayer_win_diag()
    TicTacToe.board = { "1", "2", "X", "4", "X", "6", "X", "O", "O" }
    Lu.assertFalse(TicTacToe.is_player_win("O"))
    Lu.assertTrue(TicTacToe.is_player_win("X"))
end

function TestGeneral:test_fix_spot()
    TicTacToe.board = { "1", "2", "X", "4", "X", "6", "X", "O", "O" }
    Lu.assertTrue(TicTacToe.fix_spot(1, "X"))
    -- Now occupied
    Lu.assertFalse(TicTacToe.fix_spot(1, "X"))
    --  Too small
    Lu.assertFalse(TicTacToe.fix_spot(-3, "O"))
    --  Too big
    Lu.assertFalse(TicTacToe.fix_spot(13, "O"))
end

TestAI = {}

function TestAI:test_random_move()
    TicTacToe.board = { "1", "2", "X", "4", "X", "6", "X", "O", "O" }
    local valid_spots = { [1] = true, [2] = true, [4] = true, [6] = true }
    for _ = 1, 100 do
        local result = TicTacToe.random_move()
        Lu.assertTrue(valid_spots[result.spot] ~= nil)
    end
end

function TestAI:test_win_move()
    TicTacToe.board = { "X", "2", "O", "4", "X", "6", "7", "8", "9" }
    for _ = 1, 100 do
        Lu.assertEquals(TicTacToe.get_winning_move("X").spot, 9)
        Lu.assertEquals(TicTacToe.win_move("X").spot, 9)
    end
end

function TestAI:test_block_win_move()
    TicTacToe.board = { "X", "2", "O", "4", "X", "6", "7", "8", "9" }
    for _ = 1, 100 do
        local win_move = TicTacToe.block_win_move("X")
        Lu.assertEquals(win_move.spot, 9)
        local block_move = TicTacToe.block_win_move("O")
        Lu.assertEquals(block_move.spot, 9)
    end
end

function TestAI:test_minmax_base_cases()
    TicTacToe.board = { "X", "X", "X", "X", "X", "X", "X", "X", "X" }
    Lu.assertEquals(TicTacToe.minmax("X"), { spot = -1, end_state = 1 })
    Lu.assertEquals(TicTacToe.minmax("O"), { spot = -1, end_state = -1 })
    TicTacToe.board = { "X", "O", "X", "X", "O", "O", "O", "X", "X" }
    Lu.assertEquals(TicTacToe.minmax("X"), { spot = -1, end_state = 0 })
    Lu.assertEquals(TicTacToe.minmax("O"), { spot = -1, end_state = 0 })
end

function TestAI:test_minmax_takes_open_spot()
    TicTacToe.board = { "X", "X", "3", "O", "X", "O", "X", "O", "O" }
    Lu.assertEquals(TicTacToe.minmax("X"), { spot = 3, end_state = 1 })
    TicTacToe.board = { "X", "X", "3", "O", "X", "O", "X", "O", "O" }
    Lu.assertEquals(TicTacToe.minmax("O"), { spot = 3, end_state = 1 })
end

function TestAI:test_minmax_blocks_win()
    TicTacToe.board = { "O", "O", "X", "X", "5", "O", "6", "O", "X" }
    Lu.assertEquals(TicTacToe.minmax("X"), { spot = 5, end_state = 0 })
end

function TestAI:test_minmax_takes_win()
    TicTacToe.board = { "O", "O", "X", "X", "-", "-", "-", "O", "X" }
    Lu.assertEquals(TicTacToe.minmax("O"), { spot = 5, end_state = 1 })
end

function TestAI:test_minmax_best()
    -- Correctly block middle
    -- Anything else can lead to a loss
    TicTacToe.board = { "X", "-", "-", "-", "-", "-", "-", "-", "-" }
    Lu.assertEquals(TicTacToe.minmax("O"), { spot = 5, end_state = 0 })
end

os.exit(Lu.LuaUnit.run())
