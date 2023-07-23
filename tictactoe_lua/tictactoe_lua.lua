TicTacToe = {
    board = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
    win_conditions = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 }, -- Rows
        { 1, 4, 7 }, { 2, 5, 8 }, { 3, 5, 9 },                -- Cols
        { 1, 5, 9 }, { 3, 5, 7 }                              -- Diags
    },
    player_markers = { X = true, O = true },
    ai_opponent = false,
    ai_marker = "X",
    ai_strength = 0,
}

function TicTacToe.swap_player(current_player)
    if current_player == "X" then
        return "O"
    end
    return "X"
end

function TicTacToe.show_board()
    local line_separator = "---------------"
    local side_length = 3
    print(line_separator)
    for row = 1, side_length do
        for col = 1, side_length do
            io.write("| " .. TicTacToe.board[(row - 1) * side_length + col] .. " |")
        end
        print ""
        print(line_separator)
    end
end

function TicTacToe.is_board_filled()
    for _, value in ipairs(TicTacToe.board) do
        if TicTacToe.player_markers[value] == nil then
            return false
        end
    end
    return true
end

function TicTacToe.check_win_condition(win_condition, current_player)
    local result = { spots_open = {}, spots_done = 0 }
    for _, spot in ipairs(win_condition) do
        if TicTacToe.board[spot] == current_player then
            result.spots_done = result.spots_done + 1
            -- If it is not the current player and it is not the other one
            -- Then it is open
        elseif TicTacToe.board[spot] ~= TicTacToe.swap_player(current_player) then
            table.insert(result.spots_open, spot)
        end
    end
    return result
end

function TicTacToe.is_player_win(current_player)
    for _, win_condition in pairs(TicTacToe.win_conditions) do
        local result = TicTacToe.check_win_condition(win_condition, current_player)
        if result.spots_done == 3 then
            return true
        end
    end
    return false
end

function TicTacToe.fix_spot(spot, current_player)
    if TicTacToe.board[spot] == nil then
        print("ERROR: Spot has to be in range [1-9]!")
        return false
    end
    if TicTacToe.player_markers[TicTacToe.board[spot]] ~= nil then
        print("ERROR: Spot " .. spot .. " is already occupied!")
        return false
    end
    TicTacToe.board[spot] = current_player
    return true
end

function TicTacToe.player_turn(current_player)
    while true do
        print("Player " .. current_player .. " turn.")
        TicTacToe.show_board()
        local result = io.read("n")
        if result == nil then
            print("ERROR: Input must be a valid integer!")
            io.read("l")
            goto continue
        end
        if TicTacToe.fix_spot(result, current_player) then
            break
        end
        ::continue::
    end
end

function TicTacToe.play()
    local player = "X"

    while true do
        TicTacToe.player_turn(player)
        if TicTacToe.is_player_win(player) then
            print("Player " .. player .. " wins the game!")
            break
        end
        if TicTacToe.is_board_filled() then
            print("Draw!")
            break
        end
        player = TicTacToe.swap_player(player)
    end
    TicTacToe.show_board()
end
