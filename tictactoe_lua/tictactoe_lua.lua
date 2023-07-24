TicTacToe = {
    board = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
    win_conditions = { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 }, -- Rows
        { 1, 4, 7 }, { 2, 5, 8 }, { 3, 6, 9 },                -- Cols
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

function TicTacToe.get_empty_cells()
    local empty_cells = {}
    for index, value in ipairs(TicTacToe.board) do
        if TicTacToe.player_markers[value] == nil then
            table.insert(empty_cells, index)
        end
    end
    return empty_cells
end

function TicTacToe.random_move()
    local empties = TicTacToe.get_empty_cells()
    return { spot = empties[math.random(#empties)], end_state = -1 }
end

function TicTacToe.get_winning_move(current_player)
    for _, win_condition in pairs(TicTacToe.win_conditions) do
        local result = TicTacToe.check_win_condition(win_condition, current_player)
        if result.spots_done == 2 and #result.spots_open == 1 then
            return { spot = result.spots_open[1], -1 }
        end
    end
    return nil
end

function TicTacToe.win_move(current_player)
    local winning_move = TicTacToe.get_winning_move(current_player)
    if winning_move == nil then
        return TicTacToe.random_move()
    else
        return winning_move
    end
end

function TicTacToe.block_win_move(current_player)
    local winning_move = TicTacToe.get_winning_move(current_player)
    if winning_move ~= nil then
        return winning_move
    end
    local blocking_move = TicTacToe.get_winning_move(TicTacToe.swap_player(current_player))
    if blocking_move ~= nil then
        return blocking_move
    end
    return TicTacToe.random_move()
end

function TicTacToe.minmax(current_player)
    -- Game already won
    if TicTacToe.is_player_win(current_player) then
        return { spot = -1, end_state = 1 }
    end
    -- Game already lost
    if TicTacToe.is_player_win(TicTacToe.swap_player(current_player)) then
        return { spot = -1, end_state = -1 }
    end
    local empty_cells = TicTacToe.get_empty_cells()
    -- Game already drawn
    if #empty_cells == 0 then
        return { spot = -1, end_state = 0 }
    end
    -- New game, just random
    if #empty_cells == 9 then
        return TicTacToe.random_move()
    end
    -- Recursive cases
    local best_move = {spot = -1, end_state = -1}
    for _, current_spot in ipairs(empty_cells) do
        TicTacToe.board[current_spot] = current_player
        local current_move = TicTacToe.minmax(TicTacToe.swap_player(current_player))
        if -current_move.end_state >= best_move.end_state then
            best_move = {spot = current_spot, end_state = - current_move.end_state}
        end
        TicTacToe.board[current_spot] = tostring(current_spot)
    end
    return best_move
end

function Sleep (a)
    local sec = tonumber(os.clock() + a);
    while (os.clock() < sec) do
    end
end

function TicTacToe.ai_turn(current_player)
    print("Ai turn as " .. current_player .. ".")
    TicTacToe.show_board()
    local ai_move
    if TicTacToe.ai_strength == 1 then
        ai_move = TicTacToe.random_move()
    elseif TicTacToe.ai_strength == 2 then
        ai_move = TicTacToe.win_move(current_player)
    elseif TicTacToe.ai_strength == 3 then
        ai_move = TicTacToe.block_win_move(current_player)
    else
        ai_move = TicTacToe.minmax(current_player)
    end
    TicTacToe.board[ai_move.spot] = current_player
    Sleep(1)
end

function TicTacToe.get_yes_no(question)
    while true do
        print(question)
        local response = io.read(1)
        io.read("l")
        if response == "Y" or response == "y" then
            return true
        end
        if response == "N" or response == "n" then
            return false
        end
    end
end

function TicTacToe.get_ai_opponent()
    TicTacToe.ai_opponent = TicTacToe.get_yes_no("Play alone vs AI? [y/n]")
end

function TicTacToe.get_ai_start()
    if TicTacToe.get_yes_no("Should the AI make the first move? [y/n]") then
        TicTacToe.ai_marker = "X"
    else
        TicTacToe.ai_marker = "O"
    end
end

function TicTacToe.get_ai_strength()
    print("AI strength settings:")
    print("1: Easy")
    print("2: Medium")
    print("3: Hard")
    print("4: Impossible")
    while not (TicTacToe.ai_strength >= 1 and TicTacToe.ai_strength <= 4) do
        local response = io.read("n")
        if response ~= nil then
            TicTacToe.ai_strength = response
        else
            io.read("l")
            print("Bad choice!")
        end
    end
end

function TicTacToe.initialize_game()
    TicTacToe.get_ai_opponent()
    if TicTacToe.ai_opponent then
        TicTacToe.get_ai_start()
        TicTacToe.get_ai_strength()
    end
end

function TicTacToe.play()
    local player = "X"

    while true do
        if TicTacToe.ai_opponent and TicTacToe.ai_marker == player then
            TicTacToe.ai_turn(player)
        else
            TicTacToe.player_turn(player)
        end
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
