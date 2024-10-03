class
    AI_PLAYER

inherit
    PLAYER

create {ANY}
    make

feature {ANY} -- Initialization

    make (a_marker: CHARACTER; a_strength: INTEGER; some_win_conditions: ARRAY [ARRAY [INTEGER]])
            -- Create a player with the given marker.
        require
            valid_marker: a_marker = 'X' or else a_marker = 'O'
            valid_strength: a_strength > 0
        do
            marker := a_marker
            strength := a_strength
            win_conditions := some_win_conditions
        ensure
            marker_set: marker = a_marker
            strength_set: strength = a_strength
            win_conditions_set: win_conditions = some_win_conditions
        end

feature {} -- Access

    strength: INTEGER
            -- Player's marker, should be either 'X' or 'O'.

    win_conditions: ARRAY [ARRAY [INTEGER]]
            -- The win conditions for the game.

feature {ANY} -- Element change

    take_turn (a_board: BOARD)
            -- Place the player's marker in the first free space on the board.
        local
            position: INTEGER
            exts: EXTERNALS
            left: NATURAL_32
        do
            io.put_string("AI turns as player " + marker.out + " with strength " + strength.out + "%N")
            a_board.show
            -- Find the first free position.

            inspect strength
            when 1 then
                position := next_move(a_board)
            when 2 then
                position := random_move(a_board)
            when 3 then
                position := win_move(a_board)
            when 4 then
                position := win_block_move(a_board)
            else
                position := best_move(a_board)
            end
            -- Place the marker at the found position.
            a_board.add_marker(position, marker)
            create exts
            left := exts.sleep(1.to_natural_32)
        ensure then
            strength_unchanged: strength = old strength
        end

    swapped_player: AI_PLAYER
            -- Return an identical instance with the marker swapped.
        do
            create Result.make(swapped_marker, strength, win_conditions)
        ensure then
            marker_swapped: Result.marker /= marker and then (Result.marker = 'X' or else Result.marker = 'O')
        end

feature {AI_PLAYER} -- Implementation

    next_move(a_board: BOARD): INTEGER
        -- Make the move at the first open spot on the board.
        local
            position: INTEGER
        do
            from
                position := 1
            until
                position > 9 or else a_board.cell(position).is_digit
            loop
                position := position + 1
            end
            Result := position
        end

    random_integer (lower, upper: INTEGER): INTEGER
            -- Return a random integer between lower and upper bounds.
        require
            lower <= upper
        local
            random: MINIMAL_RANDOM_NUMBER_GENERATOR
        do
            create random.make
            random.next
            Result := (lower-1) + random.last_integer(upper - (lower-1))
        ensure
            in_range: lower <= Result and then Result <= upper
        end

    random_move(a_board: BOARD): INTEGER
        -- Make the move on a random open spot on the board.
        local
            open_spots: ARRAY [INTEGER]
        do
            open_spots := a_board.empty_cells
            Result := open_spots.item(random_integer(1, open_spots.count))
        end

    try_win_move(a_board: BOARD): INTEGER
        -- Try to find a winning move. Return -1 if None is available.
        local
            done_spots, i, j: INTEGER
            open_spots: ARRAY [INTEGER]
            condition: ARRAY [INTEGER]
        do
            Result := 0
            create open_spots.with_capacity(3, 1)
            from
                i := win_conditions.lower
            until
                i > win_conditions.upper or else Result > 0
            loop
                condition := win_conditions.item(i)
                done_spots := 0
                open_spots.clear_count
                from
                    j := condition.lower
                until
                    j > condition.upper
                loop
                    if a_board.cell(condition.item(j)) = marker then
                        done_spots := done_spots + 1
                    elseif a_board.cell(condition.item(j)).is_digit then
                        open_spots.add_last(condition.item(j))
                    end
                    j := j + 1
                end
                if done_spots = 2 and open_spots.count = 1 then
                    Result := open_spots.item(1)
                end
                i := i + 1
            end
        end

    win_move(a_board: BOARD): INTEGER
        -- Perform a winning or random move.
        local
            position: INTEGER
        do
            position := try_win_move(a_board)
            if position = 0 then
                Result := random_move(a_board)
            else
                Result := position
            end
        end

    win_block_move(a_board: BOARD): INTEGER
        -- Perform a winning, blocking, or random move.
        local
            position: INTEGER
        do
            position := try_win_move(a_board)
            if position = 0 then
                position := swapped_player.try_win_move(a_board)
            end
            if position = 0 then
                position := random_move(a_board)
            end
            Result := position
        end

    minmax(a_board: BOARD): MOVE
        -- Find the best move possible by using the Minimax algorithm.
        local
            move: MOVE
            score: INTEGER
            open_spots: ARRAY [INTEGER]
            i: INTEGER
        do
            open_spots := a_board.empty_cells
            if a_board.game_won(Current, win_conditions) then
                create move.make(1, 1)
            elseif a_board.game_won(swapped_player, win_conditions) then
                create move.make(1, -1)
            elseif open_spots.is_empty then
                create move.make(1, 0)
            elseif a_board.is_empty then
                create move.make(random_integer(1, 9), 0)
            else
                create move.make(0, -1)
                from
                    i := open_spots.lower
                until
                    i > open_spots.upper
                loop
                    a_board.add_marker(open_spots.item(i), marker)
                    Current.swap_marker
                    score := -minmax(a_board).game_state
                    a_board.remove_marker(open_spots.item(i))
                    Current.swap_marker
                    if score >= move.game_state then
                        move.make(open_spots.item(i), score)
                    end
                    i := i + 1
                end
            end
            Result := move
        end

    best_move(a_board: BOARD): INTEGER
        -- Perform the best move possible.
        local
            move: MOVE
        do
            move := minmax(a_board)
            Result := move.spot
        end

end
