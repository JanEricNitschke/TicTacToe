class
    GAME

create {ANY}
    make

feature {ANY} -- Initialization

    make
            -- Initialize the game with a board and two players.
        local
            temp_x_player: HUMAN_PLAYER
            temp_o_player: HUMAN_PLAYER
            row1, row2, row3, col1, col2, col3, diag1, diag2: ARRAY [INTEGER]
        do
            create temp_x_player.make('X')
            create temp_o_player.make('O')
            x_player := temp_x_player
            o_player := temp_o_player

            create board.make

            row1 := << {INTEGER_32 1}, {INTEGER_32 2}, {INTEGER_32 3} >>
            row2 := << {INTEGER_32 4}, {INTEGER_32 5}, {INTEGER_32 6} >>
            row3 := << {INTEGER_32 7}, {INTEGER_32 8}, {INTEGER_32 9} >>
            col1 := << {INTEGER_32 1}, {INTEGER_32 4}, {INTEGER_32 7} >>
            col2 := << {INTEGER_32 2}, {INTEGER_32 5}, {INTEGER_32 8} >>
            col3 := << {INTEGER_32 3}, {INTEGER_32 6}, {INTEGER_32 9} >>
            diag1 := << {INTEGER_32 1}, {INTEGER_32 5}, {INTEGER_32 9} >>
            diag2 := << {INTEGER_32 3}, {INTEGER_32 5}, {INTEGER_32 7} >>

            win_conditions := << row1, row2, row3, col1, col2, col3, diag1, diag2 >>
            io.put_string("Game initialized.%N")
        end

feature {ANY} -- Game Logic

    play
            -- Play the game until the board is full.
        local
            x_turn: BOOLEAN
            done: BOOLEAN
            current_player: PLAYER
        do
            x_turn := True
            io.put_string("Playing the game!%N")
            from
                done := False
            until
                done
            loop
                if x_turn then
                    current_player := x_player
                else
                    current_player := o_player
                end
                current_player.take_turn(board)
                if board.game_won(current_player, win_conditions) then
                    done := True
                    io.put_string("Player " + current_player.marker.out + " wins!%N")
                elseif board.is_full then
                    done := True
                    io.put_string("Game Drawn!%N")
                else
                    x_turn := not x_turn
                end
            end
            board.show
        end

feature {} -- Implementation

    board: BOARD
            -- The game board.

    x_player, o_player: PLAYER
            -- The players in the game.

    win_conditions: ARRAY [ARRAY [INTEGER]]


end -- class GAME
