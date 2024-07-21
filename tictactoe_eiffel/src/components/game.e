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
        do
            create temp_x_player.make('X')
            create temp_o_player.make('O')
            x_player := temp_x_player
            o_player := temp_o_player
            create board.make
            io.put_string("Game initialized.%N")
        end

feature {ANY} -- Game Logic

    play
            -- Play the game until the board is full.
        local
            x_turn: BOOLEAN
            done: BOOLEAN
        do
            x_turn := True
            io.put_string("Playing the game!%N")
            from
                done := False
            until
                done
            loop
                io.put_string("New turn!%N")
                if x_turn then
                    x_player.take_turn(board)
                else
                    o_player.take_turn(board)
                end
                -- Check if the board is full and stop the game if it is.
                if board.is_full then
                    done := True
                    io.put_string("The board is full. Game over!%N")
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


end -- class GAME
