class
    HUMAN_PLAYER

inherit
    PLAYER

create {ANY}
    make

feature {ANY} -- Initialization

    make (a_marker: CHARACTER)
            -- Create a player with the given marker.
        require
            valid_marker: a_marker = 'X' or else a_marker = 'O'
        do
            marker := a_marker
        ensure
            marker_set: marker = a_marker
        end

feature {ANY} -- Element change

    take_turn (a_board: BOARD)
            -- Place the player's marker in the first free space on the board.
        local
            position: INTEGER
            input: STRING
            done: BOOLEAN
            possible: BOOLEAN
        do
            from
                done := False
            until
                done
            loop
                io.put_string("Player " + marker.out + "'s turn. Enter a number between 1 and 9: %N")
                a_board.show
                possible := True
                std_input.read_line
                input := std_input.last_string
                if not input.is_integer then
                    io.put_string("Please enter a number.%N")
                    possible := False
                else
                    position := input.to_integer
                end
                if possible and (position < 1 or position > 9) then
                    io.put_string("Please enter a number between 1 and 9.%N")
                    possible := False
                end
                if possible then
                    -- Needed because it does not short-circuit.
                    if not a_board.cell(position).is_digit then
                        io.put_string("Spot already taken. Please use another one.%N")
                        possible := False
                    end
                end
                if possible then
                    done := True
                end
            end
            a_board.add_marker(position, marker)
        end

    swapped_player: HUMAN_PLAYER
            -- Return an identical instance with the marker swapped.
        do
            create Result.make(swap_marker)
        end

end
