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
        do
            -- Find the first free position.
            a_board.show
            from
                position := 1
            until
                position > 9 or else a_board.cell(position).is_digit
            loop
                position := position + 1
            end

            -- Place the marker at the found position.
            a_board.add_marker(position, marker)
        end

    swapped_player: HUMAN_PLAYER
            -- Return an identical instance with the marker swapped.
        do
            create Result.make(swap_marker)
        end

end
