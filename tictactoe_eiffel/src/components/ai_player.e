class
    AI_PLAYER

inherit
    PLAYER

create {ANY}
    make

feature {ANY} -- Initialization

    make (a_marker: CHARACTER, a_strength: INTEGER)
            -- Create a player with the given marker.
        require
            valid_marker: a_marker = 'X' or else a_marker = 'O'
            valid_strength: a_strength > 0 and then a_strength <= 4
        do
            marker := a_marker
            strength := a_strength
        ensure
            marker_set: marker = a_marker
            strength_set: strength = a_strength
        end

feature {ANY} -- Access

    strength: INTEGER
            -- Player's marker, should be either 'X' or 'O'.

feature {ANY} -- Element change

    take_turn (a_board: BOARD)
            -- Place the player's marker in the first free space on the board.
        local
            position: INTEGER
        do
            -- Find the first free position.
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

    swapped_player: AI_PLAYER
            -- Return an identical instance with the marker swapped.
        do
            create Result.make(swap_marker, strength)
        ensure
            marker_swapped: Result.marker /= marker and then (Result.marker = 'X' or else Result.marker = 'O')
            strength_same: Result.strength = strength
        end

end
