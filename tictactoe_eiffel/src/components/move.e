class
    MOVE

create {ANY}
    make

feature {ANY} -- Initialization

    make (a_spot: INTEGER; a_game_state: INTEGER)
            -- Create a player with the given marker.
        require
            valid_spot: a_spot >= 0 and then a_spot <= 9
            valid_game_state: a_game_state >= -1 and then a_game_state <= 1
        do
            spot := a_spot
            game_state := a_game_state
        ensure
            spot_set: spot = a_spot
            game_state_set: game_state = a_game_state
        end

feature {ANY} -- Access

    spot: INTEGER
            -- Spot the move is made on.
    game_state: INTEGER
            -- The state of the game after the move is made.

invariant
    valid_spot: spot >= 0 and then spot <= 9
    valid_game_state: game_state >= -1 and then game_state <= 1

end
