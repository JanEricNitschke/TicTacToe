deferred class
    PLAYER


feature {ANY} -- Access

    marker: CHARACTER
            -- Player's marker, should be either 'X' or 'O'.

feature {ANY} -- Element change

    take_turn (a_board: BOARD)
            -- Take a turn on the given board.
        require
            board_exists: a_board /= Void
            board_not_full: not a_board.is_full
        deferred
        ensure
            marker_count_increased: a_board.markers_count(marker) = old a_board.markers_count(marker) + 1
        end

    swapped_player: PLAYER
            -- Return an identical instance with the marker swapped.
        deferred
        ensure
            marker_swapped: Result.marker /= marker and then (Result.marker = 'X' or else Result.marker = 'O')
        end

feature {} -- Implementation

    swap_marker: CHARACTER
            -- Return the swapped marker.
        do
            if marker = 'X' then
                Result := 'O'
            else
                Result := 'X'
            end
        ensure
            valid_swapped: Result = 'X' or else Result = 'O'
            swapped: Result /= marker
        end

invariant
    valid_marker: marker = 'X' or else marker = 'O'

end
