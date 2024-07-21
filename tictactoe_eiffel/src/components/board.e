class
    BOARD

create {ANY}
    make

feature {ANY} -- Initialization

    make
            -- Initialize the board with indices as characters.
        do
            cells := << '1', '2', '3', '4', '5', '6', '7', '8', '9' >>
        ensure
            initialized: cells.count = 9
        end

feature {ANY} -- Access

    cell (index: INTEGER): CHARACTER
            -- Return the character at the given index.
        require
            valid_index: index >= 1 and index <= 9
        do
            Result := cells.item(index)
        ensure
            result_valid: Result = 'X' or else Result = 'O' or else (Result.out.is_integer and then Result.out.to_integer = index)
        end

    markers_count (a_marker: CHARACTER): INTEGER
            -- Return the count of the given marker on the board.
        require
            valid_marker: a_marker = 'X' or else a_marker = 'O'
        do
            Result := cells.occurrences(a_marker)
        ensure
            count_in_range: Result >= 0 and Result <= 9
        end

feature {ANY} -- Element change

    add_marker (index: INTEGER; a_marker: CHARACTER)
            -- Add the given marker to the board at the specified index.
        require
            valid_index: index >= 1 and index <= 9
            valid_marker: a_marker = 'X' or else a_marker = 'O'
            cell_available: cells.item(index).is_digit
        do
            cells.put(a_marker, index)
        ensure
            marker_added: cells.item(index) = a_marker
        end

feature {ANY} -- Status report

    is_full: BOOLEAN
            -- Is the board full?
        do
            Result := cells.occurrences('X') + cells.occurrences('O') = 9
        end

    game_won (a_player: PLAYER; win_conditions: ARRAY [ARRAY [INTEGER]]): BOOLEAN
            -- Has the given player won the game?
        require
            a_player_not_void: a_player /= Void
            conditions_within_bounds:
                win_conditions.for_all (
                    agent (wc: ARRAY [INTEGER_32]): BOOLEAN
                    do
                        Result := wc.for_all (
                            agent (x: INTEGER_32): BOOLEAN
                            do
                                Result := x >= 1 and x <= 9
                            end (?)
                        )
                    end (?)
                )

        local
            condition: ARRAY [INTEGER_32]
            i, j: INTEGER
            won: BOOLEAN
        do
            from
                i := win_conditions.lower
            until
                i > win_conditions.upper or else Result
            loop
                condition := win_conditions.item(i)
                won := True
                from
                    j := condition.lower
                until
                    j > condition.upper or else not won
                loop
                    if cells.item(condition.item(j)) /= a_player.marker then
                        won := False
                    end
                    j := j + 1
                end
                if won then
                    Result := True
                end
                i := i + 1
            end
        end


    show
            -- Pretty print the board with separators.
        local
            line_1: STRING
            line_2: STRING
            line_3: STRING
            sep_line: STRING
        do
            line_1 := cells.item(1).out + " | " + cells.item(2).out + " | " + cells.item(3).out + "%N"
            line_2 := cells.item(4).out + " | " + cells.item(5).out + " | " + cells.item(6).out + "%N"
            line_3 := cells.item(7).out + " | " + cells.item(8).out + " | " + cells.item(9).out + "%N"
            sep_line := "--------- %N"

            io.put_string(line_1)
            io.put_string(sep_line)
            io.put_string(line_2)
            io.put_string(sep_line)
            io.put_string(line_3)
        end

feature {} -- Implementation

    cells: ARRAY [CHARACTER]
            -- Array to store board cells.

end
