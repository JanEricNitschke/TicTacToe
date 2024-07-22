class TEST_AI_PLAYER

insert
    EIFFELTEST_TOOLS

create {}
    make

feature {ANY}
    make
        -- the root creation procedure
        local
            ai_player: AI_PLAYER
            board: BOARD
            row1, row2, row3, col1, col2, col3, diag1, diag2: ARRAY [INTEGER]
            win_conditions: ARRAY [ARRAY [INTEGER]]
        do
            row1 := << {INTEGER_32 1}, {INTEGER_32 2}, {INTEGER_32 3} >>
            row2 := << {INTEGER_32 4}, {INTEGER_32 5}, {INTEGER_32 6} >>
            row3 := << {INTEGER_32 7}, {INTEGER_32 8}, {INTEGER_32 9} >>
            col1 := << {INTEGER_32 1}, {INTEGER_32 4}, {INTEGER_32 7} >>
            col2 := << {INTEGER_32 2}, {INTEGER_32 5}, {INTEGER_32 8} >>
            col3 := << {INTEGER_32 3}, {INTEGER_32 6}, {INTEGER_32 9} >>
            diag1 := << {INTEGER_32 1}, {INTEGER_32 5}, {INTEGER_32 9} >>
            diag2 := << {INTEGER_32 3}, {INTEGER_32 5}, {INTEGER_32 7} >>
            win_conditions := << row1, row2, row3, col1, col2, col3, diag1, diag2 >>

            create board.make
            board.add_marker(1, 'X')

            create ai_player.make('O', 5, win_conditions)

            ai_player.take_turn(board)
            label_assert("Move made correctly", board.cell(5) = 'O')
        end
end -- class TEST_AI_PLAYER
