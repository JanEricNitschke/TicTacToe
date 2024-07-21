note
    description: "Root for playing a game of TicTacToe"
    author: "Jan-Eric Nitschke"

class
    TICTACTOE

insert
    ARGUMENTS

create {ANY}
    make

feature {ANY}

    make
            -- Start the game (and later parse command line arguments).
        local
            game: GAME
            x_strength, o_strength, i: INTEGER
            arg: STRING
        do
            x_strength := 0
            o_strength := 0
            from
                i := 1
            until
                i > argument_count
            loop
                arg := argument(i)
                if arg.has_prefix("--x-strength=") and arg.substring(14, arg.count).is_integer then
                        x_strength := arg.substring(14, arg.count).to_integer
                elseif arg.has_prefix("--o-strength=") and arg.substring(14, arg.count).is_integer then
                    o_strength := arg.substring(14, arg.count).to_integer
                end
                i := i + 1
            end
            create game.make(x_strength, o_strength)
            game.play
        end

end -- class TICTACTOE
