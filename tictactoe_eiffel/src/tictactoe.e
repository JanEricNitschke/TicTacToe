note
    description: "Root for playing a game of TicTacToe"
    author: "Jan-Eric Nitschke"

class
    TICTACTOE

create {ANY}
    make

feature {ANY}

    make
            -- Start the game (and later parse command line arguments).
        local
            game: GAME
        do
            create game.make
            game.play
        end

end -- class TICTACTOE
