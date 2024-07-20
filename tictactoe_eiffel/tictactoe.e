note
    description: "Root for trivial system printing a message"
    author: "Jan-Eric Nitschke"

class
    TICTACTOE

create {ANY}
    make

feature {ANY}

    make
            -- Print a simple message.
        do
            io.put_string ("Hello World")
            io.put_new_line
        end

end -- class TICTACTOE
