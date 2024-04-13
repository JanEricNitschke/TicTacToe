using TicTacToe_julia: TicTacToe

function parse_arguments(args::Vector{String})
    X = nothing
    O = nothing
    for i in 1:2:length(args)-1
        if args[i] == "-X"
            X = tryparse(Int, args[i+1])
        elseif args[i] == "-O"
            O = tryparse(Int, args[i+1])
        end
    end
    return X, O
end

~(TicTacToe(parse_arguments(ARGS)...))
