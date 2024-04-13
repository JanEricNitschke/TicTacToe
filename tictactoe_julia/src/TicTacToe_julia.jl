module TicTacToe_julia

export TicTacToe

const WINNING_COMBINATIONS = [
    [1, 2, 3], [4, 5, 6], [7, 8, 9],
    [1, 4, 7], [2, 5, 8], [3, 6, 9],
    [1, 5, 9], [3, 5, 7]
]


mutable struct GameBoard <: AbstractArray{Char,1}
    board::Array{Char,1}
    function GameBoard()
        board = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
        new(board)
    end
end

mutable struct Move
    position::Int
    score::Int
end


Base.iterate(g::GameBoard, state=1) = state > length(g.board) ? nothing : (g.board[state], state + 1)
Base.length(g::GameBoard) = length(g.board)
Base.isdone(g::GameBoard, state) = state > length(g.board)

Base.getindex(g::GameBoard, i::Int) = g.board[i]
Base.size(g::GameBoard) = size(g.board)

Base.propertynames(::GameBoard, private::Bool=false) = private ? (:board,) : ()

Base.setindex!(g::GameBoard, v, i::Int) = g.board[i] = v
Base.firstindex(g::GameBoard) = 1
Base.lastindex(g::GameBoard) = length(g.board)

function Base.show(io::IO, g::GameBoard)
    println(io, " $(g[1]) | $(g[2]) | $(g[3])")
    println(io, "---|---|---")
    println(io, " $(g[4]) | $(g[5]) | $(g[6])")
    println(io, "---|---|---")
    println(io, " $(g[7]) | $(g[8]) | $(g[9])")
end

mutable struct TicTacToe
    board::GameBoard
    player::Char
    x_strength::Union{Nothing,Int}
    o_strength::Union{Nothing,Int}
    function TicTacToe(x::Union{Nothing,Int}, o::Union{Nothing,Int})
        new(GameBoard(), 'X', x, o)
    end
end

function player_turn!(t::TicTacToe)
    position = 0
    println("Player $(t.player), enter your move [1-9]: ")
    println(t.board)
    while true
        try
            position = parse(Int, readline())
        catch
            println("Invalid input! Try again.")
            continue
        end

        if position < 1 || position > 9
            println("Invalid position! Try again.")
            continue
        end

        if t.board[position] == 'X' || t.board[position] == 'O'
            println("Spot already taken! Try again.")
            continue
        end

        break
    end
    t.board[position] = t.player
end

function take_turn!(t::TicTacToe)
    if t.player == 'X' && !isnothing(t.x_strength) || t.player == 'O' && !isnothing(t.o_strength)
        ai_turn!(t)
    else
        player_turn!(t)
    end
end

function ai_turn!(t::TicTacToe)
    current_strength = t.player === 'X' ? t.x_strength : t.o_strength
    println("AI turn as player $(t.player) with strength $(current_strength).")
    println(t.board)
    if current_strength == 0
        random_move!(t.board, t.player)
    elseif current_strength == 1
        winning_move!(t.board, t.player)
    elseif current_strength == 2
        blocking_move!(t.board, t.player)
    else
        optimal_move!(t.board, t.player)
    end
    sleep(1)
end

function get_random_move(board::AbstractArray{Char,1})
    empty_positions = findall(x -> x != 'X' && x != 'O', board)
    return empty_positions[rand(1:end)]
end

function random_move!(board::AbstractArray{Char,1}, player::Char)
    board[get_random_move(board)] = player
end

function get_winning_spot(board::AbstractArray{Char,1}, player::Char)
    for combination in WINNING_COMBINATIONS
        if count(x -> board[x] == player, combination) == 2 && count(x -> board[x] != 'X' && board[x] != 'O', combination) == 1
            return combination[findfirst(x -> board[x] !== 'X' && board[x] !== 'O', combination)]
        end
    end
    return nothing
end

function winning_move!(board::AbstractArray{Char,1}, player::Char)
    winning_spot = get_winning_spot(board, player)
    if !isnothing(winning_spot)
        board[winning_spot] = player
        return
    end
    random_move!(board, player)
end

function blocking_move!(board::AbstractArray{Char,1}, player::Char)
    winning_spot = get_winning_spot(board, player)
    if !isnothing(winning_spot)
        board[winning_spot] = player
        return
    end
    blocking_spot = get_winning_spot(board, player == 'X' ? 'O' : 'X')
    if !isnothing(blocking_spot)
        board[blocking_spot] = player
        return
    end
    random_move!(board, player)
end

function optimal_move!(board::AbstractArray{Char,1}, player::Char)
    move = get_optimal_move(board, player)
    board[move.position] = player
end

digittochar(d) = Char('0' + d)

function get_optimal_move(board::AbstractArray{Char,1}, player::Char)
    best_move = Move(0, -1000)
    if check_win(board, player)
        best_move.score = 1
        return best_move
    end
    if check_win(board, player == 'X' ? 'O' : 'X')
        best_move.score = -1
        return best_move
    end
    empty_cells = findall(x -> x != 'X' && x != 'O', board)
    if isempty(empty_cells)
        best_move.score = 0
        return best_move
    end

    if length(empty_cells) == length(board)
        return Move(get_random_move(board), 0)
    end

    for cell in empty_cells
        board[cell] = player
        score = -get_optimal_move(board, player == 'X' ? 'O' : 'X').score
        board[cell] = digittochar(cell)
        if score > best_move.score
            best_move.position = cell
            best_move.score = score
        end
    end
    return best_move
end

function check_win(board::AbstractArray{Char,1}, player::Char)
    for combination in WINNING_COMBINATIONS
        if all(x -> board[x] == player, combination)
            return true
        end
    end
    return false
end

function check_tie(board::AbstractArray{Char,1})
    return all(x -> x == 'X' || x == 'O', board)
end

function swap_player!(t::TicTacToe)
    t.player = t.player == 'X' ? 'O' : 'X'
end

function Base.:+(t::TicTacToe)
    take_turn!(t)
end

function Base.iterate(t::TicTacToe, state=1)
    if check_win(t.board, t.player)
        println("Player $(t.player) wins!")
        return nothing
    end
    if check_tie(t.board)
        println("Match drawn!")
        return nothing
    end
    if state != 1
        swap_player!(t)
    end
    return state, state + 1
end

function play_game!(t::TicTacToe)
    for turn in t
        println("Turn: $turn")
        +t
    end
    println(t.board)
end

Base.:~(t::TicTacToe) = play_game!(t)
end # module TicTacToe_julia
