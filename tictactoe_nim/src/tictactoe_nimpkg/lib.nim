from std/strutils import parseInt
from std/os import sleep
from std/random import sample
from std/options import Option, isSome, get, none, some

type
    Board = array[1..9, char]

type
    Score = enum
        LOSE
        DRAW
        WIN
    Move = object
        spot: range[1..9]
        score: Score

proc `-`(score: Score): Score =
    case score
    of Score.LOSE: Score.WIN
    of Score.WIN: Score.LOSE
    else: score

const
    WINNING_COMBINATIONS: array[1..8, array[1..3, int]] = [
        [1, 2, 3], [4, 5, 6], [7, 8, 9],                 # Rows
        [1, 4, 7], [2, 5, 8], [3, 6, 9],                 # Cols
        [1, 5, 9], [3, 5, 7]                             # Diagonals
    ]

proc show_board(board: Board) =
    echo $board[1], " | ", $board[2], " | ", $board[3]
    echo "---------"
    echo $board[4], " | ", $board[5], " | ", $board[6]
    echo "---------"
    echo $board[7], " | ", $board[8], " | ", $board[9]

proc is_winner(player: char, board: Board): bool =
    for i in WINNING_COMBINATIONS:
        if board[i[1]] == player and board[i[2]] == player and board[i[3]] == player:
            return true
    return false

proc is_full(board: Board): bool =
    for cell in board:
        if cell != 'X' and cell != 'O':
            return false
    return true

proc player_turn(player: char, board: var Board) =
    var move: int
    while true:
        echo "Player ", player, ", enter your move (1-9): "
        show_board(board)
        try:
            move = parseInt(readLine(stdin))
        except ValueError:
            echo "Invalid input. Please enter a number between 1 and 9!"
            continue
        if move < 1 or move > 9:
            echo "Input out of range. Please enter a number between 0 and 8!"
            continue
        if board[move] == 'X' or board[move] == 'O':
            echo "Cell already taken. Please choose another cell!"
            continue
        break
    board[move] = player

proc swap_player(player: char): char =
    if player == 'X': 'O' else: 'X'

proc get_empty_spots(board: Board): seq[int] =
    var empty_spots: seq[int] = @[]
    for i, cell in board:
        if cell != 'X' and cell != 'O':
            empty_spots.add(i)
    return empty_spots

proc random_spot(board: Board): int =
    return sample(get_empty_spots(board))

proc try_winning_spot(player: char, board: Board): Option[int] =
    for combination in WINNING_COMBINATIONS:
        var
            open_spot: Option[int] = none(int)
            taken_spots: int = 0
        for cell in combination:
            if board[cell] == player:
                taken_spots += 1
            elif board[cell] != 'X' and board[cell] != 'O':
                open_spot = some(cell)
        if taken_spots == 2 and isSome(open_spot):
            return open_spot
    return none(int)

proc winning_spot(player: char, board: Board): int =
    var spot = try_winning_spot(player, board)
    if isSome(spot):
        return spot.get
    return random_spot(board)

proc winning_or_blocking_spot(player: char, board: Board): int =
    var spot = try_winning_spot(player, board)
    if isSome(spot):
        return spot.get
    spot = try_winning_spot(swap_player(player), board)
    if isSome(spot):
        return spot.get
    return random_spot(board)

proc digit_to_char(d: range[0..9]): char = char(48 + d) # 48 = '0'

# This (and minmax_spot) actually always leaves the board in the same state
# as it was before the call. But it does temporarily change the board state
# trade off here is between not declaring and making a copy which is clearer
# but less efficient and the current approach which is more efficient but
# less clear.
proc get_optimal_spot(player: char, board: var Board): Move =
    var
        best_move = Move(spot: 1, score: Score.LOSE)
        empty_spots: seq[int] = @[]

    if is_winner(player, board):
        best_move.score = Score.WIN
        return best_move
    if is_winner(swap_player(player), board):
        best_move.score = Score.LOSE
        return best_move

    empty_spots = get_empty_spots(board)
    if empty_spots.len == 0:
        best_move.score = Score.DRAW
        return best_move
    if empty_spots.len == board.len:
        best_move.spot = sample(empty_spots)
        return best_move

    for spot in empty_spots:
        board[spot] = player
        var score = -get_optimal_spot(swap_player(player), board).score
        board[spot] = digit_to_char(spot)
        if score > best_move.score:
            best_move.spot = spot
            best_move.score = score
    return best_move

proc minmax_spot(player: char, board: var Board): int =
    get_optimal_spot(player, board).spot

proc ai_turn(player: char, board: var Board, strength: int) =
    echo "AI turn as player", player, " with strength ", strength, "."
    show_board(board)
    var best_spot = case strength
        of 1:
            random_spot(board)
        of 2:
            winning_spot(player, board)
        of 3:
            winning_or_blocking_spot(player, board)
        else:
            minmax_spot(player, board)
    board[best_spot] = player
    sleep 1000 # Sleeps for 1000ms = 1s

proc play_game*(X_strength, O_strength: int) =
    var
        board: Board = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
        player: char = 'X'
    while true:
        if player == 'X' and X_strength > 0:
            ai_turn(player, board, X_strength)
        elif player == 'O' and O_strength > 0:
            ai_turn(player, board, O_strength)
        else:
            player_turn(player, board)
        if is_winner(player, board):
            echo "Player ", player, " wins!"
            break
        elif is_full(board):
            echo "It's a draw!"
            break
        player = swap_player(player)
    show_board(board)
