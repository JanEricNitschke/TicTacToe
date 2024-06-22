readline = require 'readline'
rl = readline.createInterface
  input: process.stdin
  output: process.stdout

win_conditions = [
  [0, 1, 2]
  [3, 4, 5]
  [6, 7, 8]
  [0, 3, 6]
  [1, 4, 7]
  [2, 5, 8]
  [0, 4, 8]
  [2, 4, 6]
]

board = ["0", "1", "2", "3", "4", "5", "6", "7", "8"]
player = "X"

swap_player = (player) -> if player is "X" then "O" else "X"

blockReadLine = () ->
  new Promise (resolve) ->
    rl.once 'line', (line) -> resolve(line)

player_turn = (board, player) ->
  while true
    console.log "Player #{player}, enter your move (0-8): "
    show_board board
    input = parseInt await blockReadLine()
    if isNaN input
      console.log "Enter a number."
      continue
    if not (0 <= input <= 8)
      console.log "Enter a number in bounds (0-8)."
      continue
    if board[input] != input.toString()
      console.log "Spot is already taken."
      continue
    board[input] = player
    break

game_won = (board, player) ->
  for condition in win_conditions
    [a, b, c] = condition
    if board[a] == player and board[b] == player and board[c] == player
      return true
  false

board_full = (board) ->
  for cell in board
    if cell != "X" and cell != "O"
      return false
  true

show_board = (board) ->
  console.log "#{board[0]} | #{board[1]} | #{board[2]}"
  console.log "---------"
  console.log "#{board[3]} | #{board[4]} | #{board[5]}"
  console.log "---------"
  console.log "#{board[6]} | #{board[7]} | #{board[8]}"

sleep = (ms) ->
  new Promise (resolve) ->
    setTimeout(resolve, ms)


get_empty_cells = (board) ->
  (i for cell, i in board when cell == i.toString())

ai_random_move = (board) ->
  empty_cells = get_empty_cells board
  empty_cells[Math.floor(Math.random() * empty_cells.length)]

try_win_move = (board, player) ->
  for condition in win_conditions
    done = 0
    open = []
    for spot in condition
      if board[spot] == spot.toString()
        open.push spot
      else if board[spot] == player
        done += 1
    if done is 2 and open.length is 1
      return open[0]
  null

ai_win_move = (board, player) ->
  spot = try_win_move board, player
  if spot isnt null then spot else ai_random_move board

ai_win_block_move = (board, player) ->
  spot = try_win_move board, player
  if spot isnt null
    return spot
  spot = try_win_move board, swap_player player
  if spot isnt null then spot else ai_random_move board


minmax = (board, player) ->
  best_move =
    score: -1
    spot: null

  if game_won board, player
    best_move.score = 1
    return best_move

  if game_won board, swap_player player
    best_move.score = -1
    return best_move

  empty_cells = get_empty_cells board

  if empty_cells.length is 0
    best_move.score = 0
    return best_move

  if empty_cells.length is board.length
    best_move.spot = ai_random_move board
    return best_move

  for spot in empty_cells
    board[spot] = player
    score = -minmax(board, swap_player(player)).score
    board[spot] = spot.toString()
    if score >= best_move.score
      best_move.score = score
      best_move.spot = spot
  best_move


ai_best_move = (board, player) ->
  minmax(board, player).spot


ai_turn = (board, player, strength) ->
  console.log "AI turn as player #{player} with strength #{strength}"
  show_board board
  move = switch strength
    when 1 then ai_random_move board
    when 2 then ai_win_move board, player
    when 3 then ai_win_block_move board, player
    else ai_best_move board, player
  board[move] = player
  await sleep 1000

play = (X_strength, O_strength) ->
  console.log "X: #{X_strength}, O: #{O_strength}"
  while true
    if player is "X" and X_strength > 0
      await ai_turn board, player, X_strength
    else if player is "O" and O_strength > 0
      await ai_turn board, player, O_strength
    else
      await player_turn board, player
    if game_won board, player
      console.log "Player #{player} wins the game!"
      break
    if board_full board
      console.log "Game Drawn!"
      break
    player = swap_player player

  show_board board
  rl.close()

X_strength = 0
O_strength = 0
if process.argv.length > 2
  X_strength = parseInt process.argv[2]
if process.argv.length > 3
  O_strength = parseInt process.argv[3]
play(X_strength, O_strength)
