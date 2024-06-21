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

get_user_input = () ->
  console.log("Enter your move:")
  result = await blockReadLine()
  console.log("Got result #{result}")
  result

player_turn = (board, player) ->
  while true
    console.log "Player #{player}, enter your move (0-8): "
    show_board board
    input = parseInt await get_user_input()
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

play = () ->
  while true
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

play()
