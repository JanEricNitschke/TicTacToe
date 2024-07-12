module TictactoeCrystalLib
  WINNING_COMBINATIONS = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6],
  ]

  def show_board(board : Array(Char))
    puts " #{board[0]} | #{board[1]} | #{board[2]}"
    puts "---+---+---"
    puts " #{board[3]} | #{board[4]} | #{board[5]}"
    puts "---+---+---"
    puts " #{board[6]} | #{board[7]} | #{board[8]}"
  end

  def board_full(board : Array(Char)) : Bool
    board.all? { |cell| cell == 'X' || cell == 'O' }
  end

  def check_winner(board : Array(Char), player : Char) : Bool
    WINNING_COMBINATIONS.any? do |combo|
      combo.all? { |cell| board[cell] == player }
    end
  end

  def int_to_char(i : Int32) : Char
    ('0'.ord + i).chr
  end

  def player_turn(board : Array(Char), player : Char)
    while true
      puts "Player #{player}'s turn. Enter a number between 0 and 8"
      show_board(board)
      input = gets
      if input.nil?
        puts "Enter a number."
        next
      end
      move = input.to_i?
      if move.nil?
        puts "Enter a number."
        next
      end
      if move < 0 || move > 8
        puts "Enter a number in bounds (0-8)."
        next
      end
      puts move.chr, typeof(move.chr)
      puts board[move], typeof(board[move])
      if board[move] != int_to_char(move)
        puts "Spot is already taken."
        next
      else
        board[move] = player
        break
      end
    end
  end

  def swap_player(player : Char) : Char
    player == 'X' ? 'O' : 'X'
  end

  def play(x_strength : Int32 | Nil, o_strength : Int32 | Nil)
    board = ['0', '1', '2', '3', '4', '5', '6', '7', '8']
    player = 'X'
    while true
      player_turn(board, player)
      if check_winner(board, player)
        puts "Player #{player} wins!"
        break
      end
      if board_full(board)
        puts "Game Drawn!"
        break
      end
      player = swap_player(player)
    end
    show_board(board)
  end
end
