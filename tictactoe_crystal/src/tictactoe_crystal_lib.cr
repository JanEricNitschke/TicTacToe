module TictactoeCrystalLib
  WINNING_COMBINATIONS = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6],
  ]

  enum Score
    WIN  =  10
    DRAW =   0
    LOSE = -10

    def - : Score
      case self
      when WIN  then LOSE
      when LOSE then WIN
      else           self
      end
    end

    def <=>(other : Score)
      self.to_i <=> other.to_i
    end
  end

  struct Move
    property score, spot

    def initialize(@score : Score, @spot : Int32)
    end
  end

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

  def empty_cells(board : Array(Char)) : Array(Int32)
    board.each_with_index.select { |cell, i| cell == int_to_char(i) }.map { |_, i| i }.to_a
  end

  def ai_random_move(board : Array(Char)) : Int32
    empty_cells(board).sample
  end

  def try_win_move(board : Array(Char), player : Char) : Int32 | Nil
    WINNING_COMBINATIONS.each do |combo|
      done = 0
      open = [] of Int32
      combo.each do |cell|
        if board[cell] == player
          done += 1
        elsif board[cell] == int_to_char(cell)
          open << cell
        end
      end
      if done == 2 && open.size == 1
        return open[0]
      end
    end
    nil
  end

  def ai_win_move(board : Array(Char), player : Char) : Int32
    move = try_win_move(board, player)
    move.nil? ? ai_random_move(board) : move
  end

  def ai_win_block_move(board : Array(Char), player : Char) : Int32
    move = try_win_move(board, player)
    if move.nil?
      move = try_win_move(board, swap_player(player))
    end
    move.nil? ? ai_random_move(board) : move
  end

  def minmax(board : Array(Char), player : Char) : Move
    if check_winner(board, player)
      return Move.new(Score::WIN, 0)
    end
    if check_winner(board, swap_player(player))
      return Move.new(Score::LOSE, 0)
    end
    empty_cells = empty_cells(board)

    if empty_cells.empty?
      return Move.new(Score::DRAW, 0)
    end

    if empty_cells.size == board.size
      return Move.new(Score::DRAW, empty_cells.sample)
    end

    best_move = Move.new(Score::LOSE, 0)
    empty_cells.each do |cell|
      board[cell] = player
      score = -minmax(board, swap_player(player)).score
      board[cell] = int_to_char(cell)
      if score >= best_move.score
        best_move = Move.new(score, cell)
      end
    end
    best_move
  end

  def ai_best_move(board : Array(Char), player : Char) : Int32
    minmax(board, player).spot
  end

  def ai_turn(board : Array(Char), player : Char, strength : Int32)
    puts "AI turn as player #{player} with strength #{strength}."
    show_board(board)
    move = case strength
           when 1 then ai_random_move(board)
           when 2 then ai_win_move(board, player)
           when 3 then ai_win_block_move(board, player)
           else        ai_best_move(board, player)
           end
    board[move] = player
    sleep 1.second
  end

  def play(x_strength : Int32 | Nil, o_strength : Int32 | Nil)
    board = ['0', '1', '2', '3', '4', '5', '6', '7', '8']
    player = 'X'
    while true
      if player == 'X' && !x_strength.nil?
        ai_turn(board, player, x_strength)
      elsif player == 'O' && !o_strength.nil?
        ai_turn(board, player, o_strength)
      else
        player_turn(board, player)
      end
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
