// TicTacToe Game in Carbon

package tictactoe;

import Core library "io";

// IO functionality
// From: https://github.com/carbon-language/carbon-lang/blob/trunk/examples/advent2024/io_utils.carbon

// If non-zero, this is the most recently read character plus 2.
// The +2 is necessary to distinguish the case of no unread character
// from the case of unreading an EOF.
var unread_char: i32 = 0;

fn ReadChar() -> i32 {
  if (unread_char != 0) {
    var result: i32 = unread_char - 2;
    unread_char = 0;
    return result;
  }

  return Core.ReadChar();
}

fn UnreadChar(c: i32) {
  // TODO: assert(unread_char == 0);
  unread_char = c + 2;
}

fn ConsumeChar(c: i32) -> bool {
  var next: i32 = ReadChar();
  if (next != c) {
    UnreadChar(next);
    return false;
  }
  return true;
}

fn SkipNewline() -> bool {
  // Optional carriage return.
  ConsumeChar(13);
  // Newline.
  // TODO: Unread the CR if it was present?
  return ConsumeChar(10);
}

fn ReadInt(p: i32*) -> bool {
  var read_any_digits: bool = false;
  *p = 0;

  while (true) {
    var c: i32 = ReadChar();
    // '0' and '9'
    if (c < 48 or c > 57) {
      if ( c != 10 and c != 13) {
        SkipNewline();
      }
      break;
    }
    // TODO: Check for overflow.
    *p = *p * 10;
    // '0' is 48, so subtracting 48 gives us the digit.
    *p = *p + c - 48;
    read_any_digits = true;
  }
  return read_any_digits;
}

// String literal printing

fn PrintEnterMove() {
  // 'E'
  Core.PrintChar(69);
  // 'n'
  Core.PrintChar(110);
  // 't'
  Core.PrintChar(116);
  // 'e'
  Core.PrintChar(101);
  // 'r'
  Core.PrintChar(114);
  // ' '
  Core.PrintChar(32);
  // 'm'
  Core.PrintChar(109);
  // 'o'
  Core.PrintChar(111);
  // 'v'
  Core.PrintChar(118);
  // 'e'
  Core.PrintChar(101);
  // ' '
  Core.PrintChar(32);
  // '('
  Core.PrintChar(40);
  // '0'
  Core.PrintChar(48);
  // '-'
  Core.PrintChar(45);
  // '8'
  Core.PrintChar(56);
  // ')'
  Core.PrintChar(41);
  // ':'
  Core.PrintChar(58);
  // ' '
  Core.PrintChar(32);
}

fn PrintInvalidInput() {
  // 'I'
  Core.PrintChar(73);
  // 'n'
  Core.PrintChar(110);
  // 'v'
  Core.PrintChar(118);
  // 'a'
  Core.PrintChar(97);
  // 'l'
  Core.PrintChar(108);
  // 'i'
  Core.PrintChar(105);
  // 'd'
  Core.PrintChar(100);
  // ' '
  Core.PrintChar(32);
  // 'i'
  Core.PrintChar(105);
  // 'n'
  Core.PrintChar(110);
  // 'p'
  Core.PrintChar(112);
  // 'u'
  Core.PrintChar(117);
  // 't'
  Core.PrintChar(116);
  // '.'
  Core.PrintChar(46);
  // newline
  Core.PrintChar(10);
}

fn PrintInvalidMove() {
  // 'I'
  Core.PrintChar(73);
  // 'n'
  Core.PrintChar(110);
  // 'v'
  Core.PrintChar(118);
  // 'a'
  Core.PrintChar(97);
  // 'l'
  Core.PrintChar(108);
  // 'i'
  Core.PrintChar(105);
  // 'd'
  Core.PrintChar(100);
  // ' '
  Core.PrintChar(32);
  // 'm'
  Core.PrintChar(109);
  // 'o'
  Core.PrintChar(111);
  // 'v'
  Core.PrintChar(118);
  // 'e'
  Core.PrintChar(101);
  // '.'
  Core.PrintChar(46);
  // newline
  Core.PrintChar(10);
}

fn PrintCellOccupied() {
  // 'C'
  Core.PrintChar(67);
  // 'e'
  Core.PrintChar(101);
  // 'l'
  Core.PrintChar(108);
  // 'l'
  Core.PrintChar(108);
  // ' '
  Core.PrintChar(32);
  // 'o'
  Core.PrintChar(111);
  // 'c'
  Core.PrintChar(99);
  // 'c'
  Core.PrintChar(99);
  // 'u'
  Core.PrintChar(117);
  // 'p'
  Core.PrintChar(112);
  // 'i'
  Core.PrintChar(105);
  // 'e'
  Core.PrintChar(101);
  // 'd'
  Core.PrintChar(100);
  // '.'
  Core.PrintChar(46);
  // newline
  Core.PrintChar(10);
}

fn PrintPlayerXWins() {
  // 'P'
  Core.PrintChar(80);
  // 'l'
  Core.PrintChar(108);
  // 'a'
  Core.PrintChar(97);
  // 'y'
  Core.PrintChar(121);
  // 'e'
  Core.PrintChar(101);
  // 'r'
  Core.PrintChar(114);
  // ' '
  Core.PrintChar(32);
  // 'X'
  Core.PrintChar(88);
  // ' '
  Core.PrintChar(32);
  // 'w'
  Core.PrintChar(119);
  // 'i'
  Core.PrintChar(105);
  // 'n'
  Core.PrintChar(110);
  // 's'
  Core.PrintChar(115);
  // '!'
  Core.PrintChar(33);
  // newline
  Core.PrintChar(10);
}

fn PrintPlayerOWins() {
  // 'P'
  Core.PrintChar(80);
  // 'l'
  Core.PrintChar(108);
  // 'a'
  Core.PrintChar(97);
  // 'y'
  Core.PrintChar(121);
  // 'e'
  Core.PrintChar(101);
  // 'r'
  Core.PrintChar(114);
  // ' '
  Core.PrintChar(32);
  // 'O'
  Core.PrintChar(79);
  // ' '
  Core.PrintChar(32);
  // 'w'
  Core.PrintChar(119);
  // 'i'
  Core.PrintChar(105);
  // 'n'
  Core.PrintChar(110);
  // 's'
  Core.PrintChar(115);
  // '!'
  Core.PrintChar(33);
  // newline
  Core.PrintChar(10);
}

fn PrintDraw() {
  // 'I'
  Core.PrintChar(73);
  // 't'
  Core.PrintChar(116);
  // '\''
  Core.PrintChar(39);
  // 's'
  Core.PrintChar(115);
  // ' '
  Core.PrintChar(32);
  // 'a'
  Core.PrintChar(97);
  // ' '
  Core.PrintChar(32);
  // 'd'
  Core.PrintChar(100);
  // 'r'
  Core.PrintChar(114);
  // 'a'
  Core.PrintChar(97);
  // 'w'
  Core.PrintChar(119);
  // '!'
  Core.PrintChar(33);
  // newline
  Core.PrintChar(10);
}

// Game Logic

fn SwapPlayer(player: i32) -> i32 {
    if (player == 1) {
        return 2;
    } else {
        return 1;
    }
}

class TicTacToe {
  fn Make() -> TicTacToe {
    returned var me: TicTacToe;
    me.board = (0, 0, 0, 0, 0, 0, 0, 0, 0);
    me.current_player = 1;

    me.wins = (
      // rows
      (0, 1, 2), (3, 4, 5), (6, 7, 8),
      // cols
      (0, 3, 6), (1, 4, 7), (2, 5, 8),
      // diags
      (0, 4, 8), (2, 4, 6)
    );

    return var;
  }

  fn PrintBoard[self: Self]() {
    var i: i32 = 0;
    while (i < 9) {
      let cell: i32 = self.board[i];
      if (cell == 0) {
        // '-'
        Core.PrintChar(45);
      } else if (cell == 1) {
        // 'X'
        Core.PrintChar(88);
      } else {
        // 'O'
        Core.PrintChar(79);
      }
      if (i % 3 == 2) {
        // newline
        Core.PrintChar(10);
      } else {
        // space
        Core.PrintChar(32);
      }
      i = i + 1;
    }
  }

  fn CheckWinner[self: Self]() -> i32 {
    var i: i32 = 0;
    while (i < 8) {
      if (self.board[self.wins[i][0]] != 0 and self.board[self.wins[i][0]] == self.board[self.wins[i][1]] and self.board[self.wins[i][1]] == self.board[self.wins[i][2]]) {
        return self.board[self.wins[i][0]];
      }
      i = i + 1;
    }
    return 0;
  }

  fn IsFull[self: Self]() -> bool {
    var i: i32 = 0;
    while (i < 9) {
      if (self.board[i] == 0) {
        return false;
      }
      i = i + 1;
    }
    return true;
  }

  fn InputMove[self: Self]() -> i32 {
    var move: i32 = 0;
    while (true) {
      self.PrintBoard();
      PrintEnterMove();


      if (not ReadInt(&move)) {
        PrintInvalidInput();
        continue;
      }
      if (move < 0 or move > 8) {
        PrintInvalidMove();
        continue;
      }
      if (self.board[move] != 0) {
        PrintCellOccupied();
        continue;
      }
      break;
    }
    return move;
  }

  fn PlayGame[addr self: Self*]() {
    while (true) {
      var move: i32 = self->InputMove();
      self->board[move] = self->current_player;

      var winner: i32 = self->CheckWinner();
      if (winner != 0) {
        self->PrintBoard();
        if (winner == 1) {
          PrintPlayerXWins();
        } else {
          PrintPlayerOWins();
        }
        break;
      }
      if (self->IsFull()) {
        self->PrintBoard();
        PrintDraw();
        break;
      }

      self->current_player = SwapPlayer(self->current_player);
    }
  }


  var board: array(i32, 9);
  var current_player: i32;
  var wins: array(array(i32, 3), 8);
}

fn Run() {
  var game: TicTacToe = TicTacToe.Make();
  game.PlayGame();
}
