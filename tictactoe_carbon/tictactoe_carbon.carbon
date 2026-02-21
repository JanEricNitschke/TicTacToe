// TicTacToe Game in Carbon

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
  Core.PrintChar('E');
  Core.PrintChar('n');
  Core.PrintChar('t');
  Core.PrintChar('e');
  Core.PrintChar('r');
  Core.PrintChar(' ');
  Core.PrintChar('m');
  Core.PrintChar('o');
  Core.PrintChar('v');
  Core.PrintChar('e');
  Core.PrintChar(' ');
  Core.PrintChar('(');
  Core.PrintChar('0');
  Core.PrintChar('-');
  Core.PrintChar('8');
  Core.PrintChar(')');
  Core.PrintChar(':');
  Core.PrintChar(' ');
}

fn PrintInvalidInput() {
  Core.PrintChar('I');
  Core.PrintChar('n');
  Core.PrintChar('v');
  Core.PrintChar('a');
  Core.PrintChar('l');
  Core.PrintChar('i');
  Core.PrintChar('d');
  Core.PrintChar(' ');
  Core.PrintChar('i');
  Core.PrintChar('n');
  Core.PrintChar('p');
  Core.PrintChar('u');
  Core.PrintChar('t');
  Core.PrintChar('.');
  Core.PrintChar('\n');
}

fn PrintInvalidMove() {
  Core.PrintChar('I');
  Core.PrintChar('n');
  Core.PrintChar('v');
  Core.PrintChar('a');
  Core.PrintChar('l');
  Core.PrintChar('i');
  Core.PrintChar('d');
  Core.PrintChar(' ');
  Core.PrintChar('m');
  Core.PrintChar('o');
  Core.PrintChar('v');
  Core.PrintChar('e');
  Core.PrintChar('.');
  Core.PrintChar('\n');
}

fn PrintCellOccupied() {
  Core.PrintChar('C');
  Core.PrintChar('e');
  Core.PrintChar('l');
  Core.PrintChar('l');
  Core.PrintChar(' ');
  Core.PrintChar('o');
  Core.PrintChar('c');
  Core.PrintChar('c');
  Core.PrintChar('u');
  Core.PrintChar('p');
  Core.PrintChar('i');
  Core.PrintChar('e');
  Core.PrintChar('d');
  Core.PrintChar('.');
  Core.PrintChar('\n');
}

fn PrintPlayerXWins() {
  Core.PrintChar('P');
  Core.PrintChar('l');
  Core.PrintChar('a');
  Core.PrintChar('y');
  Core.PrintChar('e');
  Core.PrintChar('r');
  Core.PrintChar(' ');
  Core.PrintChar('X');
  Core.PrintChar(' ');
  Core.PrintChar('w');
  Core.PrintChar('i');
  Core.PrintChar('n');
  Core.PrintChar('s');
  Core.PrintChar('!');
  Core.PrintChar('\n');
}

fn PrintPlayerOWins() {
  Core.PrintChar('P');
  Core.PrintChar('l');
  Core.PrintChar('a');
  Core.PrintChar('y');
  Core.PrintChar('e');
  Core.PrintChar('r');
  Core.PrintChar(' ');
  Core.PrintChar('O');
  Core.PrintChar(' ');
  Core.PrintChar('w');
  Core.PrintChar('i');
  Core.PrintChar('n');
  Core.PrintChar('s');
  Core.PrintChar('!');
  Core.PrintChar('\n');
}

fn PrintDraw() {
  Core.PrintChar('I');
  Core.PrintChar('t');
  Core.PrintChar('\'');
  Core.PrintChar('s');
  Core.PrintChar(' ');
  Core.PrintChar('a');
  Core.PrintChar(' ');
  Core.PrintChar('d');
  Core.PrintChar('r');
  Core.PrintChar('a');
  Core.PrintChar('w');
  Core.PrintChar('!');
  Core.PrintChar('\n');
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
        Core.PrintChar('-');
      } else if (cell == 1) {
        Core.PrintChar('X');
      } else {
        Core.PrintChar('O');
      }
      if (i % 3 == 2) {
        Core.PrintChar('\n');
      } else {
        Core.PrintChar(' ');
      }
      i = i + 1;
    }
  }

  fn CheckWinner[self: Self]() -> i32 {
    var i: i32 = 0;
    while (i < 8) {
      if (self.board[self.wins[i][0]] != 0
          and self.board[self.wins[i][0]] == self.board[self.wins[i][1]]
          and self.board[self.wins[i][1]] == self.board[self.wins[i][2]]) {
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

  fn PlayGame[ref self: Self]() {
    while (true) {
      var move: i32 = self.InputMove();
      self.board[move] = self.current_player;

      var winner: i32 = self.CheckWinner();
      if (winner != 0) {
        self.PrintBoard();
        if (winner == 1) {
          PrintPlayerXWins();
        } else {
          PrintPlayerOWins();
        }
        break;
      }
      if (self.IsFull()) {
        self.PrintBoard();
        PrintDraw();
        break;
      }

      self.current_player = SwapPlayer(self.current_player);
    }
  }

  var board: array(i32, 9);
  var current_player: i32;
  var wins: array(array(i32, 3), 8);
}

fn Run() -> i32 {
  var game: TicTacToe = TicTacToe.Make();
  game.PlayGame();
  return 0;
}
