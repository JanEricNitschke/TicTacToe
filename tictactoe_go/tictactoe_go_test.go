package main

import (
	"reflect"
	"sort"
	"testing"

	"golang.org/x/exp/slices"
)

const randomMoveConstraint = "RandomMove should always be made on an empty cell containing '-'" +
	" but cell contained %c instead"

func TestGetEmptyCells(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	want := [][2]int{{0, 0}, {0, 1}, {0, 2}, {1, 0}, {1, 1}, {1, 2}, {2, 0}, {2, 1}, {2, 2}}
	is := tictactoe.getEmptyCells()
	less := func(i int, j int) bool {
		switch {
		case want[i][0] < want[j][0]:
			return true
		case want[i][0] == want[j][0] && want[i][1] < want[j][1]:
			return true
		default:
			return false
		}
	}
	sort.Slice(want, less)
	sort.Slice(is, less)
	if !reflect.DeepEqual(is, want) {
		t.Errorf("Empty board should give all cells: Is: %v, want %v", is, want)
	}
	tictactoe.board = Board{{'X', 'O', '-'}, {'O', '-', 'X'}, {'-', 'X', 'X'}}
	want = [][2]int{{0, 2}, {1, 1}, {2, 0}}
	is = tictactoe.getEmptyCells()
	sort.Slice(want, less)
	sort.Slice(is, less)
	if !reflect.DeepEqual(is, want) {
		t.Errorf("Partial board should give these three cells: Is: %v, want %v", is, want)
	}
	tictactoe.board = Board{{'X', 'O', 'X'}, {'O', 'O', 'X'}, {'O', 'X', 'X'}}
	want = [][2]int{}
	is = tictactoe.getEmptyCells()
	sort.Slice(want, less)
	sort.Slice(is, less)
	if !reflect.DeepEqual(is, want) {
		t.Errorf("Full board should give 0 length non-nil slice: Is: %v, want %v", is, want)
	}
}

func FuzzGetEmptyCells(f *testing.F) {
	f.Add('O', 'O', 'X', 'X', '-', '-', '-', 'O', 'X', 'X')
	f.Add('-', '-', '-', '-', '-', '-', '-', '-', '-', '-')
	f.Add('O', 'O', 'X', 'X', 'X', 'X', 'O', 'O', 'X', 'O')
	f.Fuzz(func(t *testing.T, aa int32, ab int32, ac int32,
		ba int32, bb int32, bc int32,
		ca int32, cb int32, cc int32,
		player rune) {
		tictactoe := TicTacToe{Board{{aa, ab, ac}, {ba, bb, bc}, {ca, cb, cc}}, false, player, 4}
		emptyCells := tictactoe.getEmptyCells()
		if len(emptyCells) > 9 {
			t.Errorf("Number of empty cells should always be between 0 and 9 inclusive but was %v instead", len(emptyCells))
		}
	})
}

func TestGetWinningMoveFindsWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'O', 'X', 'O'}, {'X', 'O', '-'}, {'X', 'X', '-'}}
	want := Move{0, 2, 2}
	is, err := tictactoe.getWinningMove('X')
	if want != is || err != nil {
		t.Errorf("getWinningMove should find win in row: Is: %v, want %v. With error: %v", is, want, err)
	}
	tictactoe.board = Board{{'O', '-', '-'}, {'-', '-', '-'}, {'O', 'X', 'X'}}
	want = Move{0, 1, 0}
	is, err = tictactoe.getWinningMove('O')
	if want != is || err != nil {
		t.Errorf("getWinningMove should find win in col: Is: %v, want %v. With error: %v", is, want, err)
	}
	tictactoe.board = Board{{'O', '-', '-'}, {'-', 'O', '-'}, {'-', '-', '-'}}
	want = Move{0, 2, 2}
	is, err = tictactoe.getWinningMove('O')
	if want != is || err != nil {
		t.Errorf("getWinningMove should find win in diagonal: Is: %v, want %v. With error: %v", is, want, err)
	}
	tictactoe.board = Board{{'-', '-', '-'}, {'-', 'X', '-'}, {'X', '-', '-'}}
	want = Move{0, 0, 2}
	is, err = tictactoe.getWinningMove('X')
	if want != is || err != nil {
		t.Errorf("getWinningMove should find win in antidiagonal: Is: %v, want %v. With error: %v", is, want, err)
	}
}

func TestGetWinningMoveNoWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'O', 'X', 'X'}, {'-', 'O', '-'}, {'O', 'X', '-'}}
	want := Move{-99, -99, -99}
	is, err := tictactoe.getWinningMove('X')
	if want != is || err == nil {
		t.Errorf("WinMove should return error if no win exists: Is: %v, want %v. With error: %v", is, want, err)
	}
}

func TestWinMovePrioritizesWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'O', '-', '-'}, {'-', '-', '-'}, {'O', 'X', 'X'}}
	want := Move{0, 1, 0}
	is := tictactoe.winMove('O')
	if want != is {
		t.Errorf("winMove should prioritize win: Is: %v, want %v", is, want)
	}
}

func TestWinMoveWorksWithoutWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', '-', '-'}, {'-', 'O', '-'}, {'-', '-', 'X'}}
	resultingMove := tictactoe.winMove('X')
	if resultingMove.row < 0 || resultingMove.row > 2 {
		t.Errorf("Row value of resulting move has to be in 0-2 but was %d instead", resultingMove.row)
	}
	if resultingMove.col < 0 || resultingMove.col > 2 {
		t.Errorf("Col value of resulting move has to be in 0-2 but was %d instead", resultingMove.col)
	}
	if resultingMove.endState != 0 {
		t.Errorf("EndState of resulting move has to be 0 but was %d instead", resultingMove.endState)
	}
	if tictactoe.board[resultingMove.row][resultingMove.col] != '-' {
		t.Errorf(randomMoveConstraint, tictactoe.board[resultingMove.row][resultingMove.col])
	}
}

func TestGetBlockingMoveFindsWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'O', 'X', 'O'}, {'X', 'O', '-'}, {'X', 'X', '-'}}
	want := Move{0, 2, 2}
	is, err := tictactoe.getBlockingMove('O')
	if want != is || err != nil {
		t.Errorf("getBlockingMove should find block in row: Is: %v, want %v. With error: %v", is, want, err)
	}
	tictactoe.board = Board{{'O', '-', '-'}, {'-', '-', '-'}, {'O', 'X', 'X'}}
	want = Move{0, 1, 0}
	is, err = tictactoe.getBlockingMove('X')
	if want != is || err != nil {
		t.Errorf("getBlockingMove should find block in col: Is: %v, want %v. With error: %v", is, want, err)
	}
	tictactoe.board = Board{{'O', '-', '-'}, {'-', 'O', '-'}, {'-', '-', '-'}}
	want = Move{0, 2, 2}
	is, err = tictactoe.getBlockingMove('X')
	if want != is || err != nil {
		t.Errorf("getBlockingMove should find block in diagonal: Is: %v, want %v. With error: %v", is, want, err)
	}
	tictactoe.board = Board{{'-', '-', '-'}, {'-', 'X', '-'}, {'X', '-', '-'}}
	want = Move{0, 0, 2}
	is, err = tictactoe.getBlockingMove('O')
	if want != is || err != nil {
		t.Errorf("getBlockingMove should find block in antidiagonal: Is: %v, want %v. With error: %v", is, want, err)
	}
}

func TestGetBlockingMoveNoBlock(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'O', 'X', 'X'}, {'-', 'O', '-'}, {'O', 'X', '-'}}
	want := Move{-99, -99, -99}
	is, err := tictactoe.getBlockingMove('O')
	if want != is || err == nil {
		t.Errorf("getBlockingMove should return error if no block exists: Is: %v, want %v. With error: %v", is, want, err)
	}
}

func TestBlockWinMovePrioritizesWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', '-', 'O'}, {'-', '-', '-'}, {'X', '-', 'O'}}
	want := Move{0, 1, 0}
	is := tictactoe.blockWinMove('X')
	if want != is {
		t.Errorf("blockWinMove should prioritize win: Is: %v, want %v", is, want)
	}
}

func TestBlockWinMoveBlocksIfNoWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'-', '-', 'O'}, {'-', '-', '-'}, {'X', '-', 'O'}}
	want := Move{0, 1, 2}
	is := tictactoe.blockWinMove('X')
	if want != is {
		t.Errorf("blockWinMove should block if there is no win: Is: %v, want %v", is, want)
	}
}

func TestBlockWinMoveWorksWithoutWin(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', '-', '-'}, {'-', 'O', '-'}, {'-', '-', 'X'}}
	resultingMove := tictactoe.blockWinMove('X')
	if resultingMove.row < 0 || resultingMove.row > 2 {
		t.Errorf("Row value of resulting move has to be in 0-2 but was %d instead", resultingMove.row)
	}
	if resultingMove.col < 0 || resultingMove.col > 2 {
		t.Errorf("Col value of resulting move has to be in 0-2 but was %d instead", resultingMove.col)
	}
	if resultingMove.endState != 0 {
		t.Errorf("EndState of resulting move has to be 0 but was %d instead", resultingMove.endState)
	}
	if tictactoe.board[resultingMove.row][resultingMove.col] != '-' {
		t.Errorf(randomMoveConstraint, tictactoe.board[resultingMove.row][resultingMove.col])
	}
}

func TestRandomMove(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', 'X', '-'}, {'O', 'X', 'O'}, {'X', 'O', 'O'}}
	want := Move{0, 0, 2}
	is := tictactoe.randomMove()
	if want != is {
		t.Errorf("RandomMove should return only valid move: Is: %v, want %v", is, want)
	}
}

func FuzzRandomMove(f *testing.F) {
	f.Add('O', 'O', 'X', 'X', '-', '-', '-', 'O', 'X')
	f.Add('-', '-', '-', '-', '-', '-', '-', '-', '-')
	f.Add('O', 'O', 'X', 'X', 'X', 'X', 'O', 'O', 'X')
	f.Fuzz(func(t *testing.T, aa int32, ab int32, ac int32, ba int32, bb int32, bc int32, ca int32, cb int32, cc int32) {
		tictactoe := TicTacToe{Board{{aa, ab, ac}, {ba, bb, bc}, {ca, cb, cc}}, false, 'X', 4}
		resultingMove := tictactoe.randomMove()
		emptyMove := Move{endState: -99, row: -99, col: -99}
		if resultingMove != emptyMove {
			if resultingMove.row < 0 || resultingMove.row > 2 {
				t.Errorf("Row value of resulting move has to be in 0-2 but was %d instead", resultingMove.row)
			}
			if resultingMove.col < 0 || resultingMove.col > 2 {
				t.Errorf("Col value of resulting move has to be in 0-2 but was %d instead", resultingMove.col)
			}
			if resultingMove.endState != 0 {
				t.Errorf("EndState of resulting move has to be 0 but was %d instead", resultingMove.endState)
			}
			if tictactoe.board[resultingMove.row][resultingMove.col] != '-' {
				t.Errorf(randomMoveConstraint, tictactoe.board[resultingMove.row][resultingMove.col])
			}
		}
	})
}

func TestMinmaxEmptyDraws(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	want := 0
	is := tictactoe.minmax('X').endState
	if want != is {
		t.Errorf("Perfectly played game ends in draw: Is: %v, want %v", is, want)
	}
}

func TestMinmaxCorrectBaseCase(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', 'O', 'X'}, {'O', 'X', 'O'}, {'O', 'X', 'O'}}
	want := Move{0, -1, -1}
	is := tictactoe.minmax('X')
	if want != is {
		t.Errorf("Won board should return win: Is: %v, want %v", is, want)
	}
	want = Move{0, -1, -1}
	is = tictactoe.minmax('O')
	if want != is {
		t.Errorf("Won board should return win: Is: %v, want %v", is, want)
	}
}

func TestMinmaxBestMove(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', 'X', '-'}, {'O', 'X', 'O'}, {'X', 'O', 'O'}}
	want := Move{1, 0, 2}
	is := tictactoe.minmax('X')
	if want != is {
		t.Errorf("Only possible move should return win: Is: %v, want %v", is, want)
	}
	want = Move{1, 0, 2}
	is = tictactoe.minmax('O')
	if want != is {
		t.Errorf("Only possible move should return win: Is: %v, want %v", is, want)
	}

	tictactoe.board = Board{{'O', 'O', 'X'}, {'X', '-', 'O'}, {'-', 'O', 'X'}}
	want = Move{0, 1, 1}
	is = tictactoe.minmax('X')
	if want != is {
		t.Errorf("Best of two moves returns draw: Is: %v, want %v", is, want)
	}
	want = Move{1, 1, 1}
	is = tictactoe.minmax('O')
	if want != is {
		t.Errorf("Best of two moves returns win: Is: %v, want %v", is, want)
	}

	tictactoe.board = Board{{'O', 'O', 'X'}, {'X', '-', '-'}, {'-', 'O', 'X'}}
	want = Move{1, 1, 1}
	is = tictactoe.minmax('O')
	if want != is {
		t.Errorf("Best move returns win: Is: %v, want %v", is, want)
	}

	tictactoe.board = Board{{'X', '-', 'X'}, {'O', '-', 'A'}, {'X', 'A', 'O'}}
	wantState := -1
	isState := tictactoe.minmax('O').endState
	if wantState != isState {
		t.Errorf("Fork leads to loss: Is: %v, want %v", is, want)
	}

	tictactoe.board = Board{{'X', '-', '-'}, {'-', '-', '-'}, {'-', '-', '-'}}
	want = Move{0, 1, 1}
	is = tictactoe.minmax('O')
	if want != is {
		t.Errorf("Blocking forks leads to draw: Is: %v, want %v", is, want)
	}
}

func FuzzMinMax(f *testing.F) {
	f.Add('O', 'O', 'X', 'X', '-', '-', '-', 'O', 'X', 'X')
	f.Add('-', '-', '-', '-', '-', '-', '-', '-', '-', '-')
	f.Add('O', 'O', 'X', 'X', 'X', 'X', 'O', 'O', 'X', 'O')
	f.Fuzz(func(t *testing.T, aa int32, ab int32, ac int32,
		ba int32, bb int32, bc int32,
		ca int32, cb int32, cc int32,
		player rune) {
		tictactoe := TicTacToe{Board{{aa, ab, ac}, {ba, bb, bc}, {ca, cb, cc}}, false, player, 4}
		bestMove := tictactoe.minmax(player)
		if !slices.Contains([]int{-1, 0, 1}, bestMove.endState) {
			t.Errorf("EndState should always be in -1,0,1 but was %v", bestMove.endState)
		}
		if !slices.Contains([]int{-1, 0, 1, 2}, bestMove.row) {
			t.Errorf("Move row should always be in -1,0,1,2 but was %v", bestMove.row)
		}
		if !slices.Contains([]int{-1, 0, 1, 2}, bestMove.col) {
			t.Errorf("Move col should always be in -1,0,1,2 but was %v", bestMove.col)
		}
	})
}

func TestFixSpotFixesOnce(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	if !tictactoe.fixSpot('X', 0, 0) {
		t.Errorf("Fix spot should work first time")
	}
	if !(tictactoe.board[0][0] == 'X') {
		t.Errorf("Spot should have been changed:  Is: %v, want %v", tictactoe.board[0][0], 'X')
	}
	if tictactoe.fixSpot('X', 0, 0) {
		t.Errorf("Fix spot should not work a second time on the same spot")
	}
}

func TestFixSpotRejectsInvalid(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	if tictactoe.fixSpot('X', 7, 1) {
		t.Errorf("Fix spot should reject out of bounds: Row: %v, Col: %v", 7, 1)
	}
	if tictactoe.fixSpot('X', 2, -1) {
		t.Errorf("Fix spot should reject out of bounds: Row: %v, Col: %v", 2, -1)
	}
}

func TestIsPlayerWinRecognizesNoWinner(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	if tictactoe.isPlayerWin('X') {
		t.Errorf("Empty board should have no winner 'X'!")
	}
	if tictactoe.isPlayerWin('O') {
		t.Errorf("Empty board should have no winner 'O'!")
	}
}

func TestIsPlayerWinAcceptsRow(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', 'X', 'X'}, {'-', '-', '-'}, {'-', '-', '-'}}
	if !tictactoe.isPlayerWin('X') {
		t.Errorf("Full row of 'X' should give winner 'X'!")
	}
	if tictactoe.isPlayerWin('O') {
		t.Errorf("Full row of 'X' should not give winner 'O'!")
	}
}

func TestIsPlayerWinAcceptsColumn(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'O', '-', '-'}, {'O', '-', '-'}, {'O', '-', '-'}}
	if !tictactoe.isPlayerWin('O') {
		t.Errorf("Full column of 'O' should give winner 'O'!")
	}
	if tictactoe.isPlayerWin('X') {
		t.Errorf("Full column of 'O' should not give winner 'X'!")
	}
}

func TestIsPlayerWinAcceptsDiagonals(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', '-', '-'}, {'-', 'X', '-'}, {'O', '-', 'X'}}
	if !tictactoe.isPlayerWin('X') {
		t.Errorf("Full diagonal 'X' should give winner 'X'!")
	}
	if tictactoe.isPlayerWin('O') {
		t.Errorf("Full diagonal of 'X' should not give winner 'O'!")
	}
	tictactoe.board = Board{{'X', 'X', 'O'}, {'-', 'O', 'X'}, {'O', 'X', 'O'}}
	if !tictactoe.isPlayerWin('O') {
		t.Errorf("Full anti-diagonal 'O' should give winner 'O'!")
	}
	if tictactoe.isPlayerWin('X') {
		t.Errorf("Full anti-diagonal of 'O' should not give winner 'X'!")
	}
	tictactoe.board = Board{{'X', 'O', 'X'}, {'O', 'X', 'A'}, {'X', 'A', 'O'}}
	if !tictactoe.isPlayerWin('X') {
		t.Errorf("Full anti-diagonal 'X' should give winner 'X'!")
	}
	if tictactoe.isPlayerWin('O') {
		t.Errorf("Full anti-diagonal of 'X' should not give winner 'O'!")
	}
}

func TestIsBoardFilledRecognizesEmptyBoard(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	if tictactoe.isBoardFilled() {
		t.Errorf("Empty board is not full!")
	}
}

func TestIsBoardFilledRecognizesPartialBoard(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', '-', '-'}, {'-', 'X', '-'}, {'O', '-', 'X'}}
	if tictactoe.isBoardFilled() {
		t.Errorf("Partially filled board is not full!")
	}
	tictactoe.board = Board{{'X', 'X', 'O'}, {'-', 'X', 'X'}, {'O', 'O', 'X'}}
	if tictactoe.isBoardFilled() {
		t.Errorf("Partially filled board is not full!")
	}
}

func TestIsBoardFilledRecognizesFullBoard(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	tictactoe.board = Board{{'X', 'X', 'X'}, {'X', 'X', 'X'}, {'O', 'X', 'X'}}
	if !tictactoe.isBoardFilled() {
		t.Errorf("Full board isfull!")
	}
	tictactoe.board = Board{{'X', 'X', 'O'}, {'O', 'X', 'X'}, {'O', 'O', 'X'}}
	if !tictactoe.isBoardFilled() {
		t.Errorf("Full filled board is full!")
	}
}

func TestSwapPlayer(t *testing.T) {
	t.Parallel()
	tictactoe := NewTicTacToe()
	want := 'X'
	is := tictactoe.swapPlayer('O')
	if want != is {
		t.Errorf("'O' should be swapped to 'X': Is: %v, want %v", is, want)
	}
	want = 'O'
	is = tictactoe.swapPlayer('X')
	if want != is {
		t.Errorf("'X' should be swapped to 'O': Is: %v, want %v", is, want)
	}
}

func FuzzSwapPlayer(f *testing.F) {
	f.Add('X')
	f.Add('O')
	f.Fuzz(func(t *testing.T, player rune) {
		tictactoe := NewTicTacToe()
		newPlayer := tictactoe.swapPlayer(player)
		if newPlayer != 'X' && newPlayer != 'O' {
			t.Errorf("swapPlayer should always return 'O' or 'X' but returned %v instead.", newPlayer)
		}
	})
}
