package tictactoe;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import java.util.Arrays;

import tictactoe.game.AIPlayer;
import tictactoe.game.Marker;
import tictactoe.game.Board;
import tictactoe.game.Move;

public class AIPlayerTest {

    @RepeatedTest(5)
    public void testRandomMove() {
        Board board = new Board();
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 1);
        Move randomMove = aiPlayer.getMove(board, marker);
        assertTrue(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8).contains(randomMove.getSpot()));

        board = new Board(new char[] { 'X', 'X', 'X', '3', '4', 'O', 'O', 'O', 'O' });
        randomMove = aiPlayer.getMove(board, marker);
        assertTrue(Arrays.asList(3, 4).contains(randomMove.getSpot()));
    }

    @RepeatedTest(5)
    public void testWinningMoveEmpty() {
        Board board = new Board();
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 2);
        Move randomMove = aiPlayer.getMove(board, marker);
        assertTrue(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8).contains(randomMove.getSpot()));

        board = new Board(new char[] { 'X', 'X', '2', '3', '4', '5', '6', '7', '8' });
        Move winningMove = aiPlayer.getMove(board, marker);
        assertEquals(2, winningMove.getSpot());
    }

    @Test
    public void testWinningMoveRow() {
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 2);
        Board board = new Board(new char[] { 'X', 'X', '2', '3', '4', '5', '6', '7', '8' });
        Move winningMove = aiPlayer.getMove(board, marker);
        assertEquals(2, winningMove.getSpot());

        board = new Board(new char[] { '0', '1', '2', 'O', '4', 'O', '6', '7', '8' });
        winningMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(4, winningMove.getSpot());

        board = new Board(new char[] { '0', '1', '2', '3', '4', '5', '6', 'X', 'X' });
        winningMove = aiPlayer.getMove(board, marker);
        assertEquals(6, winningMove.getSpot());
    }

    @Test
    public void testWinningMoveCol() {
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 2);
        Board board = new Board(new char[] { 'O', '1', '2', 'O', '4', '5', '6', '7', '8' });
        Move winningMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(6, winningMove.getSpot());

        board = new Board(new char[] { '0', 'X', '2', '3', '4', '5', '6', 'X', '8' });
        winningMove = aiPlayer.getMove(board, marker);
        assertEquals(4, winningMove.getSpot());

        board = new Board(new char[] { '0', '1', '2', '3', '4', 'O', '6', '7', 'O' });
        winningMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(2, winningMove.getSpot());
    }

    @Test
    public void testWinningMoveDiag() {
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 2);
        Board board = new Board(new char[] { 'X', '1', '2', '3', 'X', '5', '6', '7', '8' });
        Move winningMove = aiPlayer.getMove(board, marker);
        assertEquals(8, winningMove.getSpot());

        board = new Board(new char[] { '0', '1', '2', '3', 'O', '5', 'O', '7', '8' });
        winningMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(2, winningMove.getSpot());
    }

    @RepeatedTest(5)
    public void testBlockingMoveEmpty() {
        Board board = new Board();
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 3);
        Move randomMove = aiPlayer.getMove(board, marker);
        assertTrue(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8).contains(randomMove.getSpot()));

        board = new Board(new char[] { 'X', 'X', '2', '3', '4', '5', '6', '7', '8' });
        Move winningMove = aiPlayer.getMove(board, marker);
        assertEquals(2, winningMove.getSpot());
    }

    @Test
    public void testBlockingMoveRow() {
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 3);
        Board board = new Board(new char[] { 'X', 'X', '2', '3', '4', '5', 'O', 'O', '8' });
        Move winningMove = aiPlayer.getMove(board, marker);
        assertEquals(2, winningMove.getSpot(), "Should prioritize win");

        aiPlayer = new AIPlayer('X', 3);
        board = new Board(new char[] { 'X', '1', '2', '3', '4', '5', 'O', 'O', '8' });
        Move blockingMove = aiPlayer.getMove(board, marker);
        assertEquals(8, blockingMove.getSpot(), "Should fall back to blocking");
    }

    @Test
    public void testBlockingMoveCol() {
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 3);
        Board board = new Board(new char[] { 'O', '1', 'X', '3', '4', '5', 'O', '7', 'X' });
        Move winningMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(3, winningMove.getSpot(), "Should prioritize win");

        aiPlayer = new AIPlayer('X', 3);
        board = new Board(new char[] { '0', '1', 'X', '3', '4', '5', 'O', '7', 'X' });
        Move blockingMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(5, blockingMove.getSpot(), "Should fall back to blocking");
    }

    @Test
    public void testBlockingMoveDiag() {
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 3);
        Board board = new Board(new char[] { 'X', '1', '2', '3', '4', '5', '6', '7', 'X' });
        Move winningMove = aiPlayer.getMove(board, marker);
        assertEquals(4, winningMove.getSpot(), "Should prioritize win");

        aiPlayer = new AIPlayer('X', 3);
        board = new Board(new char[] { 'O', '1', '2', '3', '4', '5', '6', '7', 'O' });
        Move blockingMove = aiPlayer.getMove(board, marker);
        assertEquals(4, blockingMove.getSpot(), "Should fall back to blocking");
    }

    @RepeatedTest(5)
    public void testMinmaxEmpty() {
        Board board = new Board();
        Marker marker = new Marker();

        AIPlayer aiPlayer = new AIPlayer('X', 4);
        Move randomMove = aiPlayer.getMove(board, marker);
        assertTrue(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8).contains(randomMove.getSpot()));
    }

    @Test
    public void testMinmaxBase() {
        Marker marker = new Marker();
        AIPlayer aiPlayer = new AIPlayer('X', 4);

        Board board = new Board(new char[] { 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' });
        Move bestMove = aiPlayer.getMove(board, marker);
        assertEquals(1, bestMove.getEndState());

        board = new Board(new char[] { 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' });
        bestMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(-1, bestMove.getEndState());

        board = new Board(new char[] { 'X', 'O', 'X', 'X', 'O', 'O', 'O', 'X', 'X' });
        bestMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(0, bestMove.getEndState());
        bestMove = aiPlayer.getMove(board, marker);
        assertEquals(0, bestMove.getEndState());
    }

    @Test
    public void testMinmaxLevel0() {
        Marker marker = new Marker();
        AIPlayer aiPlayer = new AIPlayer('X', 4);

        Board board = new Board(new char[] { 'X', 'X', '2', 'O', 'X', 'O', 'X', 'O', 'O' });
        Move bestMove = aiPlayer.getMove(board, marker);
        assertEquals(2, bestMove.getSpot());
        assertEquals(1, bestMove.getEndState());
        bestMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(2, bestMove.getSpot());
        assertEquals(1, bestMove.getEndState());
    }

    @Test
    public void testMinmaxLevel1() {
        Marker marker = new Marker();
        AIPlayer aiPlayer = new AIPlayer('X', 4);

        Board board = new Board(new char[] { 'O', 'O', 'X', 'X', '4', 'O', '6', 'O', 'X' });
        Move bestMove = aiPlayer.getMove(board, marker);
        assertEquals(4, bestMove.getSpot());
        assertEquals(0, bestMove.getEndState());
        bestMove = aiPlayer.getMove(board, marker.swapMarker());
        assertEquals(4, bestMove.getSpot());
        assertEquals(1, bestMove.getEndState());
    }

    @Test
    public void testMinmaxDeep() {
        Marker marker = new Marker();
        AIPlayer aiPlayer = new AIPlayer('X', 4);

        Board board = new Board(new char[] { 'O', '1', '2', '3', '4', '5', '6', '7', '8' });
        Move bestMove = aiPlayer.getMove(board, marker);
        assertEquals(4, bestMove.getSpot());
        assertEquals(0, bestMove.getEndState());
    }
}
