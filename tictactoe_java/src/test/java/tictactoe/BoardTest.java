package tictactoe;

import org.junit.jupiter.api.Test;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import tictactoe.game.Board;
import tictactoe.game.ConditionResult;
import tictactoe.game.Marker;

public class BoardTest {

    @Test
    public void testBoardFull() {
        Board board = new Board();
        assertFalse(board.boardFull());
        board = new Board(new char[] { 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X' });
        assertTrue(board.boardFull());
    }

    @Test
    public void testGetOpenSpots() {
        Board board = new Board();
        ArrayList<Integer> expectedList = new ArrayList<Integer>(Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8));
        assertEquals(expectedList, board.getOpenSpots());
        board = new Board(new char[] { 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X' });
        expectedList = new ArrayList<Integer>();
        assertEquals(expectedList, board.getOpenSpots());

        board = new Board(new char[] { 'X', '1', 'X', 'O', '4', 'O', 'X', 'O', '8' });
        expectedList = new ArrayList<Integer>(Arrays.asList(1, 4, 8));
        assertEquals(expectedList, board.getOpenSpots());
    }

    @Test
    public void testPlayerWinEmpty() {
        Board board = new Board();
        Marker marker = new Marker();
        assertFalse(board.playerWin(marker));
        assertFalse(board.playerWin(marker.swapMarker()));
    }

    @Test
    public void testPlayerWinRows() {
        Board board = new Board();
        Marker marker = new Marker();
        board = new Board(new char[] { 'X', 'X', 'X', '3', '4', '5', '6', '7', '8' });
        assertTrue(board.playerWin(marker));
        assertFalse(board.playerWin(marker.swapMarker()));
        board = new Board(new char[] { '0', '1', '2', 'O', 'O', 'O', '6', '7', '8' });
        assertFalse(board.playerWin(marker));
        assertTrue(board.playerWin(marker.swapMarker()));
        board = new Board(new char[] { '0', '1', '2', '3', '4', '5', 'X', 'X', 'X' });
        assertTrue(board.playerWin(marker));
        assertFalse(board.playerWin(marker.swapMarker()));
    }

    @Test
    public void testPlayerWinCols() {
        Board board = new Board();
        Marker marker = new Marker();
        board = new Board(new char[] { 'O', '1', '2', 'O', '4', '5', 'O', '7', '8' });
        assertFalse(board.playerWin(marker));
        assertTrue(board.playerWin(marker.swapMarker()));
        board = new Board(new char[] { '0', 'X', '2', '3', 'X', '5', '6', 'X', '8' });
        assertTrue(board.playerWin(marker));
        assertFalse(board.playerWin(marker.swapMarker()));
        board = new Board(new char[] { '0', '1', 'O', '3', '4', 'O', '6', '7', 'O' });
        assertFalse(board.playerWin(marker));
        assertTrue(board.playerWin(marker.swapMarker()));
    }

    @Test
    public void testPlayerWinDiags() {
        Board board = new Board();
        Marker marker = new Marker();
        board = new Board(new char[] { 'X', '1', '2', '3', 'X', '5', '6', '7', 'X' });
        assertTrue(board.playerWin(marker));
        assertFalse(board.playerWin(marker.swapMarker()));
        board = new Board(new char[] { '0', '1', 'O', '3', 'O', '5', 'O', '7', '8' });
        assertFalse(board.playerWin(marker));
        assertTrue(board.playerWin(marker.swapMarker()));
    }

    @Test
    public void testCheckCondition() {
        Board board = new Board();
        Marker marker = new Marker();
        int expectedSpotsDone = 0;
        LinkedList<Integer> expectedSpotsOpen = new LinkedList<Integer>(Arrays.asList(0, 1, 2));
        ConditionResult conditionResult = board.checkCondition(new int[] { 0, 1, 2 }, marker);
        assertEquals(expectedSpotsDone, conditionResult.getSpotsDone());
        assertEquals(expectedSpotsOpen, conditionResult.getSpotsOpen());

        board = new Board(new char[] { '0', 'O', '2', '3', '4', '5', '6', 'O', '8' });
        conditionResult = board.checkCondition(new int[] { 1, 7, 8 }, marker.swapMarker());
        expectedSpotsDone = 2;
        expectedSpotsOpen = new LinkedList<Integer>(Arrays.asList(8));
        assertEquals(expectedSpotsDone, conditionResult.getSpotsDone());
        assertEquals(expectedSpotsOpen, conditionResult.getSpotsOpen());
    }

    @Test
    public void testFixSpot() {
        Board board = new Board();
        Marker marker = new Marker();
        assertTrue(board.fixSpot(0, marker), "Should be able to fix an empty spot in bounds.");
        assertFalse(board.fixSpot(0, marker), "Should not be able to fix occupied spot");
        assertFalse(board.fixSpot(-1, marker), "Should not be able to fix spot out of bounds below.");
        assertFalse(board.fixSpot(9, marker), "Should not be able to fix spot out of bounds above.");
    }

    @Test
    public void testClearSpot() {
        Board board = new Board(new char[] { 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' });
        char cleared = board.clearSpot(1);
        board.fixSpot(1, new Marker());
        assertEquals('1', cleared);
        cleared = board.clearSpot(8);
        board.fixSpot(1, new Marker());
        assertEquals('8', cleared);
    }

}
