package tictactoe_java.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;

import java.util.Scanner;

import tictactoe_java.TicTacToeJava;

public class TicTacToeJavaTest {

    TicTacToeJava tictactoe;

    @BeforeEach
    void setup() {
        tictactoe = new TicTacToeJava();
    }

    @Test
    @DisplayName("Reading user input should work.")
    public void testGetUserYesNo() {
        Scanner scanner = new Scanner("a\nN");
        assertFalse(tictactoe.getUserYesNo("Test question", scanner));

        scanner = new Scanner("1 \n z \n y");
        assertTrue(tictactoe.getUserYesNo("Test question", scanner));
    }
}
