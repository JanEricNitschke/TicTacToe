package tictactoe_java.game;

public class Board {
    char[] gameBoard = { '0', '1', '2', '3', '4', '5', '6', '7', '8' };
    int[][] winConditions = { { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 }, // Rows
            { 0, 3, 6 }, { 1, 4, 7 }, { 2, 5, 8 }, /// Cols
            { 0, 4, 8 }, { 2, 4, 6 } }; // Diagonals

    public void showBoard() {
        String lineSeparator = "---------------";
        System.out.println(lineSeparator);
        for (int row = 0; row < 3; row++) {
            for (int col = 0; col < 3; col++) {
                System.out.print("| " + gameBoard[(row * 3) + col] + " |");
            }
            System.out.println("\n" + lineSeparator);
        }
    }
}
