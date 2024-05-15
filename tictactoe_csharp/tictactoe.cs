Game game = new Game();
game.playGame();


class Game
{

    private char[] board;

    private int[][] win_conditions = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8], // rows
    [0, 3, 6], [1, 4, 7], [2, 5, 8], // columns
    [0, 4, 8], [2, 4, 6] // diagonals
];

    public Game()
    {
        board = ['0', '1', '2', '3', '4', '5', '6', '7', '8'];
    }
    private void player_turn(char player) {
        int move;

        while (true) {
            Console.WriteLine($"Player {player}, enter your move (0-8): ");
            show_board();
            if (!int.TryParse(Console.ReadLine(), out move)) {
                Console.WriteLine("Invalid move. Enter a number.");
                continue;
            }
            if (move < 0 || move > 8) {
                Console.WriteLine("Invalid move. Enter a number in bounds.");
                continue;
            }
            if (board[move] == 'X' || board[move] == 'O') {
                Console.WriteLine("Invalid move. That space is already taken.");
                continue;
            }
            break;
        }
        board[move] = player;
    }

    private bool is_winner(char player) {
        foreach (var condition in win_conditions) {
            if (condition.All(i => board[i] == player)) {
                return true;
            }
        }
        return false;
    }

    private bool is_board_full() {
        return board.All(c => c == 'X' || c == 'O');
    }

    private char swapPlayer(char player) {
        return player == 'X' ? 'O' : 'X';
    }

    private void show_board() {
        Console.WriteLine($"{board[0]} | {board[1]} | {board[2]}");
        Console.WriteLine("---------");
        Console.WriteLine($"{board[3]} | {board[4]} | {board[5]}");
        Console.WriteLine("---------");
        Console.WriteLine($"{board[6]} | {board[7]} | {board[8]}");
    }

    public void playGame()
    {
        char currentPlayer = 'X';
        while (true) {
            player_turn(currentPlayer);
            if (is_winner(currentPlayer)) {
                Console.WriteLine($"Player {currentPlayer} wins!");
                break;
            }
            if (is_board_full()) {
                Console.WriteLine("It's a tie!");
                break;
            }
            currentPlayer = swapPlayer(currentPlayer);
        }
        show_board();
    }
}
