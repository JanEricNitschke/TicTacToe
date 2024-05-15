AIStrength X = 0; // Variable to store the value after -X
AIStrength O = 0; // Variable to store the value after -O



// Iterate through the arguments
for (int i = 0; i < args.Length; i++)
{
    // Check if the argument starts with '-'
    if (args[i].StartsWith('-'))
    {
        // Check for -X argument
        if (args[i] == "-X" && i + 1 < args.Length)
        {
            // Parse the value after -X
            if (Enum.TryParse(args[i + 1], out AIStrength value))
            {
                X = value;
            }
        }
        // Check for -O argument
        else if (args[i] == "-O" && i + 1 < args.Length)
        {
            // Parse the value after -O
            if (Enum.TryParse(args[i + 1], out AIStrength value))
            {
                O = value;
            }
        }
    }
}

Game game = new(X, O);
game.PlayGame();


enum AIStrength
{
    HUMAN = 0,
    EASY = 1,
    MEDIUM = 2,
    HARD = 3,
    IMPOSSIBLE = 4
}

enum Score { WIN = 1, TIE = 0, LOSE = -1 }

struct Move(int spot, Score score)
{
    public int spot = spot;
    public Score score = score;
}

class Game(AIStrength X = 0, AIStrength O = 0)
{
    private readonly char[] board = ['0', '1', '2', '3', '4', '5', '6', '7', '8'];
    private readonly AIStrength X_strength = X;
    private readonly AIStrength O_strength = O;

    private readonly int[][] win_conditions = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8], // rows
    [0, 3, 6], [1, 4, 7], [2, 5, 8], // columns
    [0, 4, 8], [2, 4, 6] // diagonals
];

    private void PlayerTurn(char player)
    {
        int move;

        while (true)
        {
            Console.WriteLine($"Player {player}, enter your move (0-8): ");
            ShowBoard();
            if (!int.TryParse(Console.ReadLine(), out move))
            {
                Console.WriteLine("Invalid move. Enter a number.");
                continue;
            }
            if (move < 0 || move > 8)
            {
                Console.WriteLine("Invalid move. Enter a number in bounds.");
                continue;
            }
            if (board[move] == 'X' || board[move] == 'O')
            {
                Console.WriteLine("Invalid move. That space is already taken.");
                continue;
            }
            break;
        }
        board[move] = player;
    }

    private int[] EmptyCells()
    {
        return board.Select((c, i) => c != 'X' && c != 'O' ? i : -1).Where(i => i != -1).ToArray();
    }

    private int RandomMove()
    {
        var emptyCells = EmptyCells();
        return emptyCells[new Random().Next(emptyCells.Length)];
    }

    private int? TryWinningMove(char player)
    {
        foreach (var condition in win_conditions)
        {
            var emptyCells = condition.Where(i => board[i] != 'X' && board[i] != 'O').ToArray();
            if (emptyCells.Length == 1 && condition.Count(i => board[i] == player) == 2)
            {
                return emptyCells[0];
            }
        }
        return null;
    }

    private int WinningMove(char player)
    {
        var move = TryWinningMove(player);
        return move ?? RandomMove();
    }

    private int WinningBlockingMove(char player)
    {
        var move = TryWinningMove(player);
        if (move != null)
        {
            return move.Value;
        }
        var opponent = SwapPlayer(player);
        move = TryWinningMove(opponent);
        return move ?? RandomMove();
    }

    private static Score NegateScore(Score score)
    {
        return score switch
        {
            Score.WIN => Score.LOSE,
            Score.LOSE => Score.WIN,
            _ => score
        };
    }

    private Move BestMove(char player)
    {
        var bestMove = new Move(-1, Score.LOSE);
        if (IsWinner(player))
        {
            return new Move(-1, Score.WIN);
        }
        if (IsWinner(SwapPlayer(player)))
        {
            return new Move(-1, Score.LOSE);
        }

        var emptyCells = EmptyCells();
        if (emptyCells.Length == 0)
        {
            return new Move(-1, Score.TIE);
        }
        if (emptyCells.Length == 9)
        {
            return new Move(RandomMove(), Score.TIE);
        }

        foreach (var cell in emptyCells)
        {
            board[cell] = player;
            var score = NegateScore(BestMove(SwapPlayer(player)).score);
            board[cell] = (char)(cell + '0');
            if (score >= bestMove.score)
            {
                bestMove = new Move(cell, score);
            }
        }
        return bestMove;
    }

    private int MinmaxMove(char player)
    {
        return BestMove(player).spot;
    }

    private void AiTurn(char player, AIStrength aIStrength)
    {
        Console.WriteLine($"AI turn as player {player} with strength {aIStrength}");
        ShowBoard();
        var move = aIStrength switch
        {
            AIStrength.HUMAN => throw new Exception("AIStrength.HUMAN should not be used for AI turn."),
            AIStrength.EASY => RandomMove(),
            AIStrength.MEDIUM => WinningMove(player),
            AIStrength.HARD => WinningBlockingMove(player),
            AIStrength.IMPOSSIBLE => MinmaxMove(player),
            _ => throw new ArgumentException("Invalid AI strength")
        };
        board[move] = player;
        Thread.Sleep(1000);
    }

    private bool IsWinner(char player)
    {
        foreach (var condition in win_conditions)
        {
            if (condition.All(i => board[i] == player))
            {
                return true;
            }
        }
        return false;
    }

    private bool IsBoardFull()
    {
        return board.All(c => c == 'X' || c == 'O');
    }

    private static char SwapPlayer(char player)
    {
        return player == 'X' ? 'O' : 'X';
    }

    private void ShowBoard()
    {
        Console.WriteLine($"{board[0]} | {board[1]} | {board[2]}");
        Console.WriteLine("---------");
        Console.WriteLine($"{board[3]} | {board[4]} | {board[5]}");
        Console.WriteLine("---------");
        Console.WriteLine($"{board[6]} | {board[7]} | {board[8]}");
    }

    public void PlayGame()
    {
        char currentPlayer = 'X';
        while (true)
        {
            if (currentPlayer == 'X' && X_strength != AIStrength.HUMAN)
            {
                AiTurn(currentPlayer, X_strength);
            }
            else if (currentPlayer == 'O' && O_strength != AIStrength.HUMAN)
            {
                AiTurn(currentPlayer, O_strength);
            }
            else
            {
                PlayerTurn(currentPlayer);
            }

            if (IsWinner(currentPlayer))
            {
                Console.WriteLine($"Player {currentPlayer} wins!");
                break;
            }
            if (IsBoardFull())
            {
                Console.WriteLine("It's a tie!");
                break;
            }
            currentPlayer = SwapPlayer(currentPlayer);
        }
        ShowBoard();
    }
}
