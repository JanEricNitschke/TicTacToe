namespace Tictactoe.Lib
{
    public enum AIStrength
    {
        HUMAN = 0,
        EASY = 1,
        MEDIUM = 2,
        HARD = 3,
        IMPOSSIBLE = 4
    }

    public enum Score { WIN = 1, TIE = 0, LOSE = -1 }

    public struct Move(int spot, Score score)
    {
        public int spot = spot;
        public Score score = score;
    }

    public class Game(AIStrength X = 0, AIStrength O = 0)
    {
        internal char[] board = ['0', '1', '2', '3', '4', '5', '6', '7', '8'];
        internal readonly AIStrength X_strength = X;
        internal readonly AIStrength O_strength = O;

        internal readonly int[][] win_conditions = [
            [0, 1, 2], [3, 4, 5], [6, 7, 8], // rows
            [0, 3, 6], [1, 4, 7], [2, 5, 8], // columns
            [0, 4, 8], [2, 4, 6] // diagonals
        ];

        internal void PlayerTurn(char player)
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

        internal int[] EmptyCells()
        {
            return board.Select((c, i) => c != 'X' && c != 'O' ? i : -1).Where(i => i != -1).ToArray();
        }

        internal int RandomMove()
        {
            var emptyCells = EmptyCells();
            return emptyCells[new Random().Next(emptyCells.Length)];
        }

        internal int? TryWinningMove(char player)
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

        internal int WinningMove(char player)
        {
            var move = TryWinningMove(player);
            return move ?? RandomMove();
        }

        internal int WinningBlockingMove(char player)
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

        internal static Score NegateScore(Score score)
        {
            return score switch
            {
                Score.WIN => Score.LOSE,
                Score.LOSE => Score.WIN,
                _ => score
            };
        }

        internal Move BestMove(char player)
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

        internal int MinmaxMove(char player)
        {
            return BestMove(player).spot;
        }

        internal void AiTurn(char player, AIStrength aIStrength)
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

        internal bool IsWinner(char player)
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

        internal bool IsBoardFull()
        {
            return board.All(c => c == 'X' || c == 'O');
        }

        internal static char SwapPlayer(char player)
        {
            return player == 'X' ? 'O' : 'X';
        }

        internal void ShowBoard()
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
}
