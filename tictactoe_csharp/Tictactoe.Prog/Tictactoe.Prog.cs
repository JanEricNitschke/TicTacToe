using Tictactoe.Lib;


AIStrength X = AIStrength.HUMAN;
AIStrength O = AIStrength.HUMAN;

for (int i = 0; i < args.Length; i++)
{
    if (args[i].StartsWith('-'))
    {
        if (args[i] == "-X" && i + 1 < args.Length)
        {
            if (Enum.TryParse(args[i + 1], out AIStrength value))
            {
                X = value;
            }
        }
        else if (args[i] == "-O" && i + 1 < args.Length)
        {
            if (Enum.TryParse(args[i + 1], out AIStrength value))
            {
                O = value;
            }
        }
    }
}

Game game = new(X, O);
game.PlayGame();
