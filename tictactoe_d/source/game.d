module game;

immutable size_t[3][8] WIN_CONDITIONS = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6]
];

char swap(char player) @safe @nogc nothrow pure
{
    return player == 'X' ? 'O' : 'X';
}

void show(const char[9] board) @safe
{
    import std.stdio : writeln;
    import std.string : format;

    writeln("+---+---+---+");
    for (int i = 0; i < 9; i += 3)
    {
        writeln("| %s | %s | %s |".format(board[i], board[i + 1], board[i + 2]));
        writeln("+---+---+---+");
    }
}

bool full(const char[9] board) @safe @nogc nothrow pure
{
    foreach (cell; board)
    {
        if (cell != 'X' && cell != 'O')
        {
            return false;
        }
    }
    return true;
}

bool is_winner(const char player, const char[9] board) @safe @nogc nothrow pure
{
    foreach (condition; WIN_CONDITIONS)
    {
        if (board[condition[0]] == player &&
            board[condition[1]] == player &&
            board[condition[2]] == player)
        {
            return true;
        }
    }
    return false;
}

void player_turn(char player, ref char[9] board)
{
    import std.stdio : writeln, stdin, stdout;
    import std.string : format, strip;
    import std.conv : to, ConvException;

    while (true)
    {
        "Player %s's turn. Enter a number between 0 and 8.".format(player).writeln;
        board.show;
        stdout.flush;
        auto input = stdin.readln.strip;
        size_t cell;
        try
        {
            cell = input.to!size_t;
        }
        catch (ConvException e)
        {
            "Invalid input. Please enter a number.".writeln;
            continue;
        }
        if (cell < 0 || cell > 8)
        {
            "Invalid input. Please enter a number between 0 and 8.".writeln;
            continue;
        }
        if (board[cell] == 'X' || board[cell] == 'O')
        {
            "Cell already taken. Please choose another cell.".writeln;
            continue;
        }
        board[cell] = player;
        break;
    }
}

void play()
{
    import std.stdio : writeln;
    import std.string : format;

    char player = 'X';
    char[9] board = ['0', '1', '2', '3', '4', '5', '6', '7', '8'];

    while (true)
    {
        player_turn(player, board);
        if (is_winner(player, board))
        {
            "Player %s wins!".format(player).writeln;
            break;
        }
        if (board.full)
        {
            "Game Drawn!".writeln;
            break;
        }
        player = player.swap;
    }
    board.show;
}

@safe @nogc nothrow pure unittest
{
    assert('X'.swap == 'O');
}
