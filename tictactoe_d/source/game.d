module game;

import std.typecons : Nullable;

private immutable size_t[3][8] WIN_CONDITIONS = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6]
];

private char swap(const char player) @safe @nogc nothrow pure
{
    return player == 'X' ? 'O' : 'X';
}

private void show(const char[9] board) @safe
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

private bool full(const char[9] board) @safe @nogc nothrow pure
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

private bool is_winner(const char player, const char[9] board) @safe @nogc nothrow pure
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

private void player_turn(const char player, ref char[9] board)
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

private struct EmptySpots
{
    private size_t[9] spots;
    private size_t count;

    static EmptySpots from_board(const char[9] board) @safe @nogc nothrow pure
    {
        size_t[9] empty;
        size_t empty_count = 0;
        foreach (i, cell; board)
        {
            if (cell != 'X' && cell != 'O')
            {
                empty[empty_count] = i;
                ++empty_count;
            }
        }
        return EmptySpots(empty, empty_count);
    }

    size_t get_random_element() @safe
    {
        import std.random : uniform;

        auto random_index = uniform(0, count);
        return spots[random_index];
    }

    size_t[] empties() return @safe @nogc nothrow pure
    {
        return spots[0 .. count];
    }

    bool empty() @safe @nogc nothrow pure
    {
        return count == 0;
    }
}

private size_t ai_random_move(const char[9] board) @safe
{
    return EmptySpots.from_board(board).get_random_element();
}

private Nullable!size_t try_win_move(const char player, const char[9] board) @safe @nogc nothrow pure
{
    Nullable!size_t winning_spot;
    foreach (condition; WIN_CONDITIONS)
    {
        size_t empty_count = 0;
        size_t done_count = 0;
        size_t empty_spot;
        foreach (spot; condition)
        {
            if (board[spot] == player)
            {
                done_count++;
            }
            else if (board[spot] != player.swap)
            {
                ++empty_count;
                empty_spot = spot;
            }
        }
        if (empty_count == 1 && done_count == 2)
        {
            winning_spot = empty_spot;
            break;
        }
    }
    return winning_spot;
}

private size_t ai_win_move(const char player, const char[9] board) @safe
{
    auto winning_spot = try_win_move(player, board);
    if (!winning_spot.isNull)
    {
        return winning_spot.get;
    }
    return ai_random_move(board);
}

private size_t ai_win_block_move(const char player, const char[9] board) @safe
{
    auto winning_move = try_win_move(player, board);
    if (!winning_move.isNull)
    {
        return winning_move.get;
    }
    auto blocking_move = try_win_move(player.swap, board);
    if (!blocking_move.isNull)
    {
        return blocking_move.get;
    }
    return ai_random_move(board);
}

enum Score
{
    WIN = 1,
    DRAW = 0,
    LOSE = -1
}

private struct Move
{
    size_t spot;
    Score score;
}

private Move minmax(const char player, char[9] board) @safe pure
{
    import std.conv : to;

    if (is_winner(player, board))
    {
        return Move(0, Score.WIN);
    }
    if (is_winner(player.swap, board))
    {
        return Move(0, Score.LOSE);
    }
    auto empty_spots = EmptySpots.from_board(board);
    if (empty_spots.empty)
    {
        return Move(0, Score.DRAW);
    }
    auto best_move = Move(0, Score.LOSE);
    foreach (spot; empty_spots.empties)
    {
        board[spot] = player;
        auto score = -minmax(player.swap, board).score;
        board[spot] = (spot + '0').to!char;
        if (score >= best_move.score)
        {
            best_move = Move(spot, score);
        }
    }
    return best_move;
}

private size_t ai_best_move(const char player, const char[9] board) @safe pure
{
    return minmax(player, board).spot;
}

private void ai_turn(const char player, ref char[9] board, const int strength)
{
    import std.stdio : writeln, stdout;
    import std.string : format;
    import core.thread : Thread, seconds;

    "AI turns as player %s with strength %s".format(player, strength).writeln;
    board.show;
    stdout.flush;
    size_t move;
    switch (strength)
    {
    case 1:
        move = ai_random_move(board);
        break;
    case 2:
        move = ai_win_move(player, board);
        break;
    case 3:
        move = ai_win_block_move(player, board);
        break;
    default:
        move = ai_best_move(player, board);
        break;
    }
    board[move] = player;
    Thread.sleep(1.seconds);
}

void play(const int xStrength, const int oStrength)
{
    import std.stdio : writeln;
    import std.string : format;

    char player = 'X';
    char[9] board = ['0', '1', '2', '3', '4', '5', '6', '7', '8'];

    while (true)
    {
        if (player == 'X' && xStrength)
        {
            ai_turn(player, board, xStrength);
        }
        else if (player == 'O' && oStrength)
        {
            ai_turn(player, board, oStrength);
        }
        else
        {
            player_turn(player, board);

        }
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

@safe pure unittest
{
    assert('X'.swap == 'O');
    assert(minmax('O', ['X', '1', '2', '3', '4', '5', '6', '7', '8']) == Move(4, Score.DRAW));
}
