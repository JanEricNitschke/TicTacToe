program tictactoe;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp { you can add units after this };

type
  TBoard = array[0..8] of char;

type
  TIntegerArray = array of integer;

type
  TBestMove = record
    Score: integer;
    Spot: integer;
  end;

  function InitializeBoard: TBoard;
  var
    i: integer;
    Board: TBoard;
  begin
    for i := 0 to 8 do
      Board[i] := Chr(i + Ord('0'));
    Result := Board;
  end;

  procedure DisplayBoard(const Board: TBoard);
  begin
    WriteLn(Board[0], ' | ', Board[1], ' | ', Board[2]);
    WriteLn('--+---+--');
    WriteLn(Board[3], ' | ', Board[4], ' | ', Board[5]);
    WriteLn('--+---+--');
    WriteLn(Board[6], ' | ', Board[7], ' | ', Board[8]);
  end;

  function IsValidMove(Board: TBoard; Move: integer): boolean;
  begin
    Result := (Move >= 0) and (Move <= 8) and (Board[Move] <> 'X') and
      (Board[Move] <> 'O');
  end;

  procedure PlayerTurn(var Board: TBoard; Player: char);
  var
    Move: integer;
    Valid: boolean;
  begin
    repeat
      WriteLn('Player ', Player, ', enter your move (0-8): ');
      ReadLn(Move);
      Valid := IsValidMove(Board, Move);
      if not Valid then
        WriteLn('Invalid move. Please try again.');
    until Valid;

    Board[Move] := Player;
  end;

  function IsWinner(Board: TBoard; Player: char): boolean;
  const
    WinConditions: array[0..7, 0..2] of integer = (
      (0, 1, 2), (3, 4, 5), (6, 7, 8),  // rows
      (0, 3, 6), (1, 4, 7), (2, 5, 8),  // columns
      (0, 4, 8), (2, 4, 6)              // diagonals
      );
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to 7 do
    begin
      if (Board[WinConditions[i, 0]] = Player) and
        (Board[WinConditions[i, 1]] = Player) and
        (Board[WinConditions[i, 2]] = Player) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  function IsDraw(Board: TBoard): boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 0 to 8 do
    begin
      if (Board[i] <> 'X') and (Board[i] <> 'O') then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;

  function SwapPlayer(CurrentPlayer: char): char;
  begin
    if CurrentPlayer = 'X' then
      Result := 'O'
    else
      Result := 'X';
  end;

  function GetEmptySpots(Board: TBoard): TIntegerArray;
  var
    i: integer;
    EmptySpots: array of integer = nil;
  begin
    SetLength(EmptySpots, 0);
    for i := 0 to 8 do
    begin
      if (Board[i] <> 'X') and (Board[i] <> 'O') then
      begin
        SetLength(EmptySpots, Length(EmptySpots) + 1);
        EmptySpots[High(EmptySpots)] := i;
      end;
    end;
    Result := EmptySpots;
  end;

  function RandomMove(Board: TBoard): integer;
  var
    EmptySpots: array of integer;
  begin
    EmptySpots := GetEmptySpots(Board);
    Result := EmptySpots[Random(Length(EmptySpots))];
  end;

  function FindWinningMove(Board: TBoard; Player: char): integer;
  var
    i: integer;
    IsWon: boolean;
  begin
    for i := 0 to 8 do
    begin
      if (Board[i] <> 'X') and (Board[i] <> 'O') then
      begin
        // Try placing the player symbol in the spot
        Board[i] := Player;
        isWon := IsWinner(Board, Player);
        Board[i] := Chr(i + Ord('0')); // Undo the move
        if isWon then
          Exit(i); // Return the winning position
      end;
    end;
    Result := -1; // No winning move found
  end;

  function FindBlockingMove(Board: TBoard; Player: char): integer;
  var
    Opponent: char;
  begin
    Opponent := SwapPlayer(Player);
    Result := FindWinningMove(Board, Opponent); // Block the opponent's winning move
  end;

  function WinningMove(Board: TBoard; Player: char): integer;
  begin
    Result := FindWinningMove(Board, Player); // AI tries to win
    if Result = -1 then
      Result := RandomMove(Board); // No winning move, pick random
  end;

  function WinningBlockingMove(Board: TBoard; Player: char): integer;
  begin
    Result := FindWinningMove(Board, Player); // AI tries to win
    if Result = -1 then
      Result := FindBlockingMove(Board, Player); // No winning move, try to block
    if Result = -1 then
      Result := RandomMove(Board); // No blocking move, pick random
  end;

  function Minimax(var Board: TBoard; Player: char): TBestMove;
  var
    EmptySpots: array of integer;
    Spot: integer;
    Score: integer;
    i: integer;
    Opponent: char;
  begin
    if IsWinner(Board, Player) then
    begin
      Result.Score := 1;  // Winning move
      Exit;
    end;

    Opponent := SwapPlayer(Player);

    if IsWinner(Board, Opponent) then
    begin
      Result.Score := -1; // Opponent wins, bad move
      Exit;
    end;

    EmptySpots := GetEmptySpots(Board);
    if Length(EmptySpots) = 0 then
    begin
      Result.Score := 0; // Draw
      Exit;
    end;

    if Length(EmptySpots) = 9 then
    begin
      Result.Score := 0;
      Result.Spot := RandomMove(Board);
      Exit;
    end;

    Result.Score := -MaxInt;
    for i := 0 to High(EmptySpots) do
    begin
      Spot := EmptySpots[i];
      Board[Spot] := Player;
      Score := -Minimax(Board, SwapPlayer(Player)).Score;
      Board[Spot] := Chr(Spot + Ord('0')); // Reset spot
      if Score > Result.Score then
      begin
        Result.Score := Score;
        Result.Spot := Spot;
      end;
    end;
  end;

  function AIBestMove(var Board: TBoard; Player: char): integer;
  begin
    Result := Minimax(Board, Player).Spot;
  end;

  procedure AITurn(var Board: TBoard; Player: char; Strength: integer);
  var
    Move: integer;
  begin
    WriteLn('AI is thinking for player ', Player, ' with strength ', Strength, '...');

    case Strength of
      1: Move := RandomMove(Board);
      2: begin
        Move := WinningMove(Board, Player);
      end;
      3: begin
        Move := WinningBlockingMove(Board, Player);
      end;
      else
        Move := AIBestMove(Board, Player);
    end;

    Board[Move] := Player;
    Sleep(1000);
  end;

type

  { TicTacToePascal }

  TicTacToePascal = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function GetAIStrength(ShortOption: char; LongOption: string): integer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TicTacToePascal }


  function TicTacToePascal.GetAIStrength(ShortOption: char; LongOption: string): integer;
  var
    OptionValue: string;
    Strength: integer;
  begin
    OptionValue := GetOptionValue(ShortOption, LongOption);
    if (OptionValue = '') or not TryStrToInt(OptionValue, Strength) then
      Strength := 0; // Default AI strength is 0 (human player)
    Result := Strength;
  end;

  procedure TicTacToePascal.DoRun;
  var
    Board: TBoard;
    Player: char;
    XStrength, OStrength: integer;
  begin
    Randomize;
    XStrength := GetAIStrength('x', 'x-strength');
    OStrength := GetAIStrength('o', 'o-strength');

    WriteLn('AI Strength for X: ', XStrength);
    WriteLn('AI Strength for O: ', OStrength);

    Board := InitializeBoard;
    Player := 'X';

    repeat
      DisplayBoard(Board);
      if (Player = 'X') and (XStrength > 0) then
        AITurn(Board, Player, XStrength)  // AI turn for player X
      else if (Player = 'O') and (OStrength > 0) then
        AITurn(Board, Player, OStrength)  // AI turn for player O
      else
        PlayerTurn(Board, Player);  // Human player turn

      if IsWinner(Board, Player) then
      begin
        DisplayBoard(Board);
        WriteLn('Player ', Player, ' wins!');
        Break;
      end
      else if IsDraw(Board) then
      begin
        DisplayBoard(Board);
        WriteLn('The game is a draw!');
        Break;
      end;

      Player := SwapPlayer(Player);

    until False;
    // stop program loop
    Terminate;
  end;

  constructor TicTacToePascal.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TicTacToePascal.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TicTacToePascal.WriteHelp;
  begin
    writeln('Usage: ', ExeName, ' -x=<X AI Strength> -o=<O AI Strength>');
  end;


var
  Application: TicTacToePascal;
begin
  Application := TicTacToePascal.Create(nil);
  Application.Title := 'TicTacToePascal';
  Application.Run;
  Application.Free;
end.
