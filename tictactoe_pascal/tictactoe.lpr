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


type

  { TicTacToePascal }

  TicTacToePascal = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TicTacToePascal }

  procedure TicTacToePascal.DoRun;
  var
    ErrorMsg: string;
    Board: TBoard;
    Player: char;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }
    Board := InitializeBoard;
    Player := 'X';

    repeat
      DisplayBoard(Board);
      PlayerTurn(Board, Player);

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
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TicTacToePascal;
begin
  Application := TicTacToePascal.Create(nil);
  Application.Title := 'TicTacToePascal';
  Application.Run;
  Application.Free;
end.
