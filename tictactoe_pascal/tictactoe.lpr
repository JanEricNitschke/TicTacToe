program tictactoe;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

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
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  WriteLn('Hello World');
  // stop program loop
  Terminate;
end;

constructor TicTacToePascal.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
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
  Application:=TicTacToePascal.Create(nil);
  Application.Title:='TicTacToePascal';
  Application.Run;
  Application.Free;
end.
