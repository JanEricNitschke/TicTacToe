' Declarations
Declare Sub PlayerMove( player as String, board() as String)
Declare Sub ShowBoard( board() as String)
Declare Function SwapPlayer( player as String) as String
Declare Function BoardFull( board() as String) as Boolean
Declare Function PlayerWon( player as String, board() as String) as Boolean

' Main program

Dim player as String = "X"
Dim board(0 to 8) as String = { "0", "1", "2", "3", "4", "5", "6", "7", "8" }
Dim Shared win_conditions(0 to 7, 0 to 2) as const Integer => {_
    {0, 1, 2}, {3, 4, 5}, {6, 7, 8}, _' Rows
    {0, 3, 6}, {1, 4, 7}, {2, 5, 8}, _' Cols
    {0, 4, 8}, {2, 4, 6}             _' Diagonals
}

Do
    PlayerMove(player, board())
    If PlayerWon(player, board()) Then
        Print "Player " + player + " wins!"
        Exit Do
    End If
    If BoardFull(board()) Then
        Print "It's a draw!"
        Exit Do
    End If
    player = SwapPlayer(player)
Loop

ShowBoard(board())

' Function definitions

Sub ShowBoard( board() as String )
    Print board(0) + " | " + board(1) + " | " + board(2)
    Print "---------"
    Print board(3) + " | " + board(4) + " | " + board(5)
    Print "---------"
    Print board(6) + " | " + board(7) + " | " + board(8)
End Sub

Function SwapPlayer( player as String ) as String
    If player = "X" Then
        SwapPlayer = "O"
    Else
        SwapPlayer = "X"
    End If
End Function

Function BoardFull ( board() as String ) as Boolean
    Dim i as Integer
    For i = 0 To 8
        If board(i) = Str(i) Then
            return False
        End If
    Next
    return True
End Function

Function PlayerWon ( player as String, board() as String ) as Boolean
    Dim i as Integer
    Dim j as Integer
    For i = 0 To 7
        If board(win_conditions(i, 0)) = player And _
           board(win_conditions(i, 1)) = player And _
           board(win_conditions(i, 2)) = player Then
            return True
        End If
    Next
    return False
End Function

Sub PlayerMove( player as String, board() as String )
    Dim move as Integer
    Do
        Print "Player " + player + ", enter your move (0-8):"
        ShowBoard(board())
        Input "", move
        If move < 0 Or move > 8 Then
            Print "Spot out of range, try again."
            Continue Do
        ElseIf board(move) <> Str(move) Then
            Print "Spot already taken, try again."
            Continue Do
        End If
        board(move) = player
        Exit Do
    Loop
End Sub
