' Declarations
Declare Sub PlayerMove( player as String, board() as String)
Declare Sub AIMove( player as String, board() as String, strength as Integer)
Declare Sub ShowBoard( board() as String)
Declare Function SwapPlayer( player as String) as String
Declare Function BoardFull( board() as String) as Boolean
Declare Function PlayerWon( player as String, board() as String) as Boolean

' Main program

Dim X_strength as Integer = Valint(Command(1))
Dim O_strength as Integer = Valint(Command(2))
Dim player as String = "X"
Dim board(0 to 8) as String = { "0", "1", "2", "3", "4", "5", "6", "7", "8" }
Dim Shared win_conditions(0 to 7, 0 to 2) as const Integer => {_
    {0, 1, 2}, {3, 4, 5}, {6, 7, 8}, _' Rows
    {0, 3, 6}, {1, 4, 7}, {2, 5, 8}, _' Cols
    {0, 4, 8}, {2, 4, 6}             _' Diagonals
}

Print "X strength: " + Str(X_strength)
Print "O strength: " + Str(O_strength)

Enum EndState Explicit
    loss
    draw
    win
End Enum

Operator - (ByRef rhs As EndState) As EndState
    Select Case As Const rhs
    Case EndState.loss
        return EndState.win
    Case EndState.win
        return EndState.loss
    Case Else
        return EndState.draw
    End Select
End Operator

Type Move
    As Integer spot
    As EndState state
End Type

Do
    If player = "X" And X_strength > 0 Then
        AIMove(player, board(), X_strength)
    ElseIf player = "O" And O_strength > 0 Then
        AIMove(player, board(), O_strength)
    Else
        PlayerMove(player, board())
    End If

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

' Expects the passed array "emptySpots" to be empty
Sub EmptyCells( board() as String, emptySpots() as Integer )
    Dim count AS INTEGER = 0
    Dim i as Integer
    ' Iterate over the board
    For i = 0 To UBOUND(board)
        If board(i) = Str(i) Then
            Redim Preserve emptySpots(0 to count)
            emptySpots(count) = i
            count += 1
        End If
    Next
End Sub

Function RandomOpenIndex( board() as String ) as Integer
    Dim emptySpots() as Integer
    EmptyCells(board(), emptySpots())
    Dim index as Integer = Int(Rnd() * UBOUND(emptySpots))
    return emptySpots(index)
End Function

Sub RandomMove( player as String, board() as String )
    Dim random_index as Integer = RandomOpenIndex(board())
    board(random_index) = player
End Sub

Function TryWinMove( player as String, board() as String) as Integer
    Dim i as Integer
    For i = 0 To 7
        Dim done_spots as Integer = 0
        Dim open_spot as Integer = -1
        Dim j as Integer
        For j = 0 To 2
            If board(win_conditions(i, j)) = player Then
                done_spots += 1
            ElseIf board(win_conditions(i, j)) = Str(win_conditions(i, j)) Then
                open_spot = win_conditions(i, j)
            End If
        Next
        ' If there are two done spots and the open spot has changed then
        ' there is exactly one open spot and two done spots and to win we need
        ' to take that spot.
        If done_spots = 2 And open_spot <> -1 Then
            return open_spot
        End If
    Next
    return -1
End Function

Sub WinMove( player as String, board() as String )
    Dim move as Integer = TryWinMove(player, board())
    If move <> -1 Then
        board(move) = player
    Else
        RandomMove(player, board())
    End If
End Sub

Sub WinBlockMove( player as String, board() as String )
    Dim move as Integer = TryWinMove(player, board())
    If move <> -1 Then
        board(move) = player
        return
    End If
    move = TryWinMove(SwapPlayer(player), board())
    If move <> -1 Then
        board(move) = player
        return
    End If
    RandomMove(player, board())
End Sub

Function GetBestSpot( player as String, board() as String) as Move
    Dim bestMove as Move = (-1, EndState.loss)
    If PlayerWon(player, board()) Then
        bestMove.state = EndState.win
        return bestMove
    End If
    If PlayerWon(SwapPlayer(player), board()) Then
        bestMove.state = EndState.loss
        return bestMove
    End If

    Dim emptySpots() as Integer
    EmptyCells(board(), emptySpots())

    If UBOUND(emptySpots) = -1 Then
        bestMove.state = EndState.draw
        return bestMove
    End If

    If UBOUND(emptySpots) = 8 Then
        bestMove.spot = RandomOpenIndex(board())
        bestMove.state = EndState.draw
        return bestMove
    End If

    Dim i as Integer
    For i = 0 To UBOUND(emptySpots)
        board(emptySpots(i)) = player
        Dim move as Move = GetBestSpot(SwapPlayer(player), board())
        If -move.state >= bestMove.state Then
            bestMove.spot = emptySpots(i)
            bestMove.state = -move.state
        End If
        board(emptySpots(i)) = Str(emptySpots(i))
    Next
    return bestMove
End Function

Sub BestMove( player as String, board() as String )
    Dim move as Move = GetBestSpot(player, board())
    board(move.spot) = player
End Sub

Sub AIMove( player as String, board() as String, strength as Integer )
    Print "AI turn for player " + player + " with strength " + Str(strength) + "."
    ShowBoard(board())
    Select Case As Const strength
    Case 1
        RandomMove(player, board())
    Case 2
        WinMove(player, board())
    Case 3
        WinBlockMove(player, board())
    Case Else
        BestMove(player, board())
    End Select
    Sleep 1000
End Sub
