namespace Tictactoe.Lib

type AIStrength =
    | HUMAN = 0
    | EASY = 1
    | MEDIUM = 2
    | HARD = 3
    | IMPOSSIBLE = 4

type Score =
    | WIN = 1
    | TIE = 0
    | LOSE = -1

type Move = { Spot: int; Score: Score }

type Game(xStrength: AIStrength, oStrength: AIStrength) =

    let mutable board = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8' |]

    let winConditions =
        [| [| 0; 1; 2 |]
           [| 3; 4; 5 |]
           [| 6; 7; 8 |] // rows
           [| 0; 3; 6 |]
           [| 1; 4; 7 |]
           [| 2; 5; 8 |] // columns
           [| 0; 4; 8 |]
           [| 2; 4; 6 |] |] // diagonals

    let swapPlayer player = if player = 'X' then 'O' else 'X'

    let showBoard () =
        printfn "%c | %c | %c" board.[0] board.[1] board.[2]
        printfn "---------"
        printfn "%c | %c | %c" board.[3] board.[4] board.[5]
        printfn "---------"
        printfn "%c | %c | %c" board.[6] board.[7] board.[8]

    let isWinner player =
        winConditions
        |> Array.exists (fun condition -> condition |> Array.forall (fun i -> board.[i] = player))

    let isBoardFull () =
        board |> Array.forall (fun c -> c = 'X' || c = 'O')

    let emptyCells () =
        board
        |> Array.mapi (fun i c -> if c <> 'X' && c <> 'O' then Some i else None)
        |> Array.choose id

    let randomMove () =
        let empty = emptyCells ()
        let rand = System.Random().Next(empty.Length)
        empty.[rand]

    let tryWinningMove player =
        winConditions
        |> Array.tryPick (fun condition ->
            let emptyCells =
                condition |> Array.filter (fun i -> board.[i] <> 'X' && board.[i] <> 'O')

            if
                emptyCells.Length = 1
                && condition
                   |> Array.countBy (fun i -> board.[i] = player)
                   |> Array.exists (fun (p, c) -> p && c = 2)
            then
                Some emptyCells.[0]
            else
                None)

    let winningMove player =
        tryWinningMove player |> Option.defaultValue (randomMove ())

    let winningBlockingMove player =
        tryWinningMove player
        |> Option.defaultValue (tryWinningMove (swapPlayer player) |> Option.defaultValue (randomMove ()))

    let negateScore score =
        match score with
        | Score.WIN -> Score.LOSE
        | Score.LOSE -> Score.WIN
        | _ -> score

    let rec bestMove player =
        let mutable optimalMove = { Spot = -1; Score = Score.LOSE }

        if isWinner player then
            { Spot = -1; Score = Score.WIN }
        elif isWinner (swapPlayer player) then
            { Spot = -1; Score = Score.LOSE }
        elif emptyCells().Length = 0 then
            { Spot = -1; Score = Score.TIE }
        else
            let emptyCells = emptyCells ()

            for cell in emptyCells do
                board.[cell] <- player
                let score = negateScore (bestMove (swapPlayer player)).Score
                board.[cell] <- char (cell + int '0')

                if score >= optimalMove.Score then
                    optimalMove <- { Spot = cell; Score = score }

            optimalMove

    let minmaxMove player = (bestMove player).Spot

    let aiTurn player strength =
        printfn "AI turn as player %c with strength %A" player strength
        showBoard ()

        let move =
            match strength with
            | AIStrength.HUMAN -> failwith "AIStrength.HUMAN should not be used for AI turn."
            | AIStrength.EASY -> randomMove ()
            | AIStrength.MEDIUM -> winningMove player
            | AIStrength.HARD -> winningBlockingMove player
            | AIStrength.IMPOSSIBLE -> minmaxMove player
            | _ -> failwith "Invalid AI strength"

        board.[move] <- player
        System.Threading.Thread.Sleep(1000)

    member this.PlayGame() =
        let mutable currentPlayer = 'X'

        while true do
            if currentPlayer = 'X' && xStrength <> AIStrength.HUMAN then
                aiTurn currentPlayer xStrength
            elif currentPlayer = 'O' && oStrength <> AIStrength.HUMAN then
                aiTurn currentPlayer oStrength
            else
                // Handle player input for the move
                let mutable validMove = false

                while not validMove do
                    printfn "Player %c, enter your move (0-8): " currentPlayer
                    showBoard ()
                    let input = System.Console.ReadLine()

                    match System.Int32.TryParse(input) with
                    | (true, move) when move >= 0 && move <= 8 && board.[move] <> 'X' && board.[move] <> 'O' ->
                        board.[move] <- currentPlayer
                        validMove <- true
                    | _ -> printfn "Invalid move. Try again."

            if isWinner currentPlayer then
                printfn "Player %c wins!" currentPlayer
                break
            elif isBoardFull () then
                printfn "It's a tie!"
                break

            currentPlayer <- swapPlayer currentPlayer

        showBoard ()
