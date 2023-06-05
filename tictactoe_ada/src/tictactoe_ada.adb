with Game; use Game;

procedure Tictactoe_Ada is
   Game_Board : Board       := ('1', '2', '3', '4', '5', '6', '7', '8', '9');
   Player     : Player_Char := 'X';
begin
   loop
      Player_Turn (Player, Game_Board);
      exit when Game_Over (Player, Game_Board);
      Player := Swap_Player (Player);
   end loop;
   Show_Board (Game_Board);
end Tictactoe_Ada;
