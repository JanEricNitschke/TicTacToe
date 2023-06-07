with Game;        use Game;
with Ada.Text_IO; use Ada.Text_IO;
procedure Tictactoe_Ada is
   Game_Board : Board       := ('1', '2', '3', '4', '5', '6', '7', '8', '9');
   Player     : Player_Char := 'X';

   AI_Opponent : Boolean;
   AI_Marker   : Player_Char;
   AI_Strength : Strength;

begin
   AI_Opponent := Get_AI_Opponent_Exists;
   if AI_Opponent then
      AI_Marker   := Get_AI_Opponent_Start;
      AI_Strength := Get_AI_Opponent_Strength;
   end if;
   Put_Line ("Got " & Boolean'Image (AI_Opponent));
   Put_Line ("Got " & Player_Char'Image (AI_Marker));
   Put_Line ("Got " & Strength'Image (AI_Strength));
   loop
      if AI_Opponent and then Player = AI_Marker then
         AI_Turn (Player, Game_Board, AI_Strength);
      else
         Player_Turn (Player, Game_Board);
      end if;
      exit when Game_Over (Player, Game_Board);
      Player := Swap_Player (Player);
   end loop;
   Show_Board (Game_Board);
end Tictactoe_Ada;
