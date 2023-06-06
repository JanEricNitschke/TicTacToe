with Game;                use Game;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
procedure Tictactoe_Ada is
   Game_Board : Board       := ('1', '2', '3', '4', '5', '6', '7', '8', '9');
   Player     : Player_Char := 'X';
   function Get_Player_Yes_No (Question : String) return Boolean is
      type Answer is (y, n);
      subtype Affirmative is Answer with
          Static_Predicate => Affirmative in y;
      User_Answer : Answer;
      package Answer_IO is new Enumeration_IO (Answer);
   begin
      loop
         begin
            Put_Line (Question);
            Answer_IO.Get (User_Answer);
            if User_Answer in Affirmative then
               return True;
            end if;
            return False;
         exception
            when Data_Error =>
               Put_Line ("Invalid input.");
               Skip_Line;
         end;
      end loop;
   end Get_Player_Yes_No;

   function Get_AI_Opponent_Exists return Boolean is
   begin
      return Get_Player_Yes_No ("Play alone vs AI?[y/n]");
   end Get_AI_Opponent_Exists;

   function Get_AI_Opponent_Start return Player_Char is
   begin
      if Get_Player_Yes_No ("Should the AI make the first move?[y/n]") then
         return 'X';
      end if;
      return 'O';
   end Get_AI_Opponent_Start;

   AI_Opponent : Boolean;
   AI_Marker   : Player_Char;
   subtype Strength is Integer range 1 .. 4;
   AI_Strength : Strength := 1;

   function Get_AI_Opponent_Strength return Strength is
   begin
      Put_Line ("AI strength settings:");
      Put_Line ("1: Easy");
      Put_Line ("2: Medium");
      Put_Line ("3: Hard");
      Put_Line ("4: Impossible");
      loop
         begin
            Put_Line ("How strong should the AI be?[1-4]");
            Get (AI_Strength);
            return AI_Strength;
         exception
            when Data_Error       =>
               Put_Line
                 ("ERROR: Input must be a valid integer in range [1-4]!");
               Skip_Line;
            when Constraint_Error =>
               Put_Line
                 ("ERROR: Input must be a valid integer in range [1-4]!");
         end;
      end loop;
   end Get_AI_Opponent_Strength;

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
      Player_Turn (Player, Game_Board);
      exit when Game_Over (Player, Game_Board);
      Player := Swap_Player (Player);
   end loop;
   Show_Board (Game_Board);
end Tictactoe_Ada;
