with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Lexer; use Lexer;
with ProgramLoader; use ProgramLoader;

package body Interpreter is
   
   procedure Initialize(Interpreter: out T_Interpreter) is
   begin
      Interpreter.PC := 1;
   end Initialize;
   
   -- Check if a given string represents a numeric value
   function Is_Numeric(Item: String) return Boolean is
      Dummy: Integer;
   begin
      begin
         Dummy := Integer'Value(Item);
         return True;
      exception
         when others =>
            return False;
      end;
   end Is_Numeric;
   
   -- Get the value of an operand, which can be either a numeric constant or a variable
   function GetOperandValue(Interpreter: T_Interpreter; Operand: String) return Integer is
   begin
      if Is_Numeric(Operand) then
         return Integer'Value(Operand);
      else
         return ReadVariable(Interpreter.Context, Operand);
      end if;
   end GetOperandValue;
   
   procedure PerformConditionalJump(Interpreter: in out T_Interpreter; Arguments: in T_Arguments) is
      ConditionVariable: Integer;
   begin
      ConditionVariable := ReadVariable(Interpreter.Context, To_String(Arguments(1)));
      if ConditionVariable = 1 then
         Interpreter.PC := Integer'Value(To_String(Arguments(2)));
      else
         Interpreter.PC := Interpreter.PC + 1;
      end if;
   end PerformConditionalJump;

   procedure PerformJump(Interpreter: in out T_Interpreter; Arguments: in T_Arguments) is
   begin
      Interpreter.PC := Integer'Value(To_String(Arguments(1)));
   end PerformJump;

   procedure PerformAssign(Interpreter: in out T_Interpreter; Arguments: in T_Arguments) is
   begin
      WriteVariable(Interpreter.Context, To_String(Arguments(1)), GetOperandValue(Interpreter, To_String(Arguments(2))));
      
      Interpreter.PC := Interpreter.PC + 1;
   end PerformAssign;
    
      
   procedure PerformAssignEval(Interpreter: in out T_Interpreter; Arguments: in T_Arguments) is
      FirstOperand: constant Integer := GetOperandValue(Interpreter, To_String(Arguments(2)));
      Operator: constant Unbounded_String := Arguments(3);
      SecondOperand: constant Integer := GetOperandValue(Interpreter, To_String(Arguments(4)));
   begin
      if Operator = "OR" then
         -- Here the operands need to be converted to Boolean to apply logical OR. 
         -- We then check if the operands are greater than 0, if this is the case, they will be replaced by True, otherwise by False.
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), Boolean'Pos((FirstOperand > 0) OR (SecondOperand > 0)));
      elsif Operator = "AND" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), Boolean'Pos((FirstOperand > 0) AND (SecondOperand > 0)));
      elsif Operator = "<" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), Boolean'Pos(FirstOperand < SecondOperand));
      elsif Operator = ">" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), Boolean'Pos(FirstOperand > SecondOperand));
      elsif Operator = "=" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), Boolean'Pos(FirstOperand = SecondOperand));
      elsif Operator = "+" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), FirstOperand + SecondOperand);
      elsif Operator = "-" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), FirstOperand - SecondOperand);
      elsif Operator = "*" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), FirstOperand * SecondOperand);
      elsif Operator = "/" then
         WriteVariable(Interpreter.Context, To_String(Arguments(1)), FirstOperand / SecondOperand);
      end if;

      Interpreter.PC := Interpreter.PC + 1;
   end PerformAssignEval;
   
   procedure ExecuteInstruction(Interpreter: in out T_Interpreter; Tokens: in T_Tokens) is
   begin
     
      case Tokens.InstructionType is
         when ConditionalJump => PerformConditionalJump(Interpreter, Tokens.Arguments);
         when Jump => PerformJump(Interpreter, Tokens.Arguments);
         when Pass => Interpreter.PC := Interpreter.PC + 1;
         when Assign => PerformAssign(Interpreter, Tokens.Arguments);
         when AssignEval => PerformAssignEval(Interpreter, Tokens.Arguments);
      end case;
   end ExecuteInstruction;

   -- Lance l'interpréteur en mode normal
    --
    -- Paramètres :
    --   FilePath : le chemin du fichier source à interpréter
    --
    --  Nécessite :
    --    FilePath est un chemin valide
    --    le fichier source est valide
    --
    --  Assure :
    --    Vrai
    --
    -- Exemples :
    --   Voir tests
   procedure RunNormal (FilePath : in String) is
      Interpreter : T_Interpreter;
      Program: constant T_Program := LoadFile (FilePath);
      Program_Lines_Number: Integer;
      Current_Line: Unbounded_String;
      Current_Line_Tokens: T_Tokens;
   begin
      Initialize(Interpreter);
      
      Program_Lines_Number := Program'Length;
      
      while Interpreter.PC < Program_Lines_Number loop
         Current_Line := Program(Interpreter.PC);
         Current_Line_Tokens := Lexer.Tokenize(To_String(Current_Line));
         
         ExecuteInstruction(Interpreter, Current_Line_Tokens);
      end loop;
     
      Destroy(Interpreter.Context);
   end RunNormal;
   
   -- Lancer l'interpréteur en mode debug
    --
    -- Paramètres :
    --   FilePath : le chemin du fichier source à interpréter
    --
    --  Nécessite :
    --    FilePath est un chemin valide
    --    le fichier source est valide
    --
    --  Assure :
    --    Vrai
    --
    -- Exemples :
    --   Voir tests
   procedure RunDebug(FilePath : in String) is
      Interpreter : T_Interpreter;
      Program: constant T_Program := LoadFile (FilePath);
      Program_Lines_Number: Integer;
      Current_Line: Unbounded_String;
      Current_Line_Tokens: T_Tokens;
   begin
      Put_Line("# DEBUG MODE #");
      
      Initialize(Interpreter);
      
      Program_Lines_Number := Program'Length;
      
      while Interpreter.PC <= Program_Lines_Number loop
         Put_Line("Program Counter:" & Integer'Image(Interpreter.PC + 1));
         Display(Interpreter.Context);
         
         Current_Line := Program(Interpreter.PC);
         Current_Line_Tokens := Lexer.Tokenize(To_String(Current_Line));
         
         ExecuteInstruction(Interpreter, Current_Line_Tokens);         
         
         Put_Line("-----");
      end loop;
     
      Destroy(Interpreter.Context);
      
   end RunDebug;
   
                                   
end Interpreter;
