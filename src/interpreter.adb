with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Lexer; use Lexer;

package body Interpreter is
   
   procedure Initialize(Interpreter: out T_Interpreter) is
      Program: T_Program(1..15);
   begin
      Interpreter.PC := 1;
      Interpreter.Program := new T_Program(1..15);
         
      Program(1) :=  To_Unbounded_String("n <- 10");
      Program(2) := To_Unbounded_String("i <- 1");
      Program(3) := To_Unbounded_String("Fact <- 1");
      Program(4) := To_Unbounded_String("T1 <- i < n");
      Program(5) := To_Unbounded_String("T2 <- i = n");
      Program(6) := To_Unbounded_String("T3 <- T1 OR T2");
      Program(7) := To_Unbounded_String("IF T3 GOTO 9");
      Program(8) := To_Unbounded_String("GOTO 15");
      Program(9) := To_Unbounded_String("Fact <- Fact * i");
      Program(10) := To_Unbounded_String("i <- i + 1");
      Program(11) := To_Unbounded_String("T1 <- i < n");
      Program(12) := To_Unbounded_String("T2 <- i = n");
      Program(13) := To_Unbounded_String("T3 <- T1 OR T2");
      Program(14) := To_Unbounded_String("GOTO 7");
      Program(15) := To_Unbounded_String("NULL");
      
      
      Interpreter.Program.all := Program;
   end Initialize;
   
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

  
   procedure RunNormal (FilePath : in String) is
      Interpreter : T_Interpreter;
      Program_Lines_Number: Integer;
      Current_Line: Unbounded_String;
      Current_Line_Tokens: T_Tokens;
      ReadValue: Integer;
   begin
      Initialize(Interpreter);
      
      Program_Lines_Number := Interpreter.Program.all'Length;
      
      while Interpreter.PC < Program_Lines_Number loop
         Current_Line := Interpreter.Program.all(Interpreter.PC);
         Current_Line_Tokens := Lexer.Tokenize(To_String(Current_Line));
         
         ExecuteInstruction(Interpreter, Current_Line_Tokens);
      end loop;
      
      ReadValue := ReadVariable(Interpreter.Context, "Fact");
      Put_Line(Integer'Image(ReadValue));
   end RunNormal;
   
   procedure RunDebug(FilePath : in String) is
   begin
      Put_Line(FilePath);
      return;
   end RunDebug;
   
                                   
end Interpreter;
