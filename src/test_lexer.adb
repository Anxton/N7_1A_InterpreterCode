with Lexer; use Lexer;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_Lexer is
    Assign_Line : constant String := "x <- 42";
    AssignEval_Line : constant String := "y <- 2 * x";
    Jump_Line : constant String := "GOTO 15";
    ConditionalJump_Line : constant String := "IF x GOTO 2";
    Pass_Line : constant String := "NULL";
begin
    declare
        Assign_Tokens : constant T_Tokens := Lexer.Tokenize(Assign_Line);
        AssignEval_Tokens : constant T_Tokens := Lexer.Tokenize(AssignEval_Line);
        Jump_Tokens : constant T_Tokens := Lexer.Tokenize(Jump_Line);
        ConditionalJump_Tokens : constant T_Tokens := Lexer.Tokenize(ConditionalJump_Line);
        Pass_Tokens : constant T_Tokens := Lexer.Tokenize(Pass_Line);
    begin
        pragma Assert (Assign_Tokens.InstructionType = Lexer.Assign);
        pragma Assert (Assign_Tokens.Arguments (1) = "x");
        pragma Assert (Assign_Tokens.Arguments (2) = "42");

        pragma Assert (AssignEval_Tokens.InstructionType = Lexer.AssignEval);
        pragma Assert (AssignEval_Tokens.Arguments (1) = "y");
        pragma Assert (AssignEval_Tokens.Arguments (2) = "2");
        pragma Assert (AssignEval_Tokens.Arguments (3) = "*");
        pragma Assert (AssignEval_Tokens.Arguments (4) = "x");

        pragma Assert (Jump_Tokens.InstructionType = Lexer.Jump);
        pragma Assert (Jump_Tokens.Arguments (1) = "15");

        pragma Assert (ConditionalJump_Tokens.InstructionType = Lexer.ConditionalJump);
        pragma Assert (ConditionalJump_Tokens.Arguments (1) = "x");
        pragma Assert (ConditionalJump_Tokens.Arguments (2) = "2");

        pragma Assert (Pass_Tokens.InstructionType = Lexer.Pass);
    end;
end Test_Lexer;
