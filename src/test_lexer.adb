with Lexer; use Lexer;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_Lexer is
    Assign_Line : constant String := "x <- 42";
    Jump_Line : constant String := "Goto L2";
    ConditionalJump_Line : constant String := "IF x Goto L2";
    Pass_Line : constant String := "NULL";
begin
    declare
        Assign_Tokens : constant T_Tokens := Lexer.Tokenize(Assign_Line);
        Jump_Tokens : constant T_Tokens := Lexer.Tokenize(Jump_Line);
        ConditionalJump_Tokens : constant T_Tokens := Lexer.Tokenize(ConditionalJump_Line);
        Pass_Tokens : constant T_Tokens := Lexer.Tokenize(Pass_Line);
    begin
        pragma Assert (Assign_Tokens.InstructionType = Lexer.Assign);
        pragma Assert (Assign_Tokens.Arguments (1) = "x");
        pragma Assert (Assign_Tokens.Arguments (2) = "42");

        pragma Assert (Jump_Tokens.InstructionType = Lexer.Jump);
        pragma Assert (Jump_Tokens.Arguments (1) = "L2");

        pragma Assert (ConditionalJump_Tokens.InstructionType = Lexer.ConditionalJump);
        pragma Assert (ConditionalJump_Tokens.Arguments (1) = "x");
        pragma Assert (ConditionalJump_Tokens.Arguments (2) = "L2");

        pragma Assert (Pass_Tokens.InstructionType = Lexer.Pass);
    end;
end Test_Lexer;
