with stringutils; use stringutils;
package body Lexer is   
    -- Tokenize une ligne de code en un token
    --
    -- Paramètres :
    --   Line : la ligne de code à tokeniser
    --
    -- Nécessite :
    --   Line est une ligne de code valide
    --
    -- Assure :
    --   Vrai
    --
    -- Exemples :
    --   Voir tests
    function Tokenize(Line: in String) return T_Tokens is
        Result : T_Tokens;
        Strings : constant Split_String := Split (Line);
    begin
        if Strings (1) = "GOTO" then
            Result.InstructionType := Jump;
            Result.Arguments (1) := Strings (2);
        elsif Strings (1) = "IF" then
            Result.InstructionType := ConditionalJump;
            Result.Arguments (1) := Strings (2);
            Result.Arguments (2) := Strings (4);
        elsif Strings (1) = "NULL" then
            Result.InstructionType := Pass;
        elsif Strings'Length = 3 then
            Result.InstructionType := Assign;
            Result.Arguments (1) := Strings (1);
            Result.Arguments (2) := Strings (3);
        else
            Result.InstructionType := AssignEval;
            Result.Arguments (1) := Strings (1);
            Result.Arguments (2) := Strings (3);
            Result.Arguments (3) := Strings (4);
            Result.Arguments (4) := Strings (5);
        end if;
        return Result;
    end Tokenize;
end Lexer;
