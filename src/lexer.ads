with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Lexer is
    type T_InstructionType is (Assign, Jump, ConditionalJump, Pass);
    type T_Arguments is array (1 .. 4) of Unbounded_String;
   
    type T_Tokens is record
        InstructionType: T_InstructionType;
        Arguments: T_Arguments;
    end record;
   
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
    function Tokenize(Line: in String) return T_Tokens;
end Lexer;
