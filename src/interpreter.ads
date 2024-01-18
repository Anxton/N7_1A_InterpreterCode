with Context; use Context;
with ProgramLoader; use ProgramLoader;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Interpreter is

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
    procedure RunNormal (FilePath : in String);

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
    procedure RunDebug (FilePath : in String);

private
    type P_Program is access T_Program;

    type T_Interpreter is record
        PC: Integer;
        --  Invariant: 1 <= PC <= A_Program'Length
        Context: T_Context;
        Program: P_Program;
    end record;
end Interpreter;
