with Context; use Context;

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

    type T_Interpreter is record
        PC: Integer;
        --  Invariant: 1 <= PC <= A_Program'Length
        Context: T_Context;
    end record;
end Interpreter;
