with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ProgramLoader is
   type T_Program is array (Natural range <>) of Unbounded_String;
   type P_Program is access T_Program;

    -- Charge un fichier dans un tableau de chaînes de caractères
    --
    -- Paramètres :
    --      Source : chemin du fichier à charger
    --
    -- Nécessite :
    --   Source est un fichier existant
    --
    -- Assure :
    --   LoadFile renvoie un tableau de chaînes de caractères contenant
    --   le contenu du fichier Source
    --
    -- Exemples :
    --   Voir tests
    function LoadFile (Source: String) return T_Program;
end ProgramLoader;
