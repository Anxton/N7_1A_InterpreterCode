with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package StringUtils is

    type Split_String is array (Natural range <>) of Unbounded_String;

    -- Sépare une ligne de code en 6 chaînes de caractères
    -- Paramètres :
    --   Line : ligne de code à séparer
    -- Résultat :
    --   Tableau de 6 chaînes de caractères
    -- Nécessite :
    --   Line est une ligne de code valide
    -- Assure :
    --   Vrai
    -- Exemple :
    --   Voir tests
    function Split (Line : in String) return Split_String with
            Pre => Line /= "",
            Post => Split'Result (1) /= "";

end StringUtils;
