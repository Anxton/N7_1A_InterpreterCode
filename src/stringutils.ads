with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package StringUtils is

    type Split_String is array (Natural range <>) of Unbounded_String;

    -- Sépare une chaîne de caractères en une liste de chaînes de caractères par l'espace
    -- Paramètres :
    --   Line : Chaîne de caractères à séparer
    -- Résultat :
    --   Tableau plein de chaînes de caractères
    -- Nécessite :
    --   Vrai
    -- Assure :
    --   Vrai
    -- Exemple :
    --   Voir tests
    function Split (Line : in String) return Split_String;

end StringUtils;
