with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package Context is

    type T_Context is private;

    -- Initialise la mémoire du contexte d'exécution
    -- Doit être appelé avant toute autre opération sur context
    --
    -- Paramètres :
    --   Context : le contexte d'exécution à initialiser
    --
    -- Nécessite :
    --   Vrai
    --
    -- Assure :
    --   Vrai
    --
    -- Exemples :
    --   Voir tests
    procedure Initialize (context : out T_Context);

    -- Lit la valeur d'une variable dans le contexte d'exécution
    --
    -- Paramètres :
    --   Context : le contexte d'exécution
    --   VariableName : le nom de la variable à lire
    --
    -- Nécessite :
    --   Context a été initialisé
    --
    -- Assure :
    --   Vrai
    --
    -- Exemples :
    --   Voir tests
    function ReadVariable (Context : in T_Context; VariableName : in String) return Integer with
       Pre => True, Post => True;

    -- Ecrit la valeur d'une variable dans le contexte d'exécution
    --
    -- Paramètres :
    --   Context : le contexte d'exécution
    --   VariableName : le nom de la variable à écrire
    --   Value : la valeur à écrire
    --
    -- Nécessite :
    --   Context a été initialisé
    --
    -- Assure :
    --   ReadVariable (Context, VariableName) = value
    --
    -- Exemples :
    --   Voir tests
    procedure WriteVariable (Context : out T_Context; VariableName : in String; Value : in Integer) with
       Pre => True, Post => ReadVariable (Context, VariableName) = Value;

private
    type LinkedList_Integer;

    type A_LinkedList_Integer is access LinkedList_Integer;

    type LinkedList_Integer is record
        Key   : Unbounded_String;
        Value : Integer;
        Next  : A_LinkedList_Integer;
    end record;

    type T_Context is record
        List_Integer : A_LinkedList_Integer;
    end record;

end Context;
