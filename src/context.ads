with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
package Context is

    type T_Context is private;

    -- Initialise la mémoire du contexte d'exécution
    -- Doit être appelé avant toute autre opération sur Context
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
    procedure Initialize (Context : out T_Context);

    -- Lit la valeur d'une variable dans le contexte d'exécution
    --
    -- Paramètres :
    --   Context : le contexte d'exécution
    --   VariableName : le nom de la variable à lire
    --
    -- Nécessite :
    --   Context a été initialisé
    --   VariableName existe dans le contexte
    --
    -- Assure :
    --   Vrai
    --
    -- Exemples :
    --   Voir tests
    function ReadVariable (Context : in T_Context; VariableName : in String) return Integer;

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
    procedure WriteVariable (Context : in out T_Context; VariableName : in String; Value : in Integer) with
            Pre => True, Post => ReadVariable (Context, VariableName) = Value;

    -- Libère la mémoire du contexte d'exécution
    -- Doit être appelé lorsque le contexte n'est plus utilisé
    --
    -- Paramètres :
    --   Context : le contexte d'exécution à libérer
    --
    -- Nécessite :
    --   Context a été initialisé
    --
    -- Assure :
    --   Vrai
    --
    -- Exemples :
    --   Voir tests
   procedure Destroy (Context : in out T_Context);


    -- Affiche le contenu de la liste cha�n�e du contexte d'ex�cution
    --
    -- Param�tres :
    --   Context : le contexte d'ex�cution
    --
    -- N�cessite :
    --   Context a �t� initialis�
    --
    -- Assure :
    --   Vrai
    --
    -- Exemples :
    --   Voir tests
   procedure Display(Context: in T_Context);

private

    type Node_Integer;

    type A_Node_Integer is access Node_Integer;

    type Node_Integer is record
        Key   : Unbounded_String;
        Value : Integer;
        Next  : A_Node_Integer;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation (Node_Integer, A_Node_Integer);

    type T_Context is record
        List_Integer : A_Node_Integer;
    end record;

end Context;
