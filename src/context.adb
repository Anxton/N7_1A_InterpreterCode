with Ada.Text_IO; use Ada.Text_IO;

package body Context is

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
    procedure Initialize (Context : out T_Context) is
    begin
        Context.List_Integer := null;
    end Initialize;

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
    function ReadVariable (Context : in T_Context; VariableName : in String) return Integer is
        Node : A_Node_Integer := Context.List_Integer;
    begin
        while Node /= null loop
            if Node.Key = To_Unbounded_String (VariableName) then
                return Node.Value;
            end if;
            Node := Node.Next;
        end loop;
        raise Program_Error;
    end ReadVariable;

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
    procedure WriteVariable (Context : in out T_Context; VariableName : in String; Value : in Integer) is
        Node : A_Node_Integer := Context.List_Integer;
    begin
        while Node /= null loop
            if Node.Key = To_Unbounded_String (VariableName) then
                Node.Value := Value;
                return;
            end if;
            Node := Node.Next;
        end loop;
        Node := new Node_Integer'(Key => To_Unbounded_String (VariableName), Value => Value, Next => Context.List_Integer);
        Context.List_Integer := Node;
    end WriteVariable;

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
    procedure Destroy (Context : in out T_Context) is
        Node : A_Node_Integer := Context.List_Integer;
    begin
        while Node /= null loop
            Context.List_Integer := Node.Next;
            Free (Node);
            Node := Context.List_Integer;
        end loop;
   end Destroy;


   procedure Display(Context: in T_Context) is
      Node: A_Node_Integer := Context.List_Integer;
      NodeNumber: Integer := 1;
   begin
      while Node /= null loop
         Put_Line(Integer'Image(NodeNumber) & ':' & To_String(Node.Key) & " -> " & Integer'Image(ReadVariable(Context, To_String(Node.Key))));
         Node := Node.Next;
         NodeNumber := NodeNumber + 1;
   end loop;
   end Display;

end Context;
