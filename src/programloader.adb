with ada.strings.unbounded.Text_IO; use ada.strings.unbounded.Text_IO;
with StringUtils;                   use StringUtils;
with Context;                       use Context;
with Ada.Text_IO;                   use Ada.Text_IO;

package body ProgramLoader is

    -- Filtrer les lignes de programme
    --  On ne garde que les lignes qui ne sont pas des commentaires
    --  et qui ne sont pas vides
    --  et qui ne sont pas des mots clés
    --  et qui ne sont pas des declarations de variables
    function IsProgramLine (ProgramLine : Unbounded_String) return Boolean is
        Split_Line : constant Split_String := Split (To_String (ProgramLine));
    begin
        if Split_Line (1) /= "--" and then Split_Line (1) /= ""
           and then Split_Line (1) /= "Programme"
           and then Split_Line (1) /= "Début" and then Split_Line (1) /= "Fin"
           and then Split_Line (Split_Line'Length-1) /= ":"
        then
            return True;
        else
            return False;
        end if;
    end IsProgramLine;

    function LoadFile (Source : String) return T_Program is
        File           : File_Type;
        ProgramLine    : Unbounded_String;
        Program_Length : Integer := 0;
    begin
        -- On compte le nombre de lignes du programme
        open (File, in_file, Source);
        while not end_of_file (File) loop
            get_line (File, ProgramLine);

            if IsProgramLine (ProgramLine) then
                Program_Length := Program_Length + 1;
            end if;

        end loop;
        close (File);

        -- On charge le programme dans un tableau de chaînes de caractères
        declare
            Program : T_Program (1 .. Program_Length);
            I       : Integer := 1;
            Context : T_Context;
        begin
            open (File, in_file, Source);
            while I <= Program_Length loop
                get_line (File, ProgramLine);

                if IsProgramLine (ProgramLine) then
                    Program (I) := ProgramLine;
                    I           := I + 1;
                end if;

            end loop;
            close (File);

            -- Retenir les labels et les supprimer du programme
            Initialize (Context);
            for I in Program'Range loop
                declare
                    Split_Line : constant Split_String :=
                       Split (To_String (Program (I)));
                    Temp_Line  : Unbounded_String;
                begin
                    -- pour reconnaitre un label en début de ligne, on vérifie :
                    --  Split_Line (2) = "NULL" / len = 2
                    --  Split_Line (2) = "GOTO" / len = 3
                    --  Split_Line (3) = "<-"   / len = 4 ou 6
                    --  Split_Line (2) = "IF"   / len = 5
                    if (Split_Line'Length >= 2
                        and then
                        (Split_Line (2) = "NULL"
                         or else Split_Line (2) = "GOTO"
                         or else Split_Line (2) = "IF"))
                       or else
                       (Split_Line'Length >= 3 and then Split_Line (3) = "<-")
                    then
                        -- On ajoute une correspondance entre le label et le numéro de ligne
                        WriteVariable (Context, To_String (Split_Line (1)), I);
                        -- On supprime le label de la ligne
                        for J in 2 .. Split_Line'Length loop
                            -- On ajoute le mot à la ligne temporaire
                            Append (Temp_Line, Split_Line (J));
                            -- On ajoute un espace si ce n'est pas le dernier mot
                            if J /= Split_Line'Length then
                                Append (Temp_Line, " ");
                            end if;
                        end loop;
                        -- On remplace la ligne par la ligne temporaire
                        Program (I) := Temp_Line;
                    end if;
                end;
            end loop;

            -- Remplacer les labels par leur numéro de ligne correspondant
            for I in Program'Range loop
                declare
                    Split_Line : constant Split_String :=
                       Split (To_String (Program (I)));
                    Temp_Line  : Unbounded_String;
                    Line_Num   : Integer;
                begin
                    -- pour reconnaitre un label à la fin de la ligne, on vérifie :
                    --  Split_Line (1) = "GOTO" => le label est Split_Line (2)
                    --  Split_Line (1) = "IF"   => le label est Split_Line (4)
                    if Split_Line (1) = "GOTO" then
                        -- On récupère le numéro de ligne correspondant au label
                        Line_Num  :=
                           ReadVariable (Context, To_String (Split_Line (2)));
                        -- On remplace le label par le numéro de ligne
                        Temp_Line :=
                           Split_Line (1) & " " &
                           To_Unbounded_String
                              (Line_Num'Image (2 .. Line_Num'Image'Length));

                        Program (I) := Temp_Line;
                    elsif Split_Line (1) = "IF" then
                        -- On récupère le numéro de ligne correspondant au label
                        Line_Num  :=
                           ReadVariable (Context, To_String (Split_Line (4)));
                        -- On remplace le label par le numéro de ligne
                        Temp_Line :=
                           Split_Line (1) & " " & Split_Line (2) & " " &
                           Split_Line (3) & " " &
                           To_Unbounded_String
                              (Line_Num'Image (2 .. Line_Num'Image'Length));

                        Program (I) := Temp_Line;
                    end if;
                end;
            end loop;

            Destroy (Context);

            return Program;
        end;

    end LoadFile;

end ProgramLoader;
