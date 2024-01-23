with Ada.Text_IO; use Ada.Text_IO;
with ProgramLoader;         use ProgramLoader;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_ProgramLoader is
    FilePath : constant String := "test.source";
begin
    declare
        Program : constant T_Program := LoadFile (FilePath);
    begin
    --  -- Ce code intermédiaire calcule la factorielle F d’un entier n
    --  Programme Facto est
    --  n, i, Fact, T1, T2, T3 : Entier
    --  Début
    --  n <- 5
    --  i <- 1
    --  Fact <- 1
    --  T1 <- i < n
    --  T2 <- i = n
    --  T3 <- T1 OR T2
    --  L1 IF T3 GOTO L3
    --  GOTO L2
    --  L3 Fact <- Fact * i
    --  i <- i + 1
    --  T1 <- i < n
    --  T2 <- i = n
    --  T3 <- T1 OR T2
    --  GOTO L1
    --  L2 NULL
    --  Fin
        pragma Assert (Program'Length = 19);
        pragma Assert (Program (1) = "Programme Facto est");
        pragma Assert (Program (2) = "n, i, Fact, T1, T2, T3 : Entier");
        pragma Assert (Program (3) = "Début");
        pragma Assert (Program (4) = "n <- 5");
        pragma Assert (Program (5) = "i <- 1");
        pragma Assert (Program (6) = "Fact <- 1");
        pragma Assert (Program (7) = "T1 <- i < n");
        pragma Assert (Program (8) = "T2 <- i = n");
        pragma Assert (Program (9) = "T3 <- T1 OR T2");
        pragma Assert (Program (10) = "IF T3 GOTO 12");
        pragma Assert (Program (11) = "GOTO 18");
        pragma Assert (Program (12) = "Fact <- Fact * i");
        pragma Assert (Program (13) = "i <- i + 1");
        pragma Assert (Program (14) = "T1 <- i < n");
        pragma Assert (Program (15) = "T2 <- i = n");
        pragma Assert (Program (16) = "T3 <- T1 OR T2");
        pragma Assert (Program (17) = "GOTO 10");
        pragma Assert (Program (18) = "NULL");
        pragma Assert (Program (19) = "Fin");
        for I in 1 .. Program'Length loop
            Put_Line (To_String (Program (I)));
        end loop;
    end;
end Test_ProgramLoader;
