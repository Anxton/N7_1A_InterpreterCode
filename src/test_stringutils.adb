with Ada.Text_IO; use Ada.Text_IO;
with StringUtils; use StringUtils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_StringUtils is
    Line : constant String := "L1 n <- allo + 23";
    Split_Line : constant Split_String := Split (Line);

    Line_Short : constant String := "bobo";
    Split_Line_Short : constant Split_String := Split (Line_Short);
begin
    Put_Line (To_String (Split_Line (1)));
    pragma Assert (Split_Line (1) = To_Unbounded_String ("L1"));
    Put_Line (To_String (Split_Line (2)));
    pragma Assert (Split_Line (2) = To_Unbounded_String ("n"));
    Put_Line (To_String (Split_Line (3)));
    pragma Assert (Split_Line (3) = To_Unbounded_String ("<-"));
    Put_Line (To_String (Split_Line (4)));
    pragma Assert (Split_Line (4) = To_Unbounded_String ("allo"));
    Put_Line (To_String (Split_Line (5)));
    pragma Assert (Split_Line (5) = To_Unbounded_String ("+"));
    Put_Line (To_String (Split_Line (6)));
    pragma Assert (Split_Line (6) = To_Unbounded_String ("23"));
    
    Put_Line (To_String (Split_Line_Short (1)));
    pragma Assert (Split_Line_Short (1) = To_Unbounded_String ("bobo"));
   
end Test_StringUtils;
