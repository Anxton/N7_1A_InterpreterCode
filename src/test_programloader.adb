with ProgramLoader; use ProgramLoader;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_ProgramLoader is
   FilePath : constant String := "test.source";
begin
   declare
      Program : T_Program := LoadFile(FilePath);
   begin
      pragma Assert (Program(1) = "n <- 42");
   end;
end Test_ProgramLoader;
