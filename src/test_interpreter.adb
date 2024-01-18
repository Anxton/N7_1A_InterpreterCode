with Interpreter; use Interpreter;

procedure Test_Interpreter is
    FilePath : constant String := "test.source";
begin
    RunNormal(FilePath);
    RunDebug(FilePath);
end Test_Interpreter;
