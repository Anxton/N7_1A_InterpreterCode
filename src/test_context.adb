with Ada.Text_IO; use Ada.Text_IO;
with Context; use Context;

procedure Test_Context is
    Ctx : T_Context;
    Value: Integer;
begin
    Initialize(Ctx);

    WriteVariable(Ctx, "Var1", 42);
    Value := ReadVariable(Ctx, "Var1");

    Put_Line("Var1 = " & Value'Image);
    pragma Assert (Value = 42);

    WriteVariable(Ctx, "Var1", 18);
    Value := ReadVariable(Ctx, "Var1");

    Put_Line("Var1 = " & Value'Image);
    pragma Assert (Value = 18);

    WriteVariable(Ctx, "bonjour", -50);
    Value := ReadVariable(Ctx, "bonjour");

    Put_Line("bonjour = " & Value'Image);
    pragma Assert (Value = -50);

    Value := ReadVariable(Ctx, "Var1");
    Put_Line("Var1 = " & Value'Image);
    pragma Assert (Value = 18);

    Destroy (Ctx);
end Test_Context;
