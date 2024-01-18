with Context; use Context;

procedure Test_Context is
   Ctx : T_Context;
   Value: Integer;
begin
   Initialize(Ctx);

   WriteVariable(Ctx, "Var1", 42);

   Value := ReadVariable(Ctx, "Var1");

   pragma Assert (Value = 42);
end Test_Context;
