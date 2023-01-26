with P_Cli; use P_Cli;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_Cli is 
begin
    Run_test(To_Unbounded_String("cp -r test/dir dir/test"));
end Test_Cli;