with P_SGF; use P_SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_SGF is
    System: SGF;
begin
    System := Creer;
    Add_File(System, To_Unbounded_String("Fichier1"), Dossier, 777, 0);
    Display(System);
end Test_SGF;