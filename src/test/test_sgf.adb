with P_SGF; use P_SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_SGF is
    System: SGF;
    cmd: Commande;
begin
    cmd := Commande'(Nom => ls, Option => r, Args => null);
    System := Creer;
    Lancer(F_sgf => System, F_cmd => cmd);
end Test_SGF;