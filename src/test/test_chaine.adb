with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Chaine; use P_Chaine;

procedure Test_Chaine is
    ll_chaine: Liste_String := null;
begin
    pragma Assert(Nom_via_chemin(To_Unbounded_String("/tata/toto.txt")) = To_Unbounded_String("toto.txt"));

    Separer_chemin(To_Unbounded_String("/tata/toto.txt"), ll_chaine);
    afficher_liste(ll_chaine);
    
end Test_Chaine;
