with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Chaine; use P_Chaine;

procedure Test_Chaine is
    ll_chaine: Liste_String;
begin
    ll_chaine := Init_liste;

    pragma Assert(Nom_via_chemin(To_Unbounded_String("/tata/toto.txt")) = To_Unbounded_String("toto.txt"));

    ll_chaine := Separer_chemin(To_Unbounded_String("/tata/toto.txt"));
    pragma Assert(Taille_liste(ll_chaine) = 3);
    afficher_liste(ll_chaine);

end Test_Chaine;
