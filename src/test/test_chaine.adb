with Ada.Text_IO; use Ada.Text_IO;
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
    New_Line;

    ll_chaine := Separer_chemin(To_Unbounded_String("tata/toto.txt"));
    pragma Assert(Taille_liste(ll_chaine) = 2);
    afficher_liste(ll_chaine);
    New_Line;

    pragma Assert(Get_liste(ll_chaine, 1) = To_Unbounded_String("tata"));
    pragma Assert(Get_liste(ll_chaine, 2) = To_Unbounded_String("toto.txt"));
    pragma Assert(Get_liste(ll_chaine, 3) = To_Unbounded_String(""));

    Pop_back(ll_chaine);
    pragma Assert(Taille_liste(ll_chaine) = 1);

    Pop_front(ll_chaine);
    pragma Assert(Taille_liste(ll_chaine) = 0);

    pragma Assert(Get_liste(ll_chaine, 1) = To_Unbounded_String(""));

    ll_chaine := Separer_commande(To_Unbounded_String("touch tata/toto.txt"));
    pragma Assert(Taille_liste(ll_chaine) = 2);
    afficher_liste(ll_chaine);
    New_Line;

    ll_chaine := Separer_commande(To_Unbounded_String("cp -r tata/toto.txt titi"));
    pragma Assert(Taille_liste(ll_chaine) = 4);
    afficher_liste(ll_chaine);
    New_Line;

end Test_Chaine;
