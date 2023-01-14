with P_SGF; use P_SGF;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test_SGF is
    System: SGF;
    arg: Liste_String := null;
begin
    -- Création du système de gestion de fichier
    System := Creer;

    -- Création d'un fichier
    arg := new String_Node'(Valeur => To_Unbounded_String("test.txt"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => touch, Option => none, Args => arg));

    -- Affichage du contenu du répertoire courant
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => null));
end Test_SGF;