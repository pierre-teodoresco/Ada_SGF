with P_SGF; use P_SGF;
with P_Chaine; use P_Chaine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_SGF is
    System: SGF;
    arg: Liste_String := null;
    arg2: Liste_String := null;
begin
    -- Création du système de gestion de fichier
    System := Creer;

    -- Création d'un fichier
    Put_Line("################################################################################");
    Put(">");
    Print_chemin_absolu(System);
    Put_Line("touch test.txt; mkdir tata; touch tata/toto.png; ls .; ls -l tata");
    New_Line;

    -- touch test.txt
    arg := new Noeud_String'(Valeur => To_Unbounded_String("test.txt"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => touch, Option => none, Args => arg));

    -- mkdir tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => mkdir, Option => none, Args => arg));

    -- touch tata/toto.png
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata/toto.png"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => touch, Option => none, Args => arg));

    -- ls .
    arg := new Noeud_String'(Valeur => To_Unbounded_String("."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- ls -l tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => l, Args => arg));

    -- Changer le répertoire courant pour le dossier "tata"
    Put_Line("################################################################################");
    Put(">");
    Print_chemin_absolu(System);
    Put_Line("cd tata; ls .; ls /");
    New_Line;

    -- cd tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- ls .
    arg := new Noeud_String'(Valeur => To_Unbounded_String("."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- ls /
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Supprimer le fichier "/test.txt"
    Put_Line("################################################################################");
    Put(">");
    Print_chemin_absolu(System);
    Put_Line("rm /test.txt; ls /; cd /; rm tata; rm toto.png; ls -l");
    New_Line;

    -- rm /test.txt
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/test.txt"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => none, Args => arg));

    -- ls /
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- cd /
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- rm tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => none, Args => arg));

    New_Line;

    -- Copie
    Put_Line("################################################################################");
    Put(">");
    Print_chemin_absolu(System);
    Put_Line("cp test.txt tata; ls tata; ls .");
    New_Line;

    -- cp text.txt tata
    arg2 := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    arg := new Noeud_String'(Valeur => To_Unbounded_String("test.txt"), Suivant => arg2);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cp, Option => none, Args => arg));

    -- ls tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    -- ls .
    arg := new Noeud_String'(Valeur => To_Unbounded_String("."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Suppression récursive 
    Put_Line("################################################################################");
    Put(">");
    Print_chemin_absolu(System);
    Put_Line("rm -r tata; ls /");
    New_Line;

    -- rm -r tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => r, Args => arg));

    -- ls /
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

end Test_SGF;