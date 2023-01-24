with P_SGF; use P_SGF;
with P_Chaine; use P_Chaine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_SGF is
    System: SGF;
    arg: Liste_String := null;
begin
    -- Création du système de gestion de fichier
    System := Creer;

    -- Création d'un fichier
    Put_Line("------------------");
    Put_Line("touch test.txt; mkdir tata; touch tata/toto.png; ls .; ls -l tata");
    Put_Line("------------------");
    New_Line;

    arg := new Noeud_String'(Valeur => To_Unbounded_String("test.txt"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => touch, Option => none, Args => arg));

    -- Création d'un dossier depuis un chemin relatif
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => mkdir, Option => none, Args => arg));

    -- Création d'un fichier depuis un chemin relatif
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata/toto.png"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => touch, Option => none, Args => arg));

    -- Affichage du contenu du répertoire courant (on precise "." pour le répertoire courant)
    -- C'est l'interpreteur de commande qui s'occupera de ce changement
    arg := new Noeud_String'(Valeur => To_Unbounded_String("."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Affichage du contenu du répertoire courant avec les détails
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => l, Args => arg));

    -- Changer le répertoire courant pour le dossier "tata"
    Put_Line("------------------");
    Put_Line("cd tata; ls .; ls /");
    Put_Line("------------------");
    New_Line;

    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- Affichage du contenu du répertoire courant
    arg := new Noeud_String'(Valeur => To_Unbounded_String("."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Affichage du contenu de la racine à partir du répertoire courant
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Supprimer le fichier "/test.txt"
    Put_Line("------------------");
    Put_Line("rm /test.txt; ls /; cd /; rm tata; rm toto.png; ls -l");
    Put_Line("------------------");
    New_Line;

    arg := new Noeud_String'(Valeur => To_Unbounded_String("/test.txt"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => none, Args => arg));

    -- Affichage du contenu de la racine à partir du répertoire courant
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Revenir à la racine
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- Supprimer le dossier "tata"
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => none, Args => arg));

    New_Line;

    -- Suppression récursive 
    Put_Line("------------------");
    Put_Line("rm -r tata; ls -l");
    Put_Line("------------------");
    New_Line;

    -- Supprimer le fichier "tata" avec l'option -r
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => r, Args => arg));

    -- Affichage du contenu du répertoire courant avec les détails
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => l, Args => arg));

end Test_SGF;