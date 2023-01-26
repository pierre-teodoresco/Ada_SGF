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
    Put_Line(">" & To_String(Chemin_absolu(System)));
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
    Put_Line(">" & To_String(Chemin_absolu(System)));
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
    Put_Line(">" & To_String(Chemin_absolu(System)));
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
    Put_Line(">" & To_String(Chemin_absolu(System)));
    Put_Line("cp test.txt tata; ls tata; ls .");
    New_Line;

    -- cp text.txt tata
    arg2 := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    arg := new Noeud_String'(Valeur => To_Unbounded_String("test.txt"), Suivant => arg2);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cp, Option => none, Args => arg));

    -- ls tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- ls .
    arg := new Noeud_String'(Valeur => To_Unbounded_String("."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Copie récursive
    Put_Line("################################################################################");
    Put_Line(">" & To_String(Chemin_absolu(System)));
    Put_Line("mkdir titi; cp -r tata titi; ls titi; ls tata; ls titi/tata");
    New_Line;

    -- mkdir titi
    arg := new Noeud_String'(Valeur => To_Unbounded_String("titi"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => mkdir, Option => none, Args => arg));

    -- cp -r tata titi
    arg2 := new Noeud_String'(Valeur => To_Unbounded_String("titi"), Suivant => null);
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => arg2);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cp, Option => r, Args => arg));

    -- ls titi
    arg := new Noeud_String'(Valeur => To_Unbounded_String("titi"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- ls tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));  

    New_Line;

    -- ls titi/tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("titi/tata"), Suivant => arg2);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));

    New_Line;

    -- Working directory
    Put_Line("################################################################################");
    Put_Line(">" & To_String(Chemin_absolu(System)));
    Put_Line("cd /titi; cd tata; pwd; cd ..; pwd; cd /titi/tata; pwd");
    New_Line;

    -- cd /titi
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/titi"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- cd tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- pwd
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => pwd, Option => none, Args => null));

    New_Line;

    -- cd ..
    arg := new Noeud_String'(Valeur => To_Unbounded_String(".."), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- pwd
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => pwd, Option => none, Args => null));

    -- cd /titi/tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/titi/tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => cd, Option => none, Args => arg));

    -- pwd
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => pwd, Option => none, Args => null));

    New_Line;

    -- Suppression récursive 
    Put_Line("################################################################################");
    Put_Line(">" & To_String(Chemin_absolu(System)));
    Put_Line("rm -r tata; ls /");
    New_Line;

    -- rm -r tata
    arg := new Noeud_String'(Valeur => To_Unbounded_String("tata"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => rm, Option => r, Args => arg));

    -- ls /
    arg := new Noeud_String'(Valeur => To_Unbounded_String("/"), Suivant => null);
    Lancer(F_sgf => System, F_cmd => Commande'(Nom => ls, Option => none, Args => arg));      



    -- Destruction du SGF
    Detruire(System);

end Test_SGF;