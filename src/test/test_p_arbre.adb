with P_Arbre;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_P_Arbre is

    -- instanciation de package générique
    package Arbre_Entier is
        new P_Arbre(Type_Element => Integer);
    use Arbre_Entier;
    
    -- affichage générique
    procedure Put_Int(F_i: Integer) is 
    begin
        Put(F_i, 2);
    end Put_Int;

    procedure Afficher_Entier is new Arbre_Entier.Afficher(Afficher_contenu => Put_Int);

    A: Arbre;
    B: Arbre;
    C: Arbre;
    D: Arbre;
    E: Arbre;
begin
    -- Création de l'arbre (1er niveau)
    A := Creer;

    Put_Line("Ajout de valeurs dans l'arbre :");

    -- Insertion de valeurs sur le 1er niveau
    Ajouter(A, 5);
    Ajouter(A, 3);
    
    -- 2ème niveau
    B := Fils(A);
    C := Frere(B);

    -- Insertion de valeurs sur le 2ème niveau
    Ajouter(B, 2);
    Ajouter(B, 4);

    Ajouter(C, 6);
    Ajouter(C, 7);

    -- 3ème niveau
    D := Fils(B);
    E := Fils(C);

    -- Insertion de valeurs sur le 3ème niveau
    Ajouter(D, 1);
    Ajouter(D, 8);

    Ajouter(E, 9);

    -- Affichage de l'arbre
    Afficher_Entier(A);

    New_Line;
    Put_Line("--------------------");
    Put_Line("Suppression de valeurs dans l'arbre :");

    --  Suppression d'élément
    Supprimer(E);

    --  Affichage de l'arbre
    Afficher_Entier(A);

    New_Line;
    Put_Line("--------------------");
    Put_Line("Déplacement de valeurs dans l'arbre :");

    -- Deplacer
    Deplacer(D, C);

    -- Affichage de l'arbre
    Afficher_Entier(A);

    -- Destruction de l'arbre
    Detruire(A);

end Test_P_Arbre;