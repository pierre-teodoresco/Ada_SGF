with P_Arbre; use P_Arbre;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_P_Arbre is
    A: Arbre;
    B: Arbre;
    C: Arbre;
    D: Arbre;
    E: Arbre;
begin
    -- Création de l'arbre (1er niveau)
    A := Construct(0);

    -- Insertion de valeurs sur le 1er niveau
    Ajouter(A, Construct(5));
    Ajouter(A, Construct(3));
    
    -- 2ème niveau
    B := Fils(A);
    C := Frere(B);

    -- Insertion de valeurs sur le 2ème niveau
    Ajouter(B, Construct(2));
    Ajouter(B, Construct(4));

    Ajouter(C, Construct(6));
    Ajouter(C, Construct(7));

    -- 3ème niveau
    D := Fils(B);
    E := Fils(C);

    -- Insertion de valeurs sur le 3ème niveau
    Ajouter(D, Construct(1));
    Ajouter(D, Construct(8));

    -- Affichage de l'arbre
    Afficher(A);

    Put_Line("--------------------");

    -- Suppression d'élément
    Supprimer(D);

    -- Affichage de l'arbre
    Afficher(A);

    -- Destruction de l'arbre
    Detruire(A);

end Test_P_Arbre;