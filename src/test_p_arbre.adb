with P_Arbre; use P_Arbre;

procedure Test_P_Arbre is
    A: Arbre;
    B: Arbre;
    C: Arbre;
    D: Arbre;
    E: Arbre;
begin
    -- Création de l'arbre (1er niveau)
    Creer(A);

    -- Insertion de valeurs sur le 1er niveau
    Inserer(A, 5);
    Inserer(A, 3);
    
    -- 2ème niveau
    B := Fils(A);
    C := Frere(B);

    -- Insertion de valeurs sur le 2ème niveau
    Inserer(B, 2);
    Inserer(B, 4);

    Inserer(C, 6);
    Inserer(C, 7);

    -- 3ème niveau
    D := Fils(B);
    E := Fils(C);

    -- Insertion de valeurs sur le 3ème niveau
    Inserer(D, 1);
    Inserer(E, 8);

    -- Affichage de l'arbre
    Afficher(A);

    -- Destruction de l'arbre
    Detruire(A);

end Test_P_Arbre;