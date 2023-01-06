with P_Arbre; use P_Arbre;

procedure Test_P_Arbre is
    A: Arbre;
    B: Arbre;
    C: Arbre;
    D: Arbre;
    E: Arbre;
begin
    -- Création de l'arbre
    Creer(A);

    -- Insertion de valeurs
    Inserer(A, 5);
    Inserer(A, 3);
    
    -- Fils
    B := Fils(A);
    C := Frere(B);
    D := Fils(B);
    E := Fils(C);

    -- Insertion de valeurs
    Inserer(B, 2);
    Inserer(B, 4);

    Inserer(C, 6);
    Inserer(C, 7);

    Inserer(D, 1);
    Inserer(E, 8);

    -- Affichage de l'arbre
    Afficher(A);

    -- Destruction de l'arbre
    Detruire(A);

end Test_P_Arbre;