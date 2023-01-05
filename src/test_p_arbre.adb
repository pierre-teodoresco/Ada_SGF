with P_Arbre; use P_Arbre;

procedure Test_P_Arbre is
    A: Arbre;
    B: Arbre;
begin
    -- Création de l'arbre
    Creer(A);

    -- Insertion de valeurs
    Inserer(A, 5);
    Inserer(A, 3);
    
    -- Fils
    B := Fils(A);

    -- Insertion de valeurs
    Inserer(B, 2);
    Inserer(B, 4);

    -- Affichage de l'arbre
    Afficher(A);

    -- Destruction de l'arbre
    Detruire(A);

end Test_P_Arbre;