package body P_SGF is

    -- Sous-programmes

    -- fonction Creer : crée la racine du SGF
    -- retourne : SGF
    function Creer return SGF is
        Racine_DF: DF;
        Arbre_Racine: Arbre_DF;
    begin
        Racine_DF := DF(Nom => "/", Spec => DF_Spec.Dossier, Perm => 777, Taille => 0);
        Arbre_Racine := Arbre_DF.Creer(Racine_DF);
        return SGF(Racine => Arbre_Racine, Courrant => Arbre_Racine);
    end Creer;

    -- PRIVATE

    -- package
    procedure Afficher_DF(F_df: in DF) is
    begin
        Put_Line("Nom: " & F_df.Nom);
        case F_df.Spec is
            when Dossier =>
                Put_Line("Type: Dossier");
            when Fichier =>
                Put_Line("Type: Fichier");
        end case;
        Put_Line("Permissions: " & Natural'Image(F_df.Perm));
        Put_Line("Taille: " & Natural'Image(F_df.Taille));
        New_Line;
    end Afficher_DF;

    -- Sous-programmes

    --  -- procedure Creer_dossier : crée un dossier dans le SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Nom: in Unbounded_String
    --  --         F_Perm: in DF_Perm
    procedure Creer_dossier(F_Arbre: in out Arbre_DF.Arbre; F_Nom: in Unbounded_String; F_Perm: in Natural) is
    begin
        null;
    end Creer_dossier;
    
    --  -- procedure Creer_fichier : crée un fichier dans le SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Nom: in Unbounded_String
    --  --         F_Perm: in DF_Perm
    --  --         F_Taille: in Integer
    procedure Creer_fichier(F_Arbre: in out Arbre; F_Nom: in Unbounded_String; F_Perm: in Natural; F_Taille: in Natural) is
    begin
        null;
    end Creer_fichier;

    --  -- procedure Afficher : affiche l'architecture du SGF
    --  -- params: F_Arbre: in Arbre
    procedure Afficher(F_Arbre: in Arbre) is
    begin
        null;
    end Afficher;

    --  -- procedure Supprimer : supprime un élément du SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Element: in Arbre
    procedure Supprimer(F_Arbre: in out Arbre; F_Element: in Arbre) is
    begin
        null;
    end Supprimer;
    
    --  -- procedure Deplacer : déplace un élément du SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Element: in Arbre
    --  --         F_Parent: in Arbre
    procedure Deplacer(F_Arbre: in out Arbre; F_Element: in Arbre; F_Parent: in Arbre) is
    begin
        null;
    end Deplacer;

    --  -- procedure Archiver : archive un élément du SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_nouvelle_taille
    procedure Archiver(F_Arbre: in out Arbre; F_Element: in Arbre) is
    begin
        null;
    end Archiver;

end P_SGF;