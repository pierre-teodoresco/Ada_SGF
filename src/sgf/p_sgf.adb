with Ada.Text_IO; use Ada.Text_IO;

package body P_SGF is

    -- Sous-programmes

    -- fonction Creer : crée la racine du SGF
    -- retourne : SGF
    function Creer return SGF is
        Racine_DF: DF;
        Arbre_Racine: Arbre;
        Racine: SGF;
    begin
        -- Création de la racine du SGF
        Racine_DF.Nom := To_Unbounded_String("/");
        Racine_DF.Flag := Dossier;
        Racine_DF.Perm := 777;
        Racine_DF.Taille := 0;

        -- Création de l'arbre de la racine
        Arbre_Racine := Arbre_DF.Creer(Racine_DF);

        -- Création du SGF
        Racine.Racine := Arbre_Racine;
        Racine.Courrant := Arbre_Racine;
        Racine.Format:= To_Unbounded_String("FAT32");

        return Racine;
    end Creer;

    -- procedure Lancer : exécute la commande
    -- params: F_Cmd: in Commande - commande à exécuter
    procedure Lancer(F_Cmd: in Commande) is
    begin
        null;
    end Lancer;

    procedure Display(F_sgf: in SGF) is
    begin
        Afficher_Arbre_SGF(F_sgf.Racine);
    end Display;

    -- PRIVATE

    -- package
    procedure Afficher_DF(F_df: in DF) is
    begin
        Put_Line("Nom: " & To_String(F_df.Nom));
        case F_df.Flag is
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

    -- procedure Creer_dossier : crée un dossier dans le SGF
    -- params: F_Arbre: in out Arbre
    --         F_Nom: in Unbounded_String
    --         F_Perm: in DF_Perm
    procedure Creer_dossier(F_Arbre: in out Arbre_DF.Arbre; F_Nom: in Unbounded_String; F_Perm: in Natural) is
    begin
        null;
    end Creer_dossier;
    
    -- procedure Creer_fichier : crée un fichier dans le SGF
    -- params: F_Arbre: in out Arbre
    --         F_Nom: in Unbounded_String
    --         F_Perm: in DF_Perm
    --         F_Taille: in Integer
    procedure Creer_fichier(F_Arbre: in out Arbre; F_Nom: in Unbounded_String; F_Perm: in Natural; F_Taille: in Natural) is
    begin
        null;
    end Creer_fichier;

    -- procedure Afficher : affiche l'architecture du SGF
    -- params: F_Arbre: in Arbre
    procedure Afficher(F_sgf: in SGF) is
    begin
        Afficher_Arbre_SGF(F_sgf.Racine);
    end Afficher;

    -- procedure Supprimer : supprime un élément du SGF
    -- params: F_Arbre: in out Arbre
    --         F_Element: in Arbre
    procedure Supprimer(F_Arbre: in out Arbre; F_Element: in Arbre) is
    begin
        null;
    end Supprimer;
    
    -- procedure Deplacer : déplace un élément du SGF
    -- params: F_Arbre: in out Arbre
    --         F_Element: in Arbre
    --         F_Parent: in Arbre
    procedure Deplacer(F_Arbre: in out Arbre; F_Element: in Arbre; F_Parent: in Arbre) is
    begin
        null;
    end Deplacer;

    -- procedure Archiver : archive un élément du SGF
    -- params: F_Arbre: in out Arbre
    --         F_nouvelle_taille
    procedure Archiver(F_Arbre: in out Arbre; F_Element: in Arbre) is
    begin
        null;
    end Archiver;

end P_SGF;