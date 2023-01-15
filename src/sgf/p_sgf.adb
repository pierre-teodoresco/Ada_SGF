with Ada.Text_IO; use Ada.Text_IO;

package body P_SGF is

    -- Sous-programmes

    -- fonction Creer : crée la racine du SGF
    -- retourne : SGF
    function Creer return SGF is
        Racine_DF: DF;
        Arbre_Racine: Arbre;
    begin
        -- Création du DF de la racine
        Racine_DF := DF'(Nom => To_Unbounded_String("/"), Flag => Dossier, Perm => 777, Taille => 0);

        -- Création de l'arbre de la racine
        Arbre_Racine := Arbre_DF.Creer(Racine_DF);

        -- Création du SGF
        return SGF'(Racine => Arbre_Racine, Courrant => Arbre_Racine, Format => To_Unbounded_String("ext4"));
    end Creer;

    -- procedure Lancer : exécute la commande
    -- params: F_Cmd: in Commande - commande à exécuter
    --        F_sgf: in SGF - SGF sur lequel exécuter la commande
    procedure Lancer(F_sgf: in out SGF; F_Cmd: in Commande) is
    begin
        -- Agir en fonction de la commande
        case F_Cmd.Nom is
            when pwd =>
                -- TODO : afficher le chemin du dossier courant
                null;
            when touch =>
                Creer_fichier(F_sgf, F_cmd.Args.all.Valeur);
            when mkdir =>
                -- TODO : créer un dossier
                null;
            when ls =>
                case F_cmd.Option is
                    when none => Afficher(F_sgf);
                    when l => Afficher_complet(F_sgf);
                    when others => null;
                end case;
            when cd =>
                -- TODO : changer de dossier courant
                null;
            when rm => 
                -- TODO : supprimer un élément
                null;
            when cp =>
                -- TODO : copier un élément
                null;
            when mv =>
                -- TODO : déplacer un élément
                null;
            when tar =>
                -- TODO : archiver un élément
                null;
        end case;
    end Lancer;

    -- PRIVATE

    -- package
    procedure Afficher_DF_complet(F_df: in DF) is
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
    end Afficher_DF_complet;

    procedure Afficher_DF_simple(F_df: in DF) is
    begin
        Put_Line(To_String(F_df.Nom));
    end Afficher_DF_simple;

    -- Sous-programmes

    -- procedure Creer_dossier : crée un dossier dans le SGF
    -- params: F_sgf: in out SGF
    --         F_Nom: in Unbounded_String
    --         F_Perm: in Natural
    procedure Creer_dossier(F_sgf: in out SGF; F_Nom: in Unbounded_String; F_Perm: in Natural) is
    begin
        -- TODO : créer un dossier
        null;
    end Creer_dossier;
    
    -- procedure Creer_fichier : crée un fichier dans le SGF
    -- params: F_sgf: in out SGF
    --         F_Nom: in Unbounded_String
    --         F_Perm: in Natural
    --         F_Taille: in Natural
    procedure Creer_fichier(F_sgf: in out SGF; F_Nom: in Unbounded_String) is
    begin
        Arbre_DF.Ajouter(F_sgf.Courrant, DF'(Nom => F_Nom, Flag => Fichier, Perm => 777, Taille => 0));
    end Creer_fichier;

    -- procedure Afficher : affiche l'architecture du SGF
    -- params: F_Arbre: in Arbre
    procedure Afficher(F_sgf: in SGF) is
    begin
        Afficher_Arbre_SGF_simple(Fils(F_sgf.Courrant));
    end Afficher;

    -- procedure Afficher_complet : affiche l'architecture du SGF avec les détails
    -- params: F_sgf: in SGF
    procedure Afficher_complet(F_sgf: in SGF) is
    begin
        Afficher_Arbre_SGF_complet(F_sgf.Courrant);
    end Afficher_complet;

    -- procedure Supprimer : supprime un élément du SGF
    -- params: F_Arbre: in out Arbre
    --         F_Element: in Arbre
    procedure Supprimer(F_Arbre: in out Arbre; F_Element: in Arbre) is
    begin
        -- TODO : supprimer un élément
        null;
    end Supprimer;
    
    -- procedure Deplacer : déplace un élément du SGF
    -- params: F_Arbre: in out Arbre
    --         F_Element: in Arbre
    --         F_Parent: in Arbre
    procedure Deplacer(F_Arbre: in out Arbre; F_Element: in Arbre; F_Parent: in Arbre) is
    begin
        -- TODO : déplacer un élément
        null;
    end Deplacer;

    -- procedure Archiver : archive un élément du SGF
    -- params: F_Arbre: in out Arbre
    --         F_nouvelle_taille
    procedure Archiver(F_Arbre: in out Arbre; F_Element: in Arbre) is
    begin
        -- TODO : archiver un élément
        null;
    end Archiver;

    -- fonction Creer_DF : crée un DF à partir d'un chemin d'accès
    -- params: F_Chemin: in Unbounded_String
    --         F_Flag: in DF_Flag
    --         F_Perm: in Natural
    --         F_Taille: in Natural
    -- retourne : DF
    function Creer_DF(F_Chemin: in Unbounded_String; F_Flag: in DF_Flag; F_Perm: in Natural; F_Taille: in Natural) return DF is
        j: Natural := 1;
    begin
        if F_Chemin'Length = 0 then
            return null;
        else
            -- Séparer le chemin en liste à chaque '/'
            for i in 1 .. F_Chemin'Length loop
                if F_Chemin(I) = '/' then
                    -- SGFS'(Unbounded_Slice(F_Chemin, j, i-1));
                end if;
            end loop;
        end if;

    end Creer_DF;

end P_SGF;