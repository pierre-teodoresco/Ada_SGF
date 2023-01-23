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
    -- params: F_Cmd: in Commande   - commande à exécuter
    --         F_sgf: in SGF        - SGF sur lequel exécuter la commande
    procedure Lancer(F_sgf: in out SGF; F_cmd: in Commande) is
        temp_arbre: Arbre;
        chemin: Unbounded_String;
    begin
        -- Agir en fonction de la commande
        case F_cmd.Nom is
            when pwd =>
                -- TODO : afficher le chemin du dossier courant
                null;
            when touch =>
                chemin := F_cmd.Args.all.Valeur;
                temp_arbre := Rechercher_sgf(F_sgf, chemin, True);
                Creer_fichier(temp_arbre, Nom_via_chemin(chemin));
            when mkdir =>
                chemin := F_cmd.Args.all.Valeur;
                temp_arbre := Rechercher_sgf(F_sgf, chemin, True);
                Creer_dossier(temp_arbre, Nom_via_chemin(chemin));
            when ls =>
                if F_cmd.Option = none then
                    Afficher(F_sgf);
                end if;
                if F_cmd.Option = l then
                    Afficher_complet(F_sgf);
                end if;
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
    exception
        when PATH_NOT_EXISTS => Put_Line("Le chemin n'existe pas");
    end Lancer;

    -- PRIVATE

    -- package

    function Egal_DF(F_1: in DF; F_2: in DF) return Boolean is
    begin
        return F_1.Nom = F_2.Nom;
    end Egal_DF;

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

   -- fonction Rechercher_sgf : recherche un element à partir d'un chemin
    -- params: F_sgf: in SGF
    --         F_chemin: in Unbounded_String
    --         F_est_createur: in Boolean
    -- retourne : Arbre
    function Rechercher_sgf(F_sgf: in SGF; F_chemin: in Unbounded_String; F_est_createur: in Boolean) return Arbre is
        dfs: Liste_String := Init_liste;
        est_relatif: Boolean := True;
    begin
        dfs := Separer_chemin(F_chemin);

        if Taille_liste(dfs) = 0 then
            return F_sgf.Courrant;
        elsif Get_liste(dfs, 1) = "/" then
            est_relatif := False;
        end if;
        
        if F_est_createur then
            Pop_back(dfs);
        end if;

        return Rechercher_via_liste(F_sgf, dfs, est_relatif);
        
    end Rechercher_sgf;

    -- fonction Rechercher_via_liste : recherche un élément à partir d'une liste de noms
    -- params: F_sgf: in SGF
    --         F_dfs: in Liste_String
    --         F_est_relatif: in Boolean
    -- retourne : Arbre
    function Rechercher_via_liste(F_sgf: in SGF; F_dfs: in Liste_String; F_est_relatif: in Boolean) return Arbre is
    begin
        if F_est_relatif then
            return Rechercher_df(F_sgf.Courrant, F_dfs);
        else
            return Rechercher_df(F_sgf.Racine, F_dfs);
        end if;
    end Rechercher_via_liste;

    -- fonction Rechercher_df : recherche un élément à partir d'un chemin absolu ou relatif
    -- params: F_arbre: in Arbre
    --         F_dfs: in Liste_String
    -- retourne : Arbre
    function Rechercher_df(F_arbre: in Arbre; F_dfs: in Liste_String) return Arbre is
        tmp_arbre: Arbre := F_arbre;
        tmp_df: DF;
    begin
        for i in 1..Taille_liste(F_dfs) loop
            -- on verifie si on doit prendre le pere sur ".."
            if Get_liste(F_dfs, i) = ".." then
                tmp_arbre := Pere(tmp_arbre);
            else
                -- on cree la df
                tmp_df := DF'(Nom => Get_liste(F_dfs, i), Flag => Dossier, Perm => 777, Taille => 0);
                -- on recherche l'element
                tmp_arbre := Rechercher(F_arbre, tmp_df);
                -- on recupere le contenu de l'element
                tmp_df := Contenu(tmp_arbre);
                -- si l'element n'existe pas ou si on est sur un fichier sans être à la fin du chemin on leve une exception
                if Arbre_DF.Est_vide(tmp_arbre) or else (i < Taille_liste(F_dfs) and tmp_df.Flag = Fichier) then
                    raise PATH_NOT_EXISTS;
                end if;
            end if;
        end loop;
        return tmp_arbre;
    end Rechercher_df;

    -- procedure Creer_dossier : crée un dossier dans le SGF
    -- params: F_arbre: in out Arbre
    --         F_Nom: in Unbounded_String
    procedure Creer_dossier(F_arbre: in out Arbre; F_Nom: in Unbounded_String) is
    begin
        Arbre_DF.Ajouter(F_arbre, DF'(Nom => F_Nom, Flag => Dossier, Perm => 777, Taille => 0));
    end Creer_dossier;
    
    -- procedure Creer_fichier : crée un fichier dans le SGF
    -- params: F_arbre: in out Arbre
    --         F_Nom: in Unbounded_String
    procedure Creer_fichier(F_arbre: in out Arbre; F_Nom: in Unbounded_String) is
    begin
        Arbre_DF.Ajouter(F_arbre, DF'(Nom => F_Nom, Flag => Fichier, Perm => 777, Taille => 0));
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

end P_SGF;