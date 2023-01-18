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
                temp_arbre := Rechercher_sgf(F_sgf, chemin);
                Creer_fichier(temp_arbre, Nom_via_chemin(chemin));
            when mkdir =>
                chemin := F_cmd.Args.all.Valeur;
                temp_arbre := Rechercher_sgf(F_sgf, chemin);
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

    -- fonction Rechercher : recherche un élément dans le SGF
    -- params: F_sgf: in SGF
    --         F_chemin: in Unbounded_String
    -- retourne : Arbre
    function Rechercher_sgf(F_sgf: in SGF; F_chemin: in Unbounded_String) return Arbre is
        Temp_arbre: Arbre := F_sgf.Courrant;
        Temp_chemin: Unbounded_String := F_chemin;
    begin
        -- Si le chemin est vide, on retourne le dossier courant
        if Length(F_chemin) = 0  or else Index(F_chemin, "/") = 0 then
            return F_sgf.Courrant;   
        end if;

        -- Si le chemin commence par un /, on part de la racine
        if To_String(F_chemin)(1) = '/' then
            Temp_arbre := F_sgf.Racine;
            Temp_chemin := Unbounded_Slice(Temp_chemin, 2, Length(Temp_chemin));

        elsif To_String(F_chemin)(1..2) = "./" then
            -- Si le chemin commence par ./, on part du dossier courant
            Temp_chemin := Unbounded_Slice(Temp_chemin, 3, Length(Temp_chemin));

        end if;

        return Rechercher_via_chemin(Temp_arbre, Temp_chemin);
        
    end Rechercher_sgf;

    -- fonction Rechercher_via_chemin : recherche un élément dans l'arbre via un chemin
    -- params: F_arbre: in Arbre
    --         F_chemin: in Unbounded_String
    -- retourne : Arbre
    function Rechercher_via_chemin(F_arbre: in Arbre; F_chemin: in Unbounded_String) return Arbre is
        Sous_arbre: Arbre;
        Nom_pere: Unbounded_String;
        Nom_fils: Unbounded_String;
    begin
        -- Si le chemin est vide, on retourne l'arbre
        if Length(F_chemin) = 0 then
            return F_arbre;

        elsif Length(F_chemin) = 2 and To_String(F_chemin)(1..2) = ".." then
            -- Si le chemin est .., on retourne le parent
            return Pere(F_arbre);

        elsif To_String(F_chemin)(1..2) = ".." then
            -- Si le chemin commence par .., on cherche dans le parent
            return Rechercher_via_chemin(Pere(F_arbre), Unbounded_Slice(F_chemin, 3, Length(F_chemin)));

        elsif To_String(F_chemin)(1) /= '/' then
            -- Si le chemin commence par un /, on cherche dans le parent
            return Arbre_DF.Rechercher(F_arbre, DF'(Nom => F_chemin, Flag => Dossier, Perm => 0, Taille => 0));
        
        else
            -- Sinon, on cherche dans le sous-arbre
            Nom_pere := Unbounded_Slice(F_chemin, 1, Index(F_chemin, "/")-1);
            Sous_arbre := Arbre_DF.Rechercher(F_arbre, DF'(Nom => Nom_pere, Flag => Dossier, Perm => 0, Taille => 0));

            Nom_fils := Unbounded_Slice(F_chemin, Index(F_chemin, "/")+1, Length(F_chemin));
            return Rechercher_via_chemin(Sous_arbre, Nom_fils);

        end if;

    end Rechercher_via_chemin;

    -- fonction Nom_via_chemin : retourne le nom d'un élément via un chemin
    -- params: F_chemin: in Unbounded_String
    -- retourne : Unbounded_String
    function Nom_via_chemin(F_chemin: in Unbounded_String) return Unbounded_String is
    begin
        -- parcourir le chemin à l'envers jusqu'à trouver un /
        for i in Length(F_chemin)..1 loop
            if To_String(F_chemin)(i) = '/' then
                return Unbounded_Slice(F_chemin, i+1, Length(F_chemin));
            end if;
        end loop;
        -- si on ne trouve pas de /, on retourne le chemin
        return F_chemin;
    end Nom_via_chemin;

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