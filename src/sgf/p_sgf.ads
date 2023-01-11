with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Arbre;


package P_SGF is 

    -- DF: le type Dossier Fichier
    type DF is private;

    -- instanciation de P_Arbre avec le type DF
    package Arbre_DF is 
        new P_Arbre(Type_Element => DF);
    use Arbre_DF;

    procedure Afficher_DF(F_df: in DF) is
    begin
        Put_Line("Nom: " & F_df.Nom);
        case F_df.Type is
            when Dossier =>
                Put_Line("Type: Dossier");
            when Fichier =>
                Put_Line("Type: Fichier");
        end case;
        case F_df.Perm is
            when Lecture =>
                Put_Line("Perm: Lecture");
            when Ecriture =>
                Put_Line("Perm: Ecriture");
            when Execution =>
                Put_Line("Perm: Execution");
        end case;
        Put_Line("Taille: " & Integer'Image(F_df.Taille));
        New_Line;
    end Afficher_DF;

    procedure Afficher_Architecture is new Arbre_DF.Afficher(Afficher_Contenu => Afficher_DF);

    -- Sous-programmes

    -- procedure Init : initialise le SGF en créant la racine '/'
    -- params: F_Arch: out Arbre_DF
    procedure Init(F_Arch: out Arbre_DF);

    -- procedure Creer_dossier : crée un dossier dans le SGF
    -- params: F_Arch: in out Arbre_DF
    --         F_Nom: in Unbounded_String
    --         F_Perm: in DF_Perm
    --         F_Parent: in Arbre_DF
    procedure Creer_dossier(F_Arch: in out Arbre_DF; F_Nom: in Unbounded_String; F_Perm: in DF_Perm; F_Parent: in Arbre_DF);
    
    -- procedure Creer_fichier : crée un fichier dans le SGF
    -- params: F_Arch: in out Arbre_DF
    --         F_Nom: in Unbounded_String
    --         F_Perm: in DF_Perm
    --         F_Taille: in Integer
    --         F_Parent: in Arbre_DF
    procedure Creer_fichier(F_Arch: in out Arbre_DF; F_Nom: in Unbounded_String; F_Perm: in DF_Perm; F_Taille: in Natural; F_Parent: in Arbre_DF);

    -- procedure Afficher : affiche l'architecture du SGF
    -- params: F_Arch: in Arbre_DF
    procedure Afficher(F_Arch: in Arbre_DF);

    -- procedure Supprimer : supprime un élément du SGF
    -- params: F_Arch: in out Arbre_DF
    --         F_Element: in Arbre_DF
    procedure Supprimer(F_Arch: in out Arbre_DF; F_Element: in Arbre_DF);
    
    -- procedure Deplacer : déplace un élément du SGF
    -- params: F_Arch: in out Arbre_DF
    --         F_Element: in Arbre_DF
    --         F_Parent: in Arbre_DF
    procedure Deplacer(F_Arch: in out Arbre_DF; F_Element: in Arbre_DF; F_Parent: in Arbre_DF);

    -- procedure Archiver : archive un élément du SGF
    -- params: F_Arch: in out Arbre_DF
    --         F_nouvelle_taille
    procedure Archiver(F_Arch: in out Arbre_DF; F_Element: in Arbre_DF);

private
    type DF_Type is (Dossier, Fichier);
    type DF_Perm is (Lecture, Ecriture, Execution);
    type DF is record
        Nom: Unbounded_String;
        Type: DF_Type;
        Perm: DF_Perm;
        Taille: Natural;
    end record;
end P_SGF;