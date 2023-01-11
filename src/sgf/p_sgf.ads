with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Arbre;

package P_SGF is 
    -- types
    type SGF is private;

    type Commandes is (pwd, touch, mkdir, ls, cd, rm, cp, mv, tar);
    type Options is (r);
    type Liste_String is access Unbounded_String;

    type Commande is record
        Nom: Commandes;
        Option: Options;
        Args: Liste_String;
    end record;
    
    -- Sous-programmes

    -- fonction Creer : crée la racine du SGF
    -- retourne : SGF
    function Creer return SGF;

    -- procedure Lancer : exécute la commande
    -- params: F_Cmd: in Commande - commande à exécuter
    procedure Lancer(F_Cmd: in Commande);

private

    -- types
    type DF_Spec is (Dossier, Fichier);
    type DF is record
        Nom: Unbounded_String;
        Spec: DF_Spec;
        Perm: Natural;
        Taille: Natural;
    end record;

    -- packages
    package Arbre_DF is 
        new P_Arbre(Type_Element => DF);
    use Arbre_DF;
    type SGF is record
        Racine: Arbre;
        Courrant: Arbre;
        Formats: String;
    end record;

    procedure Afficher_DF(F_df: in DF);

    procedure Afficher_SGF is new Arbre_DF.Afficher(Afficher_Contenu => Afficher_DF);

    -- Sous-programmes

    --  -- procedure Creer_dossier : crée un dossier dans le SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Nom: in Unbounded_String
    --  --         F_Perm: in DF_Perm
    procedure Creer_dossier(F_Arbre: in out Arbre_DF.Arbre; F_Nom: in Unbounded_String; F_Perm: in Natural);
    
    --  -- procedure Creer_fichier : crée un fichier dans le SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Nom: in Unbounded_String
    --  --         F_Perm: in DF_Perm
    --  --         F_Taille: in Integer
    procedure Creer_fichier(F_Arbre: in out Arbre; F_Nom: in Unbounded_String; F_Perm: in Natural; F_Taille: in Natural);

    --  -- procedure Afficher : affiche l'architecture du SGF
    --  -- params: F_Arbre: in Arbre
    procedure Afficher(F_Arbre: in Arbre);

    --  -- procedure Supprimer : supprime un élément du SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Element: in Arbre
    procedure Supprimer(F_Arbre: in out Arbre; F_Element: in Arbre);
    
    --  -- procedure Deplacer : déplace un élément du SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_Element: in Arbre
    --  --         F_Parent: in Arbre
    procedure Deplacer(F_Arbre: in out Arbre; F_Element: in Arbre; F_Parent: in Arbre);

    --  -- procedure Archiver : archive un élément du SGF
    --  -- params: F_Arbre: in out Arbre
    --  --         F_nouvelle_taille
    procedure Archiver(F_Arbre: in out Arbre; F_Element: in Arbre);
end P_SGF;