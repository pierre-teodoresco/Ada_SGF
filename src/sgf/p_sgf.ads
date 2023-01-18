with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Arbre;

package P_SGF is 
    -- types
    type DF_Flag is (Dossier, Fichier);

    type SGF is private;

    type Commandes is (pwd, touch, mkdir, ls, cd, rm, cp, mv, tar);
    type Options is (none, r, l);

    type String_Node;
    type Liste_String is access String_Node;
    type String_Node is record
        Valeur: Unbounded_String;
        Suivant: Liste_String;
    end record;

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
    --        F_sgf: in SGF - SGF sur lequel exécuter la commande
    procedure Lancer(F_sgf: in out SGF; F_cmd: in Commande);

private

    -- types
    --  type DF_Flag is (Dossier, Fichier);
    type DF is record
        Nom: Unbounded_String;
        Flag: DF_Flag;
        Perm: Natural;
        Taille: Natural;
    end record;

    -- packages

    function Egal_DF(F_1: in DF; F_2: in DF) return Boolean;

    package Arbre_DF is 
        new P_Arbre(Type_Element => DF, Egal => Egal_DF);
    use Arbre_DF;
    type SGF is record
        Racine: Arbre;
        Courrant: Arbre;
        Format: Unbounded_String;
    end record;

    type SFGS;
    type SFG_Liste is access SFGS;
    type SFGS is record
        Valeur: SGF;
        Suivant: SFG_Liste;
    end record;

    procedure Afficher_DF_complet(F_df: in DF);
    procedure Afficher_DF_simple(F_df: in DF);

    procedure Afficher_Arbre_SGF_complet is new Arbre_DF.Afficher(Afficher_Contenu => Afficher_DF_complet);
    procedure Afficher_Arbre_SGF_simple is new Arbre_DF.Afficher(Afficher_Contenu => Afficher_DF_simple);

    -- Sous-programmes

    -- fonction Rechercher : recherche un élément dans le SGF
    -- params: F_sgf: in SGF
    --         F_chemin: in Unbounded_String
    -- retourne : Arbre
    function Rechercher_sgf(F_sgf: in SGF; F_chemin: in Unbounded_String) return Arbre;

    -- fonction Rechercher_via_chemin : recherche un élément dans l'arbre via un chemin
    -- params: F_arbre: in Arbre
    --         F_chemin: in Unbounded_String
    -- retourne : Arbre
    function Rechercher_via_chemin(F_arbre: in Arbre; F_chemin: in Unbounded_String) return Arbre;

    -- fonction Nom_via_chemin : retourne le nom d'un élément via un chemin
    -- params: F_chemin: in Unbounded_String
    -- retourne : Unbounded_String
    function Nom_via_chemin(F_chemin: in Unbounded_String) return Unbounded_String;

    -- procedure Creer_dossier : crée un dossier dans le SGF
    -- params: F_sgf: in out SGF
    --         F_nom: in Unbounded_String
    --         F_perm: in Natural
    procedure Creer_dossier(F_sgf: in out SGF; F_nom: in Unbounded_String; F_perm: in Natural);
    
    -- procedure Creer_fichier : crée un fichier dans le SGF
    -- params: F_sgf: in out SGF
    --         F_nom: in Unbounded_String
    procedure Creer_fichier(F_arbre: in out Arbre; F_nom: in Unbounded_String);

    -- procedure Afficher : affiche l'architecture du SGF
    -- params: F_arbre: in Arbre
    procedure Afficher(F_sgf: in SGF);

    -- procedure Afficher_complet : affiche l'architecture du SGF avec les détails
    -- params: F_sgf: in SGF
    procedure Afficher_complet(F_sgf: in SGF);

    -- procedure Supprimer : supprime un élément du SGF
    -- params: F_arbre: in out Arbre
    --         F_element: in Arbre
    procedure Supprimer(F_arbre: in out Arbre; F_element: in Arbre);
    
    -- procedure Deplacer : déplace un élément du SGF
    -- params: F_arbre: in out Arbre
    --         F_element: in Arbre
    --         F_parent: in Arbre
    procedure Deplacer(F_arbre: in out Arbre; F_element: in Arbre; F_parent: in Arbre);

    -- procedure Archiver : archive un élément du SGF
    -- params: F_arbre: in out Arbre
    --         F_element: in Arbre
    procedure Archiver(F_arbre: in out Arbre; F_element: in Arbre);

end P_SGF;