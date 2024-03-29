with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Arbre;
with P_Chaine; use P_Chaine;

package P_SGF is 
    -- types
    type DF_Flag is (Dossier, Fichier);
    type DF is record
        Nom: Unbounded_String;
        Flag: DF_Flag;
        Perm: Natural;
        Taille: Natural;
    end record;

    type SGF is private;

    type Commandes is (pwd, touch, mkdir, ls, cd, rm, cp, mv, tar);
    type Options is (none, r, l);
    type Commande is record
        Nom: Commandes;
        Option: Options;
        Args: Liste_String;
    end record;

    -- Exceptions
    PATH_NOT_EXISTS: exception;
    DIRECTORY_NOT_EMPTY: exception;
    NOT_A_DIRECTORY: exception;
    NOT_A_FILE: exception;
    
    -- Sous-programmes

    -- fonction Creer : crée la racine du SGF
    -- retourne : SGF
    function Creer return SGF;

    -- procedure Lancer : exécute la commande
    -- params: F_Cmd: in Commande - commande à exécuter
    --        F_sgf: in SGF - SGF sur lequel exécuter la commande
    procedure Lancer(F_sgf: in out SGF; F_cmd: in Commande);

    -- procedure Detruire : détruit le SGF
    -- params: F_sgf: in out SGF
    procedure Detruire(F_sgf: in out SGF);

    -- fonction Chemin_absolu : retourne le chemin absolu d'un élément
    -- params: F_sgf: in SGF
    -- retourne : Unbounded_String
    function Chemin_absolu(F_sgf: in SGF) return Unbounded_String;

private
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

    procedure Afficher_DF_complet(F_df: in DF);
    procedure Afficher_DF_simple(F_df: in DF);

    procedure Afficher_Arbre_SGF_complet is new Arbre_DF.Afficher(Afficher_Contenu => Afficher_DF_complet);
    procedure Afficher_Arbre_SGF_simple is new Arbre_DF.Afficher(Afficher_Contenu => Afficher_DF_simple);

    -- Sous-programmes

    -- fonction Rechercher_sgf : recherche un element à partir d'un chemin
    -- params: F_sgf: in SGF    - SGF dans lequel rechercher
    --         F_chemin: in Unbounded_String    - chemin de l'élément à rechercher
    --         F_est_createur: in Boolean   - si vrai, la commande créera un élément
    -- retourne : Arbre
    function Rechercher_sgf(F_sgf: in SGF; F_chemin: in Unbounded_String; F_est_createur: in Boolean) return Arbre;

    -- fonction Rechercher_via_liste : recherche un élément à partir d'une liste de noms
    -- params: F_sgf: in SGF    - SGF dans lequel rechercher
    --         F_dfs: in Liste_String   - liste des noms des éléments à rechercher
    --         F_est_relatif: in Boolean    - si vrai, le chemin est relatif
    -- retourne : Arbre
    function Rechercher_via_liste(F_sgf: in SGF; F_dfs: in Liste_String; F_est_relatif: in Boolean) return Arbre;

    -- fonction Rechercher_df : recherche un élément à partir d'un chemin absolu
    -- params: F_arbre: in Arbre    - arbre dans lequel rechercher
    --         F_dfs: in Liste_String   - liste des noms des éléments à rechercher
    -- retourne : Arbre
    function Rechercher_df(F_arbre: in Arbre; F_dfs: in Liste_String) return Arbre;

    -- procedure Creer_dossier : crée un dossier dans le SGF
    -- params: F_arbre: in out Arbre    - arbre dans lequel créer le dossier        
    --         F_nom: in Unbounded_String   - nom du dossier
    procedure Creer_dossier(F_arbre: in out Arbre; F_Nom: in Unbounded_String);
    
    -- procedure Creer_fichier : crée un fichier dans le SGF
    -- params: F_arbre: in out Arbre    - arbre dans lequel créer le fichier
    --         F_Nom: in Unbounded_String   - nom du fichier
    procedure Creer_fichier(F_arbre: in out Arbre; F_nom: in Unbounded_String);

    -- procedure Afficher : affiche l'architecture du SGF
    -- params: F_arbre: in Arbre    - arbre à afficher
    procedure Afficher(F_arbre: in Arbre);

    -- procedure Afficher_complet : affiche l'architecture du SGF avec les détails
    -- params: F_arbre: in Arbre    - arbre à afficher
    procedure Afficher_complet(F_arbre: in Arbre);

    -- procedure Changer_repertoire : change le répertoire courant du SGF
    -- params: F_sgf: in out SGF    - SGF à modifier
    --         F_arbre: in Arbre    - arbre vers lequel on veut changer de répertoire
    procedure Changer_repertoire(F_sgf: in out SGF; F_arbre: in Arbre);

    -- procedure Supprimer : supprime un élément du SGF
    -- params: F_arbre: in out Arbre    - arbre à supprimer
    --         F_est_recursif: in Boolean   - suppression récursive ou non
    procedure Supprimer(F_arbre: in out Arbre; F_est_recursif: in Boolean);

    -- procedure Copier : copie un élément du SGF
    -- params: F_arbre: in Arbre     - arbre à copier
    --         F_cible: in out Arbre    - arbre cible
    --         F_est_recursif: in Boolean   - copie récursive ou non
    procedure Copier(F_arbre: in Arbre; F_cible: in out Arbre; F_est_recursif: in Boolean);
    
    -- procedure Deplacer : déplace un élément du SGF
    -- params: F_arbre: in out Arbre    - arbre à déplacer
    --         F_cible: in out Arbre    - arbre cible 
    procedure Deplacer(F_arbre: in out Arbre; F_cible: in out Arbre);

    -- procedure Archiver : archive un élément du SGF   
    -- params: F_arbre: in out Arbre    - arbre à archiver
    --         F_element: in Arbre   - arbre cible
    procedure Archiver(F_arbre: in out Arbre; F_element: in Arbre);

    -- fonction get_chemin_absolu : retourne le chemin absolu du repertoire courant
    -- params: F_arbre: in Arbre    - arbre à partir duquel on veut le chemin absolu
    -- return: Unbounded_String   - chemin absolu
    function get_chemin_absolu(F_arbre: in Arbre) return Unbounded_String;

end P_SGF;