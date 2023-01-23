with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package P_Chaine is

    -- Types
    type Noeud_String;
    type Liste_String is access Noeud_String;
    type Noeud_String is record
        Valeur: Unbounded_String;
        Suivant: Liste_String;
    end record;

    -- Sous-programmes

    -- fonction Init_liste : initialise une liste de chaines
    -- retourne : Liste_String
    function Init_liste return Liste_String;

    -- fonction Taille_liste : retourne la taille d'une liste de chaines
    -- params: F_liste: in Liste_String    - liste à tester
    -- retourne : Integer
    function Taille_liste(F_liste: in Liste_String) return Integer;

    -- procedure Detruire_liste : détruit une liste de chaines
    -- params: F_liste: in out Liste_String    - liste à détruire
    procedure Detruire_liste(F_liste: in out Liste_String);

    -- fonction Nom_via_chemin : retourne le nom d'un élément via un chemin
    -- params: F_chemin: in Unbounded_String    - chemin de l'élément
    -- retourne : Unbounded_String
    function Nom_via_chemin(F_chemin: in Unbounded_String) return Unbounded_String;

    -- fonction Est_absolu : indique si un chemin est absolu
    -- params: F_chemin: in Unbounded_String    - chemin à tester
    -- retourne : Boolean
    function Est_absolu(F_chemin: in Unbounded_String) return Boolean;

    -- procedure Separer_chemin : sépare un chemin en un tableau de chaines
    -- params: F_chemin: in Unbounded_String        - chemin à séparer
    -- retourne : Liste_String
    function Separer_chemin(F_chemin: in Unbounded_String) return Liste_String;

    -- procedure pop : supprime le dernier élément d'une liste de chaines
    -- params: F_liste: in out Liste_String    - liste à modifier
    procedure Pop(F_liste: in out Liste_String);

    -- tests

    -- procedure afficher_liste : affiche une liste de chaines
    -- params: F_liste: in Liste_String - liste à afficher
    procedure Afficher_liste(F_liste: in Liste_String);

private
    
    -- procedure Ajouter_liste : ajoute une chaine en fin de liste
    -- params: F_liste: in out Liste_String    - liste à modifier
    --         F_chaine: in Unbounded_String  - chaine à ajouter
    procedure Ajouter_liste(F_liste: in out Liste_String; F_chaine: in Unbounded_String);

end P_Chaine;