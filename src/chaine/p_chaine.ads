with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package P_Chaine is

    -- Types
    type Noeud;
    type Liste_String is access Noeud;
    type Noeud is record
        Valeur: Unbounded_String;
        Suivant: Liste_String;
    end record;

    -- Sous-programmes

    -- fonction Nom_via_chemin : retourne le nom d'un élément via un chemin
    -- params: F_chemin: in Unbounded_String    - chemin de l'élément
    -- retourne : Unbounded_String
    function Nom_via_chemin(F_chemin: in Unbounded_String) return Unbounded_String;

    -- procedure Separer_chemin : sépare un chemin en un tableau de chaines
    -- params: F_chemin: in Unbounded_String        - chemin à séparer
    --         F_liste_chemin: in out Liste_String  - liste de chaines résultat
    procedure Separer_chemin(F_chemin: in Unbounded_String; F_liste_chemin: in out Liste_String);

    -- tests

    -- procedure afficher_liste : affiche une liste de chaines
    -- params: F_liste: in Liste_String - liste à afficher
    procedure afficher_liste(F_liste: in Liste_String);

end P_Chaine;