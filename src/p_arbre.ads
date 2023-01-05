package P_Arbre is
    -- types
    type Arbre is private;

    -- Sous-programmes

    -- procedure Creer: crée un arbre vide
    -- paramètres: F_arbre: arbre à créer
    procedure Creer(F_arbre: out Arbre) with Post => Est_vide(F_arbre);

    -- fonction Est_vide: teste si un arbre est vide
    -- paramètres: F_arbre: arbre à tester
    -- résultat: vrai si l'arbre est vide, faux sinon
    function Est_vide(F_Arbre: in Arbre) return Boolean;

    -- fonction Pere: retourne le père d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le père
    -- résultat: père du noeud
    function Pere(F_noeud: in Arbre) return Arbre;

    -- fonction Fils: retourne le fils d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le fils
    -- résultat: fils du noeud
    function Fils(F_noeud: in Arbre) return Arbre;

    -- fonction Frere: retourne le frère d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le frère
    -- résultat: frère du noeud
    function Frere(F_noeud: in Arbre) return Arbre;

    -- procedure Ajouter: ajoute un fils à la fin de la liste des fils d'un noeud
    -- paramètres: F_noeud: noeud auquel on veut ajouter un fils
    --             F_fils: fils à ajouter
    procedure Ajouter(F_noeud: in out Arbre; F_fils: in Arbre);

    -- procedure Supprimer: supprime un noeud et ses fils
    -- paramètres: F_noeud: noeud à supprimer
   -- procedure Supprimer(F_noeud: in out Arbre);

    -- procedure Afficher: affiche un arbre
    -- paramètres: F_arbre: arbre à afficher
    procedure Afficher(F_arbre: in Arbre);

    -- procedure Detruire: détruit un arbre
    -- paramètres: F_arbre: arbre à détruire
    procedure Detruire(F_arbre: in out Arbre);

    -- TESTS

    procedure Inserer(F_arbre: in out Arbre; F_valeur: in Integer);
    
private
    type Noeud;
    type Arbre is access Noeud;
    type Noeud is record
        Pere: Arbre;
        Fils: Arbre;
        Frere: Arbre;
        Valeur: Integer; -- val de tests
    end record;
end P_Arbre;