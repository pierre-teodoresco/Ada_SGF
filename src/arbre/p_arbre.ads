generic
    type Type_Element is private;
package P_Arbre is
    -- types
    type Arbre is private;

    -- Sous-programmes

    -- fonction Creer: crée un arbre
    -- paramètres: T_element: type des éléments de l'arbre
    -- résultat: arbre créé
    function Creer(T_element: in Type_Element) return Arbre; 
    
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
    --             F_element: element à ajouter 
    procedure Ajouter(F_noeud: in out Arbre; F_element: in Type_Element);

    -- procedure Supprimer: supprime un noeud et ses fils
    -- paramètres: F_noeud: noeud à supprimer
    procedure Supprimer(F_noeud: in out Arbre);

    -- procedure Deplacer: déplace un noeud et ses fils
    -- paramètres: F_noeud: noeud à déplacer
    --             F_nouveau_pere: nouveau père du noeud
    procedure Deplacer(F_noeud: in out Arbre; F_nouveau_pere: in out Arbre);

    -- fonction Rechercher: recherche un élément dans un arbre
    -- paramètres: F_arbre: arbre dans lequel on recherche
    --             F_element: élément à rechercher
    -- résultat: noeud contenant l'élément recherché, ou noeud vide si l'élément n'est pas trouvé
    generic
        with function Egal(F_element1: in Type_Element; F_element2: in Type_Element) return Boolean;
    function Rechercher(F_arbre: in Arbre; F_element: in Type_Element) return Arbre;

    -- procedure Afficher: affiche un arbre
    -- paramètres: F_arbre: arbre à afficher
    generic
        with procedure Afficher_contenu(F_contenu: in Type_Element);
    procedure Afficher(F_arbre: in Arbre);

    -- procedure Detruire: détruit un arbre
    -- paramètres: F_arbre: arbre à détruire
    procedure Detruire(F_arbre: in out Arbre);
    
private
    type Noeud;
    type Arbre is access Noeud;
    type Noeud is record
        Pere: Arbre;
        Fils: Arbre;
        Frere: Arbre;
        Contenu: Type_Element;
    end record;
end P_Arbre;