with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body P_Arbre is

    -- implémentation de Unchecked_Deallocation
    procedure Liberer is
        new Ada.Unchecked_Deallocation(Noeud, Arbre);

    -- implémentation des sous-programmes de P_Arbre

    -- fonction Creer: crée un arbre
    -- paramètres: T_element: élément à mettre dans la racine de l'arbre
    -- résultat: arbre créé
    function Creer(T_element: in Type_Element) return Arbre is
    begin
        return new Noeud'(Pere => null, Fils => null, Frere => null, Contenu => T_element);
    end Creer;

    -- fonction Est_vide: teste si un arbre est vide
    -- paramètres: F_arbre: arbre à tester
    -- résultat: vrai si l'arbre est vide, faux sinon
    function Est_vide(F_arbre: in Arbre) return Boolean is
    begin
        return F_arbre = null;
    end Est_vide;

    -- fonction Est_feuille: teste si un noeud est une feuille
    -- paramètres: F_noeud: noeud à tester
    -- résultat: vrai si le noeud est une feuille, faux sinon
    function Est_feuille(F_noeud: in Arbre) return Boolean is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            return F_noeud.all.Fils = null;
        end if;
    exception
        when EMPTY_TREE => 
            Put_Line("Erreur: le noeud est vide");
            return false;
    end Est_feuille;

    -- fonction Est_racine: teste si un noeud est la racine
    -- paramètres: F_noeud: noeud à tester
    -- résultat: vrai si le noeud est la racine, faux sinon
    function Est_racine(F_noeud: in Arbre) return Boolean is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            return F_noeud.all.Pere = null;
        end if;
    exception
        when EMPTY_TREE => 
            Put_Line("Erreur: le noeud est vide");
            return false;
    end Est_racine;

    -- fonction clone : clone un arbre
    -- paramètres: F_arbre: arbre à cloner
    -- résultat: arbre cloné
    function clone(F_arbre: in Arbre) return Arbre is
        T_noeud: Arbre;
    begin
        if Est_vide(F_arbre) then
            return null;
        else
            T_noeud := Creer(F_arbre.all.Contenu);
            T_noeud.all.Fils := clone(F_arbre.all.Fils);
            return T_noeud;
        end if;
    end clone;

    -- fonction Pere: retourne le père d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le père
    -- résultat: père du noeud
    function Pere(F_noeud: in Arbre) return Arbre is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            return F_noeud.all.Pere;
        end if;
    end Pere;

    -- fonction Fils: retourne le fils d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le fils
    -- résultat: fils du noeud
    function Fils(F_noeud: in Arbre) return Arbre is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            return F_noeud.all.Fils;
        end if;
    end Fils;

    -- fonction Frere: retourne le frère d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le frère
    -- résultat: frère du noeud
    function Frere(F_noeud: in Arbre) return Arbre is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            return F_noeud.all.Frere;
        end if;
    end Frere;

    -- fonction Contenu: retourne le contenu d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le contenu
    -- résultat: contenu du noeud
    function Contenu(F_noeud: in Arbre) return Type_Element is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            return F_noeud.all.Contenu;
        end if;
    end Contenu;

    -- procedure Modifier_contenu: modifie le contenu d'un noeud
    -- paramètres: F_noeud: noeud dont on veut modifier le contenu
    --             F_element: nouveau contenu du noeud
    procedure Modifier_contenu(F_noeud: in out Arbre; F_element: in Type_Element) is
    begin
        if Est_vide(F_noeud) then
            raise EMPTY_TREE;
        else
            F_noeud.all.Contenu := F_element;
        end if;
    end Modifier_contenu;

    -- procedure Ajouter: ajoute un fils à la fin de la liste des fils d'un noeud
    -- paramètres: F_noeud: noeud auquel on veut ajouter un fils
    --             F_element: element à ajouter
    procedure Ajouter(F_noeud: in out Arbre; F_element: in Type_Element) is
        T_noeud: Arbre;
    begin
        if F_noeud.all.Fils = null then
            -- Premier fils
            F_noeud.all.Fils := new Noeud'(Pere => F_noeud, Fils => null, Frere => null, Contenu => F_element);
        else
            -- On se place sur le dernier fils
            T_noeud := F_noeud.all.Fils;
            while T_noeud.all.Frere /= null loop
                T_noeud := T_noeud.all.Frere;
            end loop;

            -- Ajout du fils
            T_noeud.all.Frere := new Noeud'(Pere => F_noeud, Fils => null, Frere => null, Contenu => F_element);
        end if;
    end Ajouter;

    -- procedure Supprimer: supprime un noeud et ses fils
    -- paramètres: F_noeud: noeud à supprimer
    procedure Supprimer(F_noeud: in out Arbre) is
        T_noeud: Arbre;
    begin
        if Est_vide(F_noeud) then
            null;
        else
            -- Verifier si F_noeud est un "fils direct" (pas un frère)
            if F_noeud.all.Pere.all.Fils = F_noeud then
                -- Suppression du lien paternel
                F_noeud.all.Pere.all.Fils := F_noeud.all.Frere;
            else
                -- On se place sur le noeud précédent F_noeud
                T_noeud := F_noeud.all.Pere.all.Fils;
                while T_noeud.all.Frere /= F_noeud loop
                    T_noeud := T_noeud.all.Frere;
                end loop;

                -- Suppression du lien fraternel
                T_noeud.all.Frere := F_noeud.all.Frere;
            end if;

            -- Suppression des fils
            Detruire(F_noeud.all.Fils);

            -- Suppression du noeud
            Liberer(F_noeud);
        end if;
    end Supprimer;

    -- procedure Deplacer: déplace un noeud et ses fils
    -- paramètres: F_noeud: noeud à déplacer
    --             F_nouveau_pere: nouveau père du noeud
    procedure Deplacer(F_noeud: in out Arbre; F_nouveau_pere: in out Arbre) is
        Frere_prec: Arbre;
        T_noeud: Arbre;
    begin
        -- Verifier si F_noeud est un fils direct
        if F_noeud.all.Pere.all.Fils = F_noeud then
            -- Modifier le fils du pere
            F_noeud.all.Pere.all.Fils := F_noeud.all.Frere;
        else
            -- Modifier les freres
            Frere_prec := F_noeud.all.Pere.all.Fils;
            -- Trouver le frères précédents 
            while Frere_prec.all.Frere /= F_noeud loop
                Frere_prec := Frere_prec.all.Frere;
            end loop;
            -- Retirer F_noeud de la liste des frères
            Frere_prec.all.Frere := F_noeud.all.Frere;
        end if;

        -- Retirer le frère de F_noeud
        F_noeud.all.Frere := null;

        -- Mettre à F_nouveau pere, F_noeud comme fils
        if F_nouveau_pere.all.Fils /= null then
            T_noeud := F_nouveau_pere.all.Fils;
            while T_noeud.all.Frere /= null loop
                T_noeud := T_noeud.all.Frere;
            end loop;
            T_noeud.all.Frere := F_noeud;
        else
            F_nouveau_pere.all.Fils := F_noeud;
        end if;

        -- Modifier le pere de F_noeud
        F_noeud.all.Pere := F_nouveau_pere;
    end Deplacer;

    -- procedure Copier: copie un arbre
    -- paramètres: F_arbre: arbre à copier
    --             F_nouveau_pere: nouveau père du noeud
    procedure Copier(F_arbre: in Arbre; F_nouveau_pere: in out Arbre) is
    begin
        if Est_vide(F_arbre) then
            raise EMPTY_TREE;
        else
            Ajouter(F_nouveau_pere, F_arbre.all.Contenu);
        end if;
    end Copier;

    -- procedure Rechercher: recherche un élément dans un arbre
    -- paramètres: F_arbre: arbre dans lequel on recherche
    --             F_element: élément à rechercher
    -- résultat: noeud contenant l'élément recherché
    function Rechercher(F_arbre: in Arbre; F_element: in Type_Element) return Arbre is
        T_noeud: Arbre;
    begin
        if Est_vide(F_arbre) then
            return null;
        else
            if Egal(F_arbre.all.Contenu, F_element) then
                return F_arbre;
            else
                T_noeud := Rechercher(F_arbre.all.Fils, F_element);
                if T_noeud /= null then
                    return T_noeud;
                else
                    return Rechercher(F_arbre.all.Frere, F_element);
                end if;
            end if;
        end if;
    end Rechercher;

    --  procedure Afficher: affiche un arbre
    --  paramètres: F_arbre: arbre à afficher
    procedure Afficher(F_arbre: in Arbre) is
        T_noeud: Arbre;
    begin
        -- parcours des fils du noeud
        T_noeud := F_arbre.all.Fils;
        while T_noeud /= null loop
            Afficher_contenu(T_noeud.all.Contenu);
            T_noeud := T_noeud.all.Frere;
        end loop;
    end Afficher;

    -- procedure Detruire: détruit un arbre
    -- paramètres: F_arbre: arbre à détruire
    procedure Detruire(F_arbre: in out Arbre) is
    begin
        if Est_vide(F_arbre) then
            null;
        else
            Detruire(F_arbre.all.Fils);
            Detruire(F_arbre.all.Frere);
            Liberer(F_arbre);
        end if;
    end Detruire;
end P_Arbre;