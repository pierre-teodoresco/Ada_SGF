with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body P_Arbre is

    -- implémentation de Unchecked_Deallocation
    procedure Liberer is
        new Ada.Unchecked_Deallocation (Arbre, Noeud);

    -- implémentation des sous-programmes de P_Arbre

    -- procedure Creer: crée un arbre vide
    -- paramètres: F_arbre: arbre à créer
    procedure Creer(F_arbre: out Arbre) is
    begin
        F_arbre := new Noeud'(Pere => null, Fils => null, Frere => null, Valeur => 0);
    end Creer;

    -- fonction Est_vide: teste si un arbre est vide
    -- paramètres: F_arbre: arbre à tester
    -- résultat: vrai si l'arbre est vide, faux sinon
    function Est_vide(F_arbre: in Arbre) return Boolean is
    begin
        return F_arbre = null;
    end Est_vide;

    -- fonction Pere: retourne le père d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le père
    -- résultat: père du noeud
    function Pere(F_noeud: in Arbre) return Arbre is
    begin
        return F_noeud.all.Pere;
    end Pere;

    -- fonction Fils: retourne le fils d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le fils
    -- résultat: fils du noeud
    function Fils(F_noeud: in Arbre) return Arbre is
    begin
        return F_noeud.all.Fils;
    end Fils;

    -- fonction Frere: retourne le frère d'un noeud
    -- paramètres: F_noeud: noeud dont on veut le frère
    -- résultat: frère du noeud
    function Frere(F_noeud: in Arbre) return Arbre is
    begin
        return F_noeud.all.Frere;
    end Frere;

    -- procedure Ajouter: ajoute un fils à la fin de la liste des fils d'un noeud
    -- paramètres: F_noeud: noeud auquel on veut ajouter un fils
    --             F_fils: fils à ajouter
    procedure Ajouter(F_noeud: in out Arbre; F_fils: in Arbre) is
        T_noeud: Arbre;
    begin
        if F_noeud.all.Fils = null then
            -- Premier fils
            F_noeud.all.Fils := F_fils;
        else
            -- On se place sur le dernier fils
            T_noeud := F_noeud.all.Fils;
            while T_noeud.all.Frere /= null loop
                T_noeud := T_noeud.all.Frere;
            end loop;

            -- Ajout du fils
            T_noeud.all.Frere := F_fils;
        end if;

        -- Mis à jour du père du fils
        F_fils.all.Pere := F_noeud;
    end Ajouter;

    -- procedure Supprimer: supprime un noeud et ses fils
    -- paramètres: F_noeud: noeud à supprimer
    procedure Supprimer(F_noeud: in out Arbre);

    -- procedure Afficher: affiche un arbre
    -- paramètres: F_arbre: arbre à afficher
    procedure Afficher(F_arbre: in Arbre) is
    begin
        if Est_vide(F_arbre) then
            Put_Line("Arbre vide");
        else
            Put(F_arbre.all.Valeur, 4);
            Afficher(F_arbre.all.Frere);
            New_Line;
            Afficher(F_arbre.all.Fils);
        end if;
    end Afficher;

    -- procedure Detruire: détruit un arbre
    -- paramètres: F_arbre: arbre à détruire
    procedure Detruire(F_arbre: in out Arbre);

end P_Arbre;