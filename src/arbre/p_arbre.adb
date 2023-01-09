with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

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
            Detruire(F_arbre => F_noeud.all.Fils);

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

    --  procedure Afficher: affiche un arbre
    --  paramètres: F_arbre: arbre à afficher
    procedure Afficher(F_arbre: in Arbre) is
    begin
        if Est_vide(F_arbre) then
            null;
        else
            Afficher_contenu(F_arbre.all.Contenu);
            Afficher(F_arbre.all.Frere);
            Afficher(F_arbre.all.Fils);
        end if;
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

    -- PRIVATE

    -- procedure Afficher_Noeud: affiche un noeud
    -- paramètres: F_noeud: noeud à afficher
    --  procedure Afficher_noeud(F_noeud: in Arbre) is
    --      T_frere: Arbre;
    --      T_fils: Arbre;
    --  begin
    --      -- iterative version
    --      T_frere := F_noeud.all.Frere;
    --      T_fils := F_noeud.all.Fils;
        
    --      -- Valeur
    --      Put("Courrant: ");
    --      Put(F_noeud.all.Valeur, 0);
    --      New_Line;

    --      -- Pere
    --      Put("Pere: ");
    --      if F_noeud.all.Pere = null then
    --          Put("null");
    --      else
    --          Put(F_noeud.all.Pere.all.Valeur, 2);
    --      end if;
    --      New_Line;

    --      -- Frere
    --      Put("Frere: ");
    --      if T_frere = null then
    --          Put("null");
    --      else
    --          while T_frere /= null loop
    --              Put(T_frere.all.Valeur, 2);
    --              T_frere := T_frere.all.Frere;
    --          end loop;
    --      end if;
    --      New_Line;

    --      -- Fils
    --      Put("Fils: ");
    --      if T_fils = null then
    --          Put("null");
    --      else
    --          while T_fils /= null loop
    --              Put(T_fils.all.Valeur, 2);
    --              T_fils := T_fils.all.Frere;
    --          end loop;
    --      end if;
    --      New_Line;

    --      New_Line;
    --      New_Line;

    --  end Afficher_noeud;
end P_Arbre;