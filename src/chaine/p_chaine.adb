package body P_Chaine is

    -- implémentation de Unchecked_Deallocation
    procedure Liberer is
        new Ada.Unchecked_Deallocation(Noeud_String, Liste_String);

    function Init_liste return Liste_String is
    begin
        return null;
    end Init_liste;

    function Taille_liste(F_liste: in Liste_String) return Integer is
    begin
        if F_liste = null then
            return 0;
        else
            return Taille_liste(F_liste.Suivant) + 1;
        end if;
    end Taille_liste;
    
    procedure Ajouter_liste(F_liste: in out Liste_String; F_chaine: in Unbounded_String) is
    begin
        if F_liste = null then
            F_liste := new Noeud_String'(Valeur => F_chaine, Suivant => null);
        else
            Ajouter_liste(F_liste.Suivant, F_chaine);
        end if;
    end Ajouter_liste;

    function Get_liste(F_liste: in Liste_String; F_indice: in Integer) return Unbounded_String is
    begin
        if F_liste = null then
            raise LISTE_VIDE;
        elsif F_indice > Taille_liste(F_liste) then
            raise OUT_OF_BOUNDS;
        else
            if F_indice = 1 then
                return F_liste.Valeur;
            else
                return Get_liste(F_liste.Suivant, F_indice - 1);
            end if;
        end if;
    exception
        when LISTE_VIDE =>
            return To_Unbounded_String("");
        when OUT_OF_BOUNDS =>
            return To_Unbounded_String("");
    end Get_liste;

    procedure Detruire_liste(F_liste: in out Liste_String) is 
    begin
        if F_liste /= null then
            Detruire_liste(F_liste.Suivant);
            Liberer(F_liste);
        end if;
    end Detruire_liste;

    function Nom_via_chemin(F_chemin: in Unbounded_String) return Unbounded_String is
    begin
        -- parcourir le chemin à l'envers jusqu'à trouver un /
        for i in reverse 1..Length(F_chemin) loop
            if To_String(F_chemin)(i) = '/' then
                return Unbounded_Slice(F_chemin, i+1, Length(F_chemin));
            end if;
        end loop;
        -- si on ne trouve pas de /, on retourne le chemin
        return F_chemin;
    end Nom_via_chemin;

    function Est_absolu(F_chemin: in Unbounded_String) return Boolean is
    begin
        if Length(F_chemin) = 0 then
            return False;
        else
            return To_String(F_chemin)(1) = '/';
        end if;
    end Est_absolu;

    function Separer_chemin(F_chemin: in Unbounded_String) return Liste_String is
        elem: Unbounded_String;
        chemin: Unbounded_String := F_chemin;
        liste: Liste_String;
        i: Integer := 1;
    begin
        -- si le chemin est absolu ajouter une chaine vide en premier élément
        -- et enlever le premier /
        if Est_absolu(chemin) then
            Ajouter_liste(liste, To_Unbounded_String("/"));
            chemin := Unbounded_Slice(chemin, 2, Length(chemin));
        end if;

        -- parcourir la chaine et ajouter à chaque / un élément dans la liste
        while chemin /= To_Unbounded_String("") and then i <= Length(chemin) loop
            if To_String(chemin)(i) = '/' then
                -- slice de la chaine de 1 à i-1
                elem := Unbounded_Slice(chemin, 1, i-1);
                -- ajouter chemin à la liste
                Ajouter_liste(liste, elem);
                -- slice de la chaine de i+1 à la fin
                chemin := Unbounded_Slice(chemin, i+1, Length(chemin));
                -- remettre i à 1
                i := 1;
            else
                i := i + 1;
            end if;
        end loop;

        -- ajouter le dernier élément
        Ajouter_liste(liste, chemin);

        return liste;

    end Separer_chemin;

    function Separer_commande(F_commande: in Unbounded_String) return Liste_String is
        elem: Unbounded_String;
        commande: Unbounded_String := F_commande;
        liste: Liste_String;
        i: Integer := 1;
    begin
        -- parcourir la chaine et ajouter à chaque espace un élément dans la liste
        while commande /= To_Unbounded_String("") and then i <= Length(commande) loop
            if To_String(commande)(i) = ' ' then
                -- slice de la chaine de 1 à i-1
                elem := Unbounded_Slice(commande, 1, i-1);
                -- ajouter chemin à la liste
                Ajouter_liste(liste, elem);
                -- slice de la chaine de i+1 à la fin
                commande := Unbounded_Slice(commande, i+1, Length(commande));
                -- remettre i à 1
                i := 1;
            else
                i := i + 1;
            end if;
        end loop;

        -- ajouter le dernier élément
        Ajouter_liste(liste, commande);

        return liste;

    end Separer_commande;

    procedure Pop_back(F_liste: in out Liste_String) is
    begin
        if F_liste /= null then
            if F_liste.Suivant /= null then
                Pop_back(F_liste.Suivant);
            else
                Liberer(F_liste);
                F_liste := null;
            end if;
        end if;
    end Pop_back;

    procedure Pop_front(F_liste: in out Liste_String) is
    begin
        if F_liste /= null then
            F_liste := F_liste.Suivant;
        end if;
    end Pop_front;

    procedure Afficher_liste(F_liste: in Liste_String) is 
    begin
        if F_liste /= null then
            Put_Line(To_String(F_liste.Valeur));
            afficher_liste(F_liste.Suivant);
        end if;
    end afficher_liste;

end P_Chaine;