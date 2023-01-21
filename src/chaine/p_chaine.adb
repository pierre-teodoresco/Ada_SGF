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
        return To_String(F_chemin)(1) = '/';
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
            Ajouter_liste(liste, To_Unbounded_String(""));
            chemin := Unbounded_Slice(chemin, 2, Length(chemin));
        end if;

        Put_Line(To_String(chemin));

        -- parcourir la chaine et ajouter à chaque / un élément dans la liste
        while chemin /= To_Unbounded_String("") loop
            if To_String(chemin)(i) = '/' then
                Put_Line(To_String(Unbounded_Slice(chemin, 1, i-1)));
                -- slice de la chaine de 1 à i-1
                elem := Unbounded_Slice(chemin, 1, i-1);
                -- ajouter chemin à la liste
                Ajouter_liste(liste, elem);
                -- slice de la chaine de i+1 à la fin
                chemin := Unbounded_Slice(chemin, i+1, Length(chemin));
                -- remettre i à 1
                i := 1;
                Put_Line(To_String(chemin));
            else
                i := i + 1;
            end if;
        end loop;

        -- ajouter le dernier élément
        Ajouter_liste(liste, chemin);

        return liste;

    end Separer_chemin;

    procedure Afficher_liste(F_liste: in Liste_String) is 
    begin
        if F_liste /= null then
            Put_Line(To_String(F_liste.Valeur));
            afficher_liste(F_liste.Suivant);
        end if;
    end afficher_liste;

    -- Private

    procedure Ajouter_liste(F_liste: in out Liste_String; F_chaine: in Unbounded_String) is
    begin
        if F_liste = null then
            F_liste := new Noeud_String'(Valeur => F_chaine, Suivant => null);
        else
            Ajouter_liste(F_liste.Suivant, F_chaine);
        end if;
    end Ajouter_liste;

end P_Chaine;