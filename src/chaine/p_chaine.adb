package body P_Chaine is

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

    procedure Separer_chemin(F_chemin: in Unbounded_String; F_liste_chemin: in out Liste_String) is
        chemin: Unbounded_String;
        nom: Unbounded_String;
    begin
        -- parcourir le chemin à l'envers jusqu'à trouver un /
        for i in reverse 1..Length(F_chemin) loop
            if To_String(F_chemin)(i) = '/' then
                chemin := Unbounded_Slice(F_chemin, 1, i-1);
                nom := Unbounded_Slice(F_chemin, i+1, Length(F_chemin));
                F_liste_chemin := new Noeud'(Valeur => nom, Suivant => F_liste_chemin);
                Separer_chemin(chemin, F_liste_chemin);
                exit;
            end if;
        end loop;
    end Separer_chemin;

    procedure afficher_liste(F_liste: in Liste_String) is 
    begin
        if F_liste /= null then
            Put_Line(To_String(F_liste.Valeur));
            afficher_liste(F_liste.Suivant);
        end if;
    end afficher_liste;


end P_Chaine;