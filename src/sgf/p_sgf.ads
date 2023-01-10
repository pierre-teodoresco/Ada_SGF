with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


package P_SGF is 

    -- DF: le type Dossier Fichier
    type DF_Type is (Dossier, Fichier);
    type DF_Perm is (Lecture, Ecriture, Execution);
    type DF is record
        Nom: Unbounded_String;
        Type: DF_Type;
        Perm: DF_Perm;
        Taille: Integer;
    end record;

    -- Arbre de Dossier Fichier
end P_SGF;