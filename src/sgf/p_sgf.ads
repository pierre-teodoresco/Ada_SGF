with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_Arbre;


package P_SGF is 

    -- DF: le type Dossier Fichier
    type DF is private;

    -- Arbre de Dossier Fichier
    package Arbre_DF is 
        new P_Arbre(Type_Element => DF);
    use Arbre_DF;

    

private
    type DF_Type is (Dossier, Fichier);
    type DF_Perm is (Lecture, Ecriture, Execution);
    type DF is record
        Nom: Unbounded_String;
        Type: DF_Type;
        Perm: DF_Perm;
        Taille: Integer;
    end record;
end P_SGF;