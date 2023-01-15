with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package P_Chemin is

    -- permet d'à partir d'un chemin sous forme de unbounded_string de créer une liste de DF 
    -- qui seront après utilisées pour créer/chercher un fichier ou un dossier

    -- objectif : vérifier que l'arboresence existe
    -- créer ou intéragir avec le dernier ficher de l'arborésence

    -- couper les '/' de chacun des éléments de l'arborescence pour créer les DF

    function Split (Chemin : Unbounded_String) return List_String;

end P_Chemin;