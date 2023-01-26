with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with P_SGF; use P_SGF;
with P_Chaine; use P_Chaine;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
package P_Cli is

    -- procedure Run: execute l'interface de commande
   procedure Run;

    -- procedure Run_test: execute les tests
    -- params: F_input: Unbounded_String    - input utilsateur
    procedure Run_test(F_input: in Unbounded_String);
    
private

    -- fonction Get_cmd: renvoie la commande saisie par l'utilisateur
    -- params: F_input: Liste_String    - input utilsateur
    -- return: Unbounded_String - commande dans la liste
    function Get_cmd(F_input: in Liste_String) return Unbounded_String;

    -- fonction Get_args: renvoie les arguments de la commande saisie par l'utilisateur
    -- params: F_input: Liste_String    - input utilsateur
    -- return: Liste_String - arguments dans la liste
    function Get_args(F_input: in Liste_String) return Liste_String;
    
    -- fonction Get_opt: renvoie l'option de la commande saisie par l'utilisateur
    -- params: F_input: Liste_String    - input utilsateur
    -- return: Unbounded_String - option dans la liste
    function Get_opt(F_input: in Liste_String) return Unbounded_String;

end P_Cli;