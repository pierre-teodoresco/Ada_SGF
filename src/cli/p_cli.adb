with Ada.Text_IO; use Ada.Text_IO;

package body P_Cli is

    procedure Run(F_input: in Unbounded_String) is
        System: SGF := P_SGF.Creer;
        input: Liste_String := Separer_commande(F_input);
        cmd: Unbounded_String := Get_cmd(input);
        args: Liste_String := Get_args(input);
        opt: Unbounded_String := Get_opt(input);
        the_cmd: Commande;
    begin
        loop            
            the_cmd.Nom := Commandes'Value(To_String(cmd));
            the_cmd.Args := args;
            the_cmd.Option := Options'Value(To_String(opt));
            
            Lancer(F_sgf => System, F_cmd => the_cmd);
        exit when To_String(cmd) = "exit";
        end loop;
    exception
        when CONSTRAINT_ERROR => Put_Line("La commande ou les options sont incorrectes.");
    end Run;

    procedure Run_test(F_input: in Unbounded_String) is
        input: Liste_String := Separer_commande(F_input);
        cmd: Unbounded_String := Get_cmd(input);
        args: Liste_String := Get_args(input);
        opt: Unbounded_String := Get_opt(input);
    begin
        Put_Line("Commande: " & To_String(cmd));
        Put_Line("Options: " & To_String(opt));
        Put_Line("Arguments: ");
        Afficher_liste(args);
    end Run_test;

    -- PRIVATE

    function Get_cmd(F_input: in Liste_String) return Unbounded_String is
    begin
        return Get_liste(F_input, 1);
    end Get_cmd;

    function Get_args(F_input: in Liste_String) return Liste_String is
        args: Liste_String := Init_liste;
        taille: Integer := Taille_liste(F_input);
    begin
        if To_String(Get_liste(F_input, 2))(1) = '-' then
            -- l'input a des options
            -- on parcours de l'element 3 a la fin de la liste
            for i in 3 .. taille loop
                -- on ajoute l'element a la liste des args
                Ajouter_liste(args, Get_liste(F_input, i));
            end loop;
        else
            -- l'input n'a pas d'options
            -- on parcours de l'element 2 a la fin de la liste
            for i in 2 .. taille loop
                -- on ajoute l'element a la liste des args
                Ajouter_liste(args, Get_liste(F_input, i));
            end loop;
        end if;
        
        return args;
    end Get_args;

    function Get_opt(F_input: in Liste_String) return Unbounded_String is
        opt: Unbounded_String;
    begin
        opt := Get_liste(F_input, 2);
        if To_String(opt)(1) = '-' then
            return opt;
        else
            return To_Unbounded_String("none");
        end if;
    end Get_opt;

end P_Cli;