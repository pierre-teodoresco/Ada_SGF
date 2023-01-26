package body P_Cli is

    procedure Run is
        input: Unbounded_String;
        System: SGF := P_SGF.Creer;
        nom: Unbounded_String;
        args: Liste_String;
        opt: Unbounded_String;
        cmd: Commande;
    begin
        loop 
            Put(">" & To_String(Chemin_absolu(System)) & "$ ");

            Get_Line(input);
            exit when input = "exit";

            if input = "" then
                null;
            else
                nom := Get_cmd(Separer_commande(input));
                args := Get_args(Separer_commande(input));
                opt := Get_opt(Separer_commande(input));

                cmd.Nom := Commandes'Value(To_String(nom));
                cmd.Args := args;
                cmd.Option := Options'Value(To_String(opt));
                
                Lancer(F_sgf => System, F_cmd => cmd);
            end if;
        end loop;
    --  exception
        --  when Constraint_Error => Put_Line("Commande ou option inconnue");
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
        str: Unbounded_String := Get_liste(F_input, 2);
    begin
        if To_String(str) = "" then
            null;
        elsif To_String(str)(1) = '-' then
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
        if To_String(opt) = "" then
            return To_Unbounded_String("none");
        elsif To_String(opt)(1) = '-' then
            return Unbounded_Slice(opt, 2, Length(opt));
        else
            return To_Unbounded_String("none");
        end if;
    end Get_opt;

end P_Cli;