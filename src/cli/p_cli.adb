package body P_Cli is

    function Get_cmd(F_input: in Liste_String) return Unbounded_String is
    begin
        return Get_liste(F_input, 1);
    end Get_cmd;

    function Get_args(F_input: in Liste_String) return Liste_String is
        args: Liste_String := Init_liste;
        taille: Integer := Taille_liste(F_input);
    begin
        if To_String(Get_liste(F_input, 2))(1) = "-" then
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
        if To_String(opt)(1) = "-" then
            return opt;
        else
            return To_Unbounded_String("none");
        end if;
    end Get_opt;

end P_Cli;