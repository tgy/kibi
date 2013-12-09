let main () =
    print_string "<Wordchecker> Initialization...";

    let output_web = "dictionnaries/output.html" in

    let read_file addr =
        let file = open_in addr in
        let l = ref [] in
        try
            while true do
                l := (input_line file)::!l;
            done;
            close_in file;
            String.concat "\n" (Wordchecker.reverse !l)
        with End_of_file ->
            close_in file; String.concat "\n" (Wordchecker.reverse !l)
    in

    let foo_html = read_file "dictionnaries/foo"
    and bar_html = read_file "dictionnaries/bar"
    and str = read_file "dictionnaries/input.txt" in
    
    let l = Wordchecker.cut_text str in
    let dico = Wordchecker.detect_language l in
    
    print_string "\n<Rort> I got the text :). It's in ";
    
    if (dico.Wordchecker.lang = Lang.Fr) then
        print_string "english !"
    else
        print_string "frensh !";
    
    let dicophon  =
        let rec aux l1 l2 = match (l1,l2) with
        | (_,[]) | ([],_) ->
                raise (Failure "Error in module lang.ml")
        | (e::l, f::m) when (e = dico.Wordchecker.lang) ->
                Wordchecker.get_dictionnary f
        | (_::l, _::m) ->
                aux l m
        in
        aux Lang.languages Lang.urls_phone
    in

    let errors = ref (Wordchecker.send_errors dico dicophon l) in

    print_string "\n<Wordchecker> Dans la phrase, il y a ";
    print_int (List.length !errors);
    print_string " erreurs.";
    
    let rec iter = function
        | [] ->	()
        | (e,l)::l2	->
                print_string "\nError:";
                print_string e;
                print_string " ->";
                List.iter (fun s -> print_char ' '; print_string s) l;
                iter l2
    in

    iter !errors;

    let replace s s1 s2 =
				Printf.printf "%s\n" s1;
        let i = ref 0
        and check = ref 0
        and start = ref 0 
        and newS = ref s in
        while (!i < String.length s) do
            if (Wordchecker.lowercase_char s.[!i]) = s1.[!check] then
            (
                (if !check = 0 then start := !i);
                check := !check + 1;
                if !check >= String.length s1 then
                (
										Printf.printf "OK";
                    newS := String.concat "" 
                    [
                        String.sub s 0 !start;
                        s2;
                        String.sub s (!start + !check)
                            (String.length s - !start - !check)
                    ];
                    i := String.length s
                )
            )
            else
                check := 0;
            i := !i + 1
        done;
        !newS
    in

    let newS = ref str in
    
    List.iter (function (s,l) -> newS := replace !newS s
        (
            String.concat ""
            [
                "<span class=\"wrongWord\">";
                s;
                "<span class=\"corrections\">";
                String.concat "<br/>" l;
                "</span>";
                "</span>"
            ]
        )
    ) !errors;

    let save_html addr =
        let file = open_out addr in
        output_string file foo_html;
        output_string file !newS;
        output_string file bar_html;
        close_out file
    in

    save_html output_web;

    (*let my_dico = Wordcheckerdev.create_dictionnary "dictionnaries/fr-FR.dic" Lang.Fr Wordcheckerdev.phonetic in
    Wordcheckerdev.save_dictionnary my_dico "dictionnaries/fr-FR.phone";;*)
    (*let my_dico = Wordchecker.get_dictionnary "dictionnaries/fr-FR.phone";;*)

    print_string "\n"

let _ = main ()
