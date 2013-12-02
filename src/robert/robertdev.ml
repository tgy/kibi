let classic dictionnary (str:string) (lang:Lang.language) =
    Hashtbl.add dictionnary str str

let phonetic dictionnary (str:string) (lang:Lang.language) =
    Hashtbl.add dictionnary (Soundex.soundex str lang) str

let save_dictionnary (dico:Robert.dictionnary) url =
    let file = open_out_bin url in
    Marshal.to_channel file dico [];
    close_out file

let rec create_hshtb (url:string) (lang:Lang.language) insertFunc =
    let stream = open_in url 
    and dictionnary = Hashtbl.create 31 in
    let _ = input_line stream
    and line = ref "" in
    try
        while true do
            line := input_line stream;
            (*print_string "\"";
            print_string !line;
            print_string "\"\n";*)
            line := String.sub !line 0 (String.length !line -1);
            insertFunc dictionnary !line lang;
        done;
        dictionnary;
    with End_of_file -> close_in stream; dictionnary

let create_dictionnary url (lang:Lang.language) insertFunc =
    (
        {
            Robert.hashtbl = (create_hshtb url lang insertFunc);
            Robert.lang = lang
        } : Robert.dictionnary
    )

