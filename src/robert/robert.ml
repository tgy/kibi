type dictionnary = {
    hashtbl : (string, string) Hashtbl.t;
    lang : Lang.language
}

let get_dictionnary url =
    let file = open_in_bin url in
    let d = (Marshal.from_channel file : dictionnary) in
    close_in file;
    d

let find_all (dico:dictionnary) str =
    Hashtbl.find_all dico.hashtbl str

let exists (dico:dictionnary) str =
    Hashtbl.mem dico.hashtbl str

(* /!\ Lève une exception si l'élément n'est pas présent *)
let find (dico:dictionnary) str =
    Hashtbl.find dico.hashtbl str

let rec reverse l = 
    let rec aux l1 = function 
        | []		-> l1
        |	e::l	-> aux (e::l1) l
    in
    aux [] l

let lowercase_char = function
    | 'A' -> 'a'
    | 'B' -> 'b'
    | 'C' -> 'c'
    | 'D' -> 'd' 
    | 'E' -> 'e'
    | 'F' -> 'f'
    | 'G' -> 'g'
    | 'H' -> 'h'
    | 'I' -> 'i'
    | 'J' -> 'j'
    | 'K' -> 'k'
    | 'L' -> 'l'
    | 'M' -> 'm'
    | 'N' -> 'n'
    | 'O' -> 'o'
    | 'P' -> 'p'
    | 'Q' -> 'q'
    | 'R' -> 'r'
    | 'S' -> 's'
    | 'T' -> 't'
    | 'U' -> 'u'
    | 'V' -> 'v'
    | 'W' -> 'w'
    | 'X' -> 'x'
    | 'Y' -> 'y'
    | 'Z' -> 'z'
    |  l  ->  l
let lowercase s =
    for i = 0 to String.length s - 1 do
        s.[i] <- lowercase_char s.[i]
    done;
    s

let cut_text txt =
    let l = ref []
    and s = ref 0
    and w = ref 0
    and blank = function
        | ' ' | '	' | '\n' | '\r'  | ':' | '?' | '.' | '[' | ']'
        | '{' | '}' | '"' | '!' | '1' | '2' | '3' | '4' | '5' | '6'
        | '7' | '8' | '9' | '0' | '(' | ')' | ',' | ';'	-> true
        | _												-> false
    in

    while !s + !w < String.length txt do	
        if (blank txt.[!s + !w]) then
            begin
                if (!w > 0) then
                    l := lowercase (String.sub txt !s !w)::!l;
                    s := !s + !w + 1;
                    w := 0;
            end
        else
            w := !w + 1;
    done;

    if (!w > 0) then
        reverse ((String.sub txt !s !w)::!l)
    else
        reverse !l

let detect_language l =
    let rec load_dicos = function
        | [] -> []
        | e::l -> (ref 0, get_dictionnary e)::load_dicos l
    and check_word w = function
        | [] -> ()
        | (e,d)::l ->
                if (exists d w) then
                    e := !e + 1;
                check_word w l
    in

    let dicos = load_dicos Lang.urls_dicos in
    
    let rec check_n_times n l = match (n,l) with
    | (n,_) when n <= 0	-> ()
    | (_,[]) -> ()
    | (n,e::l) ->
            check_word e dicos;
            check_n_times (n-1) l

    and max = function
        | [] -> raise (Failure "At least one language plz.")
        | (e,d)::l -> max2 !e d l

    and max2 e d = function
        | [] -> d
        | (e2,d)::l when !e2 > e ->
                max2 !e2 d l
        | _::l ->
                max2 e d l
    in
    check_n_times Lang.numb_words_language_identification l;
    max dicos

let rec limit_list l n = match l with 
    | [] -> []
    | _ when n <= 0	-> []
    | e::l -> e::(limit_list l (n-1))

let sort_errors origin l =
    let rec aux = List.sort (function (i,w) -> function (j,x) -> i - j)
    and transform = function
        | []    -> []
        | w::l  -> (Levenshtein.levenshtein origin w, w)::(transform l)
    and untransform = function
        | []        -> []
        | (n,w)::l  -> w::(untransform l)
    in
    untransform (aux (transform l))


let rec send_errors dico phone = function
    | [] -> []
    | s::l when	(exists dico s) -> send_errors dico phone l
    | s::l ->
            (s, limit_list (sort_errors s
            (find_all phone (Soundex.soundex s dico.lang)))
            Lang.numb_words_corrections)::(send_errors dico phone l)

