let delete_space str = 
    let rec invalid_char = function
        | ' ' | '_' | '\''
        | ',' | '-' | '"'  -> true
        | _                -> false
    and numb_space n = function
        | i when i >=  String.length str  -> n
        | i when (invalid_char str.[i])   -> numb_space (n+1) (i+1)
        | i                               -> numb_space n (i+1)
    and rewrite newstr pos = function
        | i when i >= String.length str   -> newstr
        | i when (invalid_char str.[i])   -> rewrite newstr pos (i+1)
        | i                               -> newstr.[pos] <- str.[i];
        rewrite newstr (pos+1) (i+1)
    in 
    rewrite (String.create (String.length str - numb_space 0 0)) 0 0

let char_value_en = function
    | 'B' | 'F' | 'P' | 'V' -> '1'
    | 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' -> '2'
    | 'D' | 'T' -> '3'
    | 'L' -> '4'
    | 'M' | 'N' -> '5'
    | _ -> '6'
let char_value_fr = function
    | 'B' | 'P' -> '1'
    | 'C' | 'K' | 'Q' -> '2'
    | 'D' | 'T' -> '3'
    | 'L' -> '4'
    | 'M' | 'N' -> '5'
    | 'R' -> '6'
    | 'G' | 'J' -> '7'
    | 'X' | 'Z' | 'S' -> '8'
    | _  -> '9'

let correct_string s =
    let replace_chars c oldchar =
        if (oldchar <>  '\195') then
            c
        else
            match c with
            | '\160' | '\161' | '\162' | '\163' | '\164' | '\165' | '\166' ->
                    'a'
            | '\167' -> 'c'
            | '\168' | '\169' | '\170' | '\171'	-> 'e'
            | '\172' | '\173' | '\174' | '\175' -> 'i'
            | '\177' -> 'n'
            | '\176' | '\178' | '\179' | '\180' | '\181' | '\182' | '\184' ->
                    'o'
            | '\185' | '\186' | '\187' | '\188'	-> 'u'
            | '\189' | '\190' | '\191' -> 'y'
            | '\157' -> 'Y'
            | '\153' | '\154' | '\155' | '\156'	-> 'U'
            | '\146' | '\147' | '\148' | '\149' | '\150' | '\152' -> 'O'
            | '\145' -> 'N'
            | '\143' | '\142' | '\141' | '\140' -> 'I'
            | '\136' | '\137' | '\138' | '\139'	-> 'E'
            | '\135' -> 'C'
            | '\128' | '\129' | '\130' | '\131' | '\132' | '\133' | '\134' ->
                    'A'
            | l -> c
    and nb_char c = 
        let n = ref 0 in
            for i = 0 to String.length s - 1 do
                if (s.[i] = c) then
                    n := !n + 1
            done;
            !n
    in
    
    let newstr = String.make (String.length s - (nb_char '\195')) '~' 
    and j = ref 0 
    in
    
    if (String.length s > 0) then	
        for i = 0 to String.length s - 1 do
            if (s.[i] <> '\195') then
            (
                if (i > 0) then
                    newstr.[!j] <- replace_chars s.[i] s.[i-1]
                else
                    newstr.[!j] <- replace_chars s.[i] ' ';

                j := !j + 1
            )
        done;
        newstr	

let soundex str (lang:Lang.language)=
    let newstr = String.make 4 '0' in
    if (String.length str == 0) then
        newstr
    else
        let invalid_char = function
            | 'A' | 'E' | 'H' | 'I' | 'O' | 'U' | 'W' | 'Y' -> true
            | _                                             -> false
        and pos = ref 1
        and i = ref 1
        and c = ref '0'
        and str2 = String.uppercase (correct_string (delete_space str))
        in
        if (String.length str2 > 0) then
            newstr.[0] <- str2.[0];
        while (!pos < String.length newstr && !i < String.length str2) do
            if (not(invalid_char str2.[!i])) then
                begin
                    if (lang = Lang.Fr) then
                        c := char_value_fr str2.[!i]	
                    else
                        c := char_value_en str2.[!i];
                    if (!c <> newstr.[!pos - 1]) then
                        begin
                            newstr.[!pos] <- !c;
                            pos := !pos + 1
                        end;
                end;
        i := !i + 1;
        done;
        newstr
