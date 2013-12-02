let print_array a =
    for i = 0 to Array.length a - 1 do
        Printf.printf "%f\n" a.(i)
    done;
    print_newline ()

let image_to_input file =
    let img = new OcsfmlGraphics.image(`File file) in
    let w, h = img#get_size in
    let a = Array.make (w * h) 0. in
    for j = 0 to h - 1 do
        for i = 0 to w - 1 do
            a.(j * w + i) <- float (abs (1 - (img#get_pixel i j).OcsfmlGraphics.Color.r / 255))
        done
    done; a

let num_to_char = function
    | n when n >= 0 && n < 123 -> String.make 1 (char_of_int (n + 33))
    | _ -> failwith "unknown char"

let array_to_char a =
    let maxi = ref 0 in
    for i = 1 to Array.length a - 1 do
        if a.(i) > a.(!maxi) then
            maxi := i;
    done; num_to_char !maxi

let char_to_file c = "../setgen/out/" ^ string_of_int (int_of_char c) ^ "/8.png"

let main () =
begin
    let (firstc, lastc) = (33, 122) and nb_fonts = 24 in
    let nb_chars = lastc - firstc + 1 in
    let net = new Network.network (1., 5., 100., 0.) (32 * 32, nb_chars, nb_chars) in
    net#load_weights "weights1024x90x90.txt";
    let t = Array.make_matrix nb_chars nb_chars 0. in
    for i = 0 to nb_chars - 1 do
        t.(i).(i) <- 1.;
    done;
    let examples = ref [] in
    for i = 0 to nb_chars - 1 do
        for j = 0 to nb_fonts - 1 do
            let si = string_of_int (i + firstc) and sj = string_of_int j in
            let path = ("../setgen/out/" ^ si ^ "/" ^ sj ^ ".png") in
            try
                examples := (image_to_input path, t.(i)) :: !examples
            with
                _ -> ()
        done
    done;
    let examples = Array.of_list !examples in
    for i = 0 to 9 do
        net#learn examples 1000 true;
        net#save_weights "weights.txt";
        Printf.printf "weights saved in weights.txt\n"
    done;
    while true do
        print_string "character: ";
        let c = int_of_char (read_line ()).[0] in
        print_string "font number: ";
        let str = read_line () in
        let num = String.sub str 0 (if String.length str < 2 then String.length str else 2) in
        let file = "../setgen/out/" ^ string_of_int c ^ "/" ^ num ^ ".png" in
        Printf.printf "reading file %S\n" file;
        let a = net#propagate (image_to_input file) in
        print_array a; print_newline ();
        Printf.printf "char: %S\n" (array_to_char a)
    done
end

let _ = main ()
