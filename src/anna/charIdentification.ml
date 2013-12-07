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
            a.(j * w + i) <- float (abs
                (1 - (img#get_pixel i j).OcsfmlGraphics.Color.r / 255))
        done
    done; a

let num_to_char = function
    | n when n >= 0 && n < 123 -> String.make 1 (char_of_int (n + 33))
    | n -> failwith ("unknown char: " ^ string_of_int n)

let interpret_network_output a =
    let maxi = ref 0 in
    for i = 1 to Array.length a - 1 do
        if a.(i) > a.(!maxi) then
            maxi := i;
    done; num_to_char !maxi

let get_examples directory_path charcodelist fonts_nb =
    let examples = ref []
    and char_nb = List.length charcodelist in
    let t = Array.make_matrix char_nb char_nb  0. in
    for i = 0 to char_nb - 1 do
        t.(i).(i) <- 1.
    done;
    let rec aux n = function
        | [] -> ()
        | code :: l ->
            for j = 0 to fonts_nb - 1 do
                let sc = string_of_int code and sj = string_of_int j in
                let path = (directory_path ^ "/" ^ sc ^ "/" ^ sj ^ ".png") in
                try
                    examples := (image_to_input path, t.(n)) :: !examples
                with
                    _ -> ()
            done; aux (n - 1) l
    in aux (char_nb - 1) charcodelist; Array.of_list !examples

let train_network
    net
    directory_path (* directory's content: <ascii-code>/<font>.png *)
    iteration_nb
    error_print_period
    ?(weights = "")
    charcodelist
    font_nb = 
        let examples = get_examples directory_path charcodelist font_nb in
        if weights <> "" then
            net#load_weights weights
        else
            net#randomize_weights;
        net#learn examples iteration_nb true error_print_period;
        net#save_weights "weights.txt";
        Printf.printf "weights saved in weights.txt\n"

let identify weights pixvector =
    let incount, hidcount, outcount = Network.read_size weights in
    let net = new Network.network
        (0., 0., 0., 0.)
        (incount, hidcount, outcount) in
    net#load_weights weights;
    let a = net#propagate pixvector in
    interpret_network_output a
