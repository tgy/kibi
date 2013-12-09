let file = ref ""

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
    | n when n >= 0 && n < 91 -> String.make 1 (char_of_int (n + 48))
    | n -> failwith ("unknown char: " ^ string_of_int n)

let array_to_char a =
    let maxi = ref 0 in
    for i = 1 to Array.length a - 1 do
        if a.(i) > a.(!maxi) then
            maxi := i;
    done; print_int !maxi; num_to_char !maxi

let main () =
begin
    let charcodelist =
      let rec aux = function
        | 47 -> []
        | n -> n :: aux (n - 1) in
      aux 122 in
    let nb_chars = List.length charcodelist in
    let net = new Network.network (150., 1., 100., 0.) (32 * 32, nb_chars, nb_chars) in
    if !file <> "" then
      begin
        net#load_weights "weights/weights1024x90x90.txt";
        Printf.printf "reading file %S\n" !file;
        let a = net#propagate (image_to_input !file) in
        print_array a; print_newline ();
        Printf.printf "char: %S\n" (array_to_char a)
      end
    else
      begin
        CharIdentification.train_network
          net
          "../setgen/out"
          40000
          40000
          (*~weights:"weights/weights0.txt"*)
          charcodelist
          1422;
      end
end;;

Arg.parse [] (fun str -> file := str) ""

let _ = main ()
