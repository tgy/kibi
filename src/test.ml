let main () =
  let img = new OcsfmlGraphics.image (`File "lenna/test/out/4rotated.png") in
  let boxes = Freddy.getlists img in
  let rec aux0 n0 = function
    | [] -> ()
    | paragraph :: paragraphs ->
      let rec aux1 n1 = function
        | [] -> ()
        | line :: lines ->
          let rec aux2 n2 = function
            | [] -> ()
            | word :: words ->
              let rec aux3 n3 = function
                | [] -> ()
                | (x0, xmax, y0, ymax) :: chars -> aux3 (n3 + 1) chars;
                    let input = Resizer.get_pixvector img (x0, y0) (xmax, ymax) 32 in
                    let id = string_of_int n0 ^ "-" ^ 
                      string_of_int n1 ^ "-" ^
                      string_of_int n2 ^ "-" ^
                      string_of_int n3 in
                    (Resizer.pixvector_to_img input 32)#save_to_file ("bla" ^ id ^ ".png");
                    print_string (Anna.identify_char "anna/weights/weights1024x90x90.txt" input);
                    flush stdout;
              in print_string " "; aux2 (n2 + 1) words; aux3 0 word
          in print_newline (); aux1 (n1 + 1) lines; aux2 0 line 
      in print_newline (); aux0 (n0 + 1) paragraphs; aux1 0 paragraph
  in aux0 0 boxes
let _ = main ()
