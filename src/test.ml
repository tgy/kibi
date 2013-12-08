let main () =
  let img = new OcsfmlGraphics.image (`File "lenna/test/out/4rotated.png") in
  let (boxes, _, _, _) = Freddy.getlists img in
  let rec aux n = function
    | [] -> []
    | (minx, maxx, miny, maxy) :: l ->
        try
          let input = Resizer.get_pixvector img (minx, miny) (maxx, maxy) 32 in
          (Resizer.pixvector_to_img input 32)#save_to_file ("bla" ^ string_of_int n ^ ".png");
          print_string (Anna.identify_char "anna/weights/weights1024x90x90.txt" input);
          flush stdout;
          aux (n + 1) l
        with
          Division_by_zero -> aux (n + 1) l
  in aux 0 boxes
let _ = main ()
