module O = OcsfmlGraphics

let print_array a =
  for i = 0 to Array.length a - 1 do
    Printf.printf "%f\n" a.(i)
  done;
  print_newline ()

let main () =
  let img = new O.image (`File "../../setgen/out/97/14.png") in
  img#save_to_file "out/test_before.png";
  let pixvector = Resizer.get_pixvector img (0, 0) (32, 32) 42 in
  let nimg = Resizer.pixvector_to_img pixvector 42 in
  nimg#save_to_file "out/test.png"
  

  (*let (b1x, b1y), (b2x, b2y) = Resizer.crop_bounds img (0, 0) (31, 31) in
  print_endline (Printf.sprintf "b1x %d ; b1y %d ; b2x %d ; b2y %d" b1x b1y b2x
  b2y)*)

let _ = main ()
