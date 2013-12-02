module O = OcsfmlGraphics

let main () =
  let img = new O.image (`File "a.png") in
  let w, h = img#get_size in
  let new_img = Resizer.resize img (w, h) (400, 400) 42 in
  (new_img#save_to_file "test.png")

let _ = main ()
