let main () =
  begin
    let img = new OcsfmlGraphics.image (`File "../test/in/1.jpg") in
    let w,h = img#get_size in
    print_endline (Printf.sprintf "Original: %dx%d" w h);
    let angle = 10. *. 3.14159265 /. 180. in
    let new_img = Rotation.rotate img angle in
    let w,h = new_img#get_size in
    print_endline (Printf.sprintf "New: %dx%d" w h);
    new_img#save_to_file "../test/out/rotated-test.png"
  end

let _ = main ()
