(* Set generator for Anna training *)

module O = OcsfmlGraphics

exception Not_Found_Output_Dir
exception Mkdir_Error

let dir_exists path =
  try Sys.file_exists path
  with _ -> false

let mkdir path =
  if not (Sys.command ("mkdir -p " ^ path) = 0) then
    print_endline (Printf.sprintf "Error in mkdir of " ^ path)

let img_size = 32
let char_size = 26
let bgcolor = O.Color.rgb 0 0 0
let intervals = [| (33, 122) |]

let interval_max =
  let max = ref max_int in
  for i = 0 to Array.length intervals - 1 do
    let _, hb = intervals.(i) in
    if hb > !max || !max = max_int then max := hb
  done; !max

let generate indir outdir =
  let files = Sys.readdir indir in
  let font_count = ref 0
  and char_count = ref 0 in
  for i = 0 to Array.length files - 1 do
    try (
      let font = new O.font (`File (indir ^ "/" ^ files.(i))) in
      let texture = font#get_texture char_size in
      let rects = Array.make (interval_max + 1) (O.IntRect.create ()) in
      for j = 0 to Array.length intervals - 1 do
        let lb, hb = intervals.(j) in
        for char_code = lb to hb do
          char_count := !char_count + 1;
          let glyph = font#get_glyph char_code char_size true in
          let rect = glyph.O.Glyph.texture_rect in
          rects.(char_code) <- rect
        done
      done;
      let tex_img = texture#copy_to_image in
      for j = 0 to Array.length intervals - 1 do
        let lb, hb = intervals.(j) in
        for char_code = lb to hb do
          let img = new O.image (`Color (bgcolor, img_size, img_size)) in
          let destX, destY =
            ((img_size - rects.(char_code).O.IntRect.width) / 2),
            ((img_size - rects.(char_code).O.IntRect.height) / 2)
          in
          img#copy tex_img destX destY
            ~srcRect: rects.(char_code)
            ~alpha: true;
          let dir_path = outdir ^ "/" ^ string_of_int char_code in
          if not (dir_exists dir_path) then
            mkdir dir_path;
          let binarized = Lenna.binarize img in
          let inverted = Lenna.invert binarized in
          let final = Resizer.pixvector_to_img (
            Resizer.get_pixvector inverted (0, 0) (img_size - 1, img_size - 1)
              img_size
          ) img_size in
          ignore (
            final#save_to_file
            (dir_path ^ "/" ^ string_of_int i ^ ".png")
          )
        done
      done;
      font_count := !font_count + 1
    )
    with
    | OcsfmlSystem.LoadFailure -> ()
  done;
  print_endline (Printf.sprintf "%d fonts processed." !font_count);
  print_endline (Printf.sprintf "%d images generated." !char_count);
