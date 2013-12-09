let main () =
  let img = new OcsfmlGraphics.image (`File "../../test/UneColMultiFont_300/5rot.png") in
  let boxes = Freddy.getlists img in
	let rec displays_boxes = function
		| [] -> img#save_to_file "boxes.png"
		| par::pars ->
		let rec aux = function
			| [] -> ()
			| line::lines ->
			let rec aux2 = function
				| [] -> ()
				| word::words ->
				let rec aux3 = function
					| [] -> ()
					| (xm,xM,ym,yM)::chars ->
					for x = xm to xM do
						img#set_pixel x ym OcsfmlGraphics.Color.red;
						img#set_pixel x yM OcsfmlGraphics.Color.red;
					done;
					for y = ym to yM do
						img#set_pixel xm y OcsfmlGraphics.Color.red;
						img#set_pixel xM y OcsfmlGraphics.Color.red;
					done;
					aux3 chars
				in aux3 word; aux2 words
			in aux2 line; aux lines
		in aux par; displays_boxes pars
	in displays_boxes boxes;
  let (i, h, o) = Network.read_size "assets/weights/weights0.txt" in
  let net_nbr = 20 in
  let nets =
    let rec aux = function
      | 0 -> []
      | n -> new Network.network (0., 0., 0., 0.) (i, h, o) :: aux (n - 1)
    in aux net_nbr in
  let rec load n = function
    | [] -> ()
    | net :: l ->
      net#load_weights ("assets/weights/weights" ^ string_of_int n ^ ".txt"); load (n + 1) l
  in load 0 nets;
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
                | (x0, xmax, y0, ymax) :: chars ->
                try
                    let input = Resizer.get_pixvector img (x0, y0) (xmax, ymax) 32 in
                    let id = string_of_int n0 ^ "-" ^ 
                      string_of_int n1 ^ "-" ^
                      string_of_int n2 ^ "-" ^
                      string_of_int n3 in
                    (Resizer.pixvector_to_img input 32)#save_to_file ("bla" ^ id ^ ".png");
                    print_string (Anna.identify_char nets input);
                    flush stdout;
                    aux3 (n3 + 1) chars
                  with
                    _ -> ()
              in print_string " "; aux3 0 word; aux2 (n2 + 1) words
          in print_newline (); aux2 0 line; aux1 (n1 + 1) lines
      in print_newline (); aux1 0 paragraph; aux0 (n0 + 1) paragraphs
  in aux0 0 boxes
let _ = main ()
