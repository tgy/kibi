let clean_status url msg =
	let oo = open_out url in
	if String.length msg > 0 then
		Printf.fprintf oo "%s\n" msg;
	close_out oo

let update_status url msg =
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%s\n" msg;
	close_out oo

let update_status_time =
	let f = ref 0. in function url ->
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%f\n" (Sys.time() -. !f);
	f := Sys.time();
	close_out oo

let update_status_total_time url =
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%f\n" (Sys.time());
	close_out oo

let loopanna img l =
  let (i, h, o) = Network.read_size "anna/weights/weights0.txt" in
  let net_nbr = 20 in
  let nets =
    let rec aux = function
      | 0 -> []
      | n -> new Network.network (0., 0., 0., 0.) (i, h, o) :: aux (n - 1)
    in aux net_nbr in
  let rec load n = function
        | [] -> ()
        | net :: l ->
          net#load_weights ("anna/weights/weights" ^ string_of_int n ^ ".txt"); load (n + 1) l
  in load 0 nets;
	let rec loopparags = function
		| [] -> []
		| paragraph :: paragraphs ->
		let rec looplines = function
      | [] -> print_newline (); []
			| line :: lines ->
			let rec loopwords = function
				| [] -> print_newline (); []
				| word :: words ->
				let rec loopchars = function 
          | [] -> print_string " "; []
					| (x0,xmax,y0,ymax) :: chars -> 
              try
                let input = Resizer.get_pixvector img (x0,y0) (xmax, ymax) 32 in
                let s = Anna.identify_char nets input in	
                print_string s;
                s::loopchars chars
              with
                Division_by_zero -> loopchars chars
        in loopchars word::loopwords words
			in loopwords (List.rev line)::looplines lines
		in looplines (List.rev paragraph)::loopparags paragraphs
	in loopparags (List.rev l)

let main () =
begin
	let input_file = ref "images/default.jpg"
	and output_dir = ref "out/"
	and statusfile = ref ".status"
	in
	Arg.parse [
		("-i", Arg.String(fun s -> input_file := s), "input image");
		("-o", Arg.String(fun s -> output_dir := s), "output dir");
		("-c", Arg.String(fun s -> statusfile := s), "status file")
	] (fun _ -> ()) "kibi.native [-i input_file] [-o output_dir] [-c status file]";
	let clean_status = clean_status !statusfile
	and update_status = update_status !statusfile
	and update_status_time () = update_status_time !statusfile
	and update_status_total_time () = update_status_total_time !statusfile
	in
	clean_status "";
	let img = new OcsfmlGraphics.image(`File !input_file) in
	update_status "Preprocessing";
	let (binarized,preproc,rotated) = Lenna.preprocess img in 
	update_status_time ();
	update_status "Segmentation";
	let boxes = Freddy.getlists rotated in
	update_status_time ();
	update_status "Identification (ANN)";
	Printf.printf "%d\n%!" (List.length boxes);
	let resultAnna = loopanna rotated boxes in
	Printf.printf "finished%!";
	update_status_time ();
	update_status "Spell Checking";
	update_status_time ();
	update_status "...End of Process";
	update_status_total_time ();
	update_status "END";
end

let _ = main ()
