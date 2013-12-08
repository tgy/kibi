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
	let rec loopparags = function
		| [] -> []
		| paragraph :: paragraphs ->
		let rec looplines = function
			| [] -> []
			| line :: lines ->
			let rec loopwords = function
				| [] -> []
				| word :: words ->
				let rec loopchars = function 
					| [] -> []
					| (x0,xmax,y0,ymax) :: chars ->
					try
						let input = Resizer.get_pixvector img (x0,y0) (xmax, ymax) 32 in
						let s = Anna.identify_char "anna/weights/weights0.txt" input in
						print_string s;
						s::loopchars chars
					with _ -> loopchars chars
				in loopchars word::loopwords words
			in loopwords line::looplines lines
		in looplines paragraph::loopparags paragraphs
	in loopparags l

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
	let boxes = Freddy.getlists img in
	update_status_time ();
	update_status "Identification (ANN)";
	Printf.printf "%d\n%!" (List.length boxes);
	let resultAnna = loopanna img boxes in
	Printf.printf "finished%!";
	update_status_time ();
	update_status "Spell Checking";
	update_status_time ();
	update_status "...End of Process";
	update_status_total_time ();
	update_status "END";
end

let _ = main ()
