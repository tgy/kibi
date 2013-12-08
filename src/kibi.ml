let clean_status url msg =
	let oo = open_out url in
	if msg != "" then
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
	clean_status "Program started";
	let img = new OcsfmlGraphics.image(`File !input_file) in
	update_status "Preprocessing";
	let (binarized,preproc,rotated) = Lenna.preprocess img in 
	update_status_time ();
	update_status "Segmentation";
	update_status_time ();
	update_status "Identification (ANN)";
	update_status_time ();
	update_status "Spell Checking";
	update_status_time ();
	update_status "...End of Process";
	update_status_total_time ();
end

let _ = main ()
