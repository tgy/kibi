let clean_status url msg =
	let oo = open_out url in
	if msg != "" then
		Printf.fprintf oo "%s\n%f\n" msg (Sys.time());
	close_out oo

let update_status url msg =
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%s\n%f\n" msg (Sys.time());
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
	clean_status !statusfile "KibiOCR: Program Started";
	let img = new OcsfmlGraphics.image(`File !input_file) in
	update_status !statusfile "KibiOCR: Image Loaded";
	let (binarized,preproc,rotated) = Lenna.preprocess img in 
	update_status !statusfile "Lenna  : preprocess finished" 	
end

let _ = main ()
