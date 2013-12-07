let updatestatus url msg =
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%s\n" msg;
	close_out oo


let main () =
begin
	let input_file = ref "input/default.png"
	and output_dir = ref "output/"
	in
	Arg.parse [
		("-i", Arg.String(fun s -> input_file := s), "input image");
		("-o", Arg.String(fun s -> output_dir := s), "output dir")
	] (fun _ -> ()) "kibi.native [-i input_file] [-o output_dir]";
	
end

let _ = main ()
