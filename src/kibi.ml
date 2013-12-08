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
  let net0 = new Network.network (0., 0., 0., 0.) (i, h, o)
  and net1 = new Network.network (0., 0., 0., 0.) (i, h, o)
  and net2 = new Network.network (0., 0., 0., 0.) (i, h, o)
  and net3 = new Network.network (0., 0., 0., 0.) (i, h, o) in
  net0#load_weights "anna/weights/weights0.txt";
  net1#load_weights "anna/weights/weights1.txt";
  net2#load_weights "anna/weights/weights2.txt";
  net3#load_weights "anna/weights/weights3.txt";
  let nets = [net0; net1; net2; net3] in
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
            let input = Resizer.get_pixvector img (x0,y0) (xmax, ymax) 32 in
            let s = Anna.identify_char nets input in	
            s::loopchars chars
        in loopchars word::loopwords words
			in loopwords line::looplines lines
		in looplines paragraph::loopparags paragraphs
	in loopparags l

let printlang () = match Robert.currentlang() with
	| Lang.Fr -> Printf.printf "[Fr]\n%!"
	| _       -> Printf.printf "[Eng]\n%!"

let printtext parags =
	let rec printl = function
		| [] -> ()
		| w::l -> Printf.printf "%s%! " w; printl l
	in let rec printp = function
		| [] -> ()
		| l::p -> printl l; Printf.printf "\n%!"; printp p
	in let rec printt = function
		| [] -> ()
		| p::t -> printp p; Printf.printf "\n%!"; printt t
	in printt parags

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
	let resultAnna = loopanna rotated boxes in
	update_status_time ();
	update_status "Spell Checking";
	let convertRobert = Robert.transformAnnaOutput resultAnna in
	Robert.detect convertRobert;
	let correctRobert = Robert.correct convertRobert in
	update_status_time ();
	(*printtext convertRobert;*)
	(*printlang ();*)
	Robert.savejson ".result.json" correctRobert;
	update_status "...End of Process";
	update_status_total_time ();
	update_status "END";
end

let _ = main ()
