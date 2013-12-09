let clean_status url s msg =
	let oo = open_out url in
	if String.length msg > 0 then
		Printf.fprintf oo "%s%s" s msg;
	close_out oo

let update_status url s msg =
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%s%s" s msg;
	close_out oo

let update_status_time =
	let f = ref 0. in function url -> function s -> 
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%s%f" s (Sys.time() -. !f);
	f := Sys.time();
	close_out oo

let update_status_total_time s url =
	let oo = open_out_gen [Open_creat;Open_append] 0o777 url in
	Printf.fprintf oo "%s%f" s (Sys.time());
	close_out oo

let loopanna img l =
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
                let s = Anna.identify_char nets input in	
                print_string s;
                s::loopchars chars
              with
                Division_by_zero -> loopchars chars
        in loopchars word::loopwords words
			in loopwords (List.rev line)::looplines lines
		in looplines (List.rev paragraph)::loopparags paragraphs
	in loopparags (List.rev l)

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
	and output_dir = ref ""
	and statusfile = ref "processing.tmp"
	and jsonfile = ref "robert.json"
	in
	Arg.parse [
		("-i", Arg.String(fun s -> input_file := s), "input image");
		("-o", Arg.String(fun s -> output_dir := s), "output dir");
	] (fun _ -> ()) "kibi.native [-i input_file] [-o output_dir] [-c status file] [-j json output]";
  statusfile := !output_dir ^ !statusfile;
  jsonfile := !output_dir ^ !jsonfile;
	let clean_status = clean_status !statusfile ""
	and update_status = update_status !statusfile 
	and update_status_time s () = update_status_time !statusfile s
	and update_status_total_time s () = update_status_total_time !statusfile s
	in
	clean_status "";
	let img = new OcsfmlGraphics.image(`File !input_file) in
	update_status "" "Preprocessing";
	let (binarized,preproc,rotated) = Lenna.preprocess img in 
	update_status_time "\n" ();
	update_status "\n" "Segmentation";
	let boxes = Freddy.getlists rotated in
	update_status_time "\n" ();
	update_status "\n" "Identification (ANN)";
	let resultAnna = loopanna rotated boxes in
	update_status_time "\n" ();
	update_status "\n" "Spell Checking";
	let convertRobert = Robert.transformAnnaOutput resultAnna in
	Robert.detect convertRobert;
	let correctRobert = Robert.correct convertRobert in
	update_status_time "\n" ();
	(*printtext convertRobert;*)
	(*printlang ();*)
	(*Robert.savejson !jsonfile correctRobert;*)
  Robert.savehtml !jsonfile correctRobert;
	update_status "\n" "...End of Process";
	update_status_total_time "\n" ();
	update_status "\n" "END";
end

let _ = main ()
