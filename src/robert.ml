(*
	string list list list 									as text
	(string * string list) list list list		as correction
  string as url
	
	detect: text -> unit
  correct: text -> correction 
	currentlang: unit -> Lang.language
  tojson: correction -> string
  savejson:  url -> correction -> unit
*)
let transformAnnaOutput char_list_list_list_list =
  let rec iterchars = function
    | [] -> ""
    | e::l -> e ^ (iterchars l) in
  let rec iterwords = function
    | [] -> []
    | e::l ->
      (iterchars e)::iterwords l in
  let rec iterlines = function
    | [] -> []
    | e::l ->
      (iterwords e)::(iterlines l) in
  let rec iterparags = function
    | [] -> []
    | e::l -> (iterlines e)::(iterparags l) in
  iterparags char_list_list_list_list


let (detect, correct, currentlang, tojson) = 
	let dico = ref None and
			dicophon = ref None in
	let rec get = function
		| None -> raise (Failure "Blabla")
		| Some(e) -> e
	and findphon = function
		| (_,[]) | ([],_) -> raise (Failure "Error in module lang.ml")
		| (e::_, f::_) when (e = (get !dico).Wordchecker.lang) ->
		Wordchecker.get_dictionnary f
		| (_::l, _::m) -> findphon (l,m)
	in
	(
		function l ->
			dico := Some (Wordchecker.detect_language l);
			dicophon := Some (findphon (Lang.languages,Lang.urls_phone));
	),(
		function l -> 
		match !dico with
			| None 		-> raise (Failure "You must first check the language")
			| Some(d)	-> Wordchecker.send_errors (get !dico) (get !dicophon) l 
	),(
		function ()	-> (get !dico).Wordchecker.lang
	),(
		let lang2str () = match (get !dico).Wordchecker.lang with
			| Lang.En	-> "en"
			| _				-> "fr"
		in
		let fusion_word (w,c) =
			let s = String.concat "," (List.map (fun s -> Printf.sprintf "%S" s) c) in
			if s = "" then
				Printf.sprintf "{%S:[]}" w
			else
				Printf.sprintf "{%S:[%s]}" w s	
		in let fusion_line l =
			Printf.sprintf "[%s]" (String.concat "," (List.map fusion_word l))
	  in let fusion_paragraphes l	=
			Printf.sprintf "[%s]" (String.concat "," (List.map fusion_line l)) 	
		in let fusion_whole l =
			Printf.sprintf "[%s]" (String.concat "," (List.map fusion_paragraphes l))
		in function l -> 
			Printf.sprintf "{\"lang\":%S,\"content\":%s}" (lang2str()) (fusion_whole l)
	)

let savejson url l =
	let s = tojson l in
	let oo = open_out url in
	output_string oo s
	
