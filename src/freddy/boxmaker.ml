(*Is the box a = (axl,axh,ayl,ayh) included in b = (bxl,bxh,byl,byh)?*)
let isainb a b =
  let (axl,axh,ayl,ayh) = a and
  (bxl,bxh,byl,byh) = b in
  axl >= bxl && axh <= bxh && ayl >= byl && ayh <= byh

let formatit charlst wordlst linelist paraglist =
	let rec getinternalboxes b = function
		|	[] -> []
		| c::l when isainb c b -> c::(getinternalboxes b l)
		| _::l -> getinternalboxes b l
	and getnew_words = function
		| [] -> []
		| e::l -> (getinternalboxes e charlst)::(getnew_words l)
	and getnew_lines = function
		| [] -> []
		| e::l -> (getnew_words (getinternalboxes e wordlst))::(getnew_lines l)
	and getnew_paras = function
		| [] -> []
		| e::l -> (getnew_lines (getinternalboxes e linelist))::(getnew_paras l)
	in
	getnew_paras paraglist

let main () =
  let a = (0,10,0,10) and b = (0,10,0,10) in
  Printf.printf "%B\n" (isainb a b);
  flush stdout

let _ = main ()
