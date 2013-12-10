let getlists preprocessedimg =
  let boxes = BoundingBoxes.get_boxes preprocessedimg in
  let (avgw,avgh) = BoundingBoxes.average_box boxes in
  let chars = Segmentation.startRLSA preprocessedimg (&&) avgw avgh in
  let words = Segmentation.startRLSA preprocessedimg (||) (avgw) (avgh / 2) in
  let lines = Segmentation.startRLSA preprocessedimg (||) (2 * avgw) (avgh / 2) in
  let parags = Segmentation.startRLSA preprocessedimg (||) (5 * avgw) (5 * avgh) in
  let charboxes = BoundingBoxes.get_boxes chars in
  let charimg = BoundingBoxes.display_boxes preprocessedimg charboxes in
  let wordboxes = BoundingBoxes.get_boxes words in
  let wordimg = BoundingBoxes.display_boxes preprocessedimg wordboxes in
  let lineboxes = BoundingBoxes.get_boxes lines in
  let lineimg = BoundingBoxes.display_boxes preprocessedimg lineboxes in
  let paragboxes = BoundingBoxes.get_boxes parags in
  let paragimg = BoundingBoxes.display_boxes preprocessedimg paragboxes in
  ignore (charimg#save_to_file ("chars.bmp"));
  ignore (wordimg#save_to_file ("words.bmp"));
  ignore (lineimg#save_to_file ("lines.bmp"));
  ignore (paragimg#save_to_file ("parags.bmp"))
  (*(charboxes, wordboxes, lineboxes, paragboxes)*)


let fusionchars l =
	let disty (_,_,ym,yM) (_,_,ym1,yM1) = 
		if (ym > yM1) then  ym - yM1
		else if (ym1 > yM) then ym1 - yM
		else 0
	in
	let inside (xm,xM,ym,yM) (xm1,xM1,ym1,yM1) =
		(xm <= xm1 && xM1 <= xM) || (xm1 <= xm && xM <= xM1) &&
		(disty (xm,xM,ym,yM) (xm1,xM1,ym1,yM1)) < 2 * (max (yM - ym) (yM1 - ym1))
	and fusion (xm,xM,ym,yM) (xm1,xM1,ym1,yM1) = (min xm xm1, max xM xM1,
																								min ym ym1, max yM yM1)
	in
	let aux b l = 
		let b = ref b in
		let rec aux2 = function
			| [] ->	[]
			| e::l when inside e !b ->
				b  := fusion !b e;
				aux2 l
			| e::l -> e::(aux2 l)
		in !b::(aux2 l)
	in let rec aux2 = function
			| [] -> []
			| e::l -> match aux e l with
				| [] -> []
				| e::l -> e::(aux2 l)
	in aux2 l


let sort l =
	let sortchars l = 
		let nl = List.sort (fun (xm,_,_,_) (xm1,_,_,_) -> compare xm xm1) (fusionchars l) in
		match nl with
			| [] ->[]
			| (x,_,_,_)::l -> nl
	in
	let sortwords line =
		let rec aux = function
			| [] -> []
			| e::l -> sortchars e::aux l
		in 
		List.sort 
			(fun l l2 -> match (l,l2) with
				| (x,_,_,_)::_,(x2,_,_,_)::__ -> compare x x2
				| _ -> 1
			) (aux line) 
	in
	let sortlines parag =
		let rec aux = function
			| [] -> []
			| e::l -> sortwords e::aux l
		in 
		List.sort 
			(fun l l2 -> match (l,l2) with
				| ((_,_,y,_)::_)::_,((_,_,y2,_)::_)::_ -> compare y y2
				| _ -> 1
			) (aux parag)
	in
	let sortparags text =
		let rec aux = function
			| [] -> []
			| e::l -> sortlines e::aux l
		in 
		List.sort
			(fun l l2 -> match (l,l2) with
				| (((_,_,y,_)::_)::_)::_,(((_,_,y2,_)::_)::_)::_ -> compare y y2
				| _ -> 1
			) (aux text)
	in sortparags l


let getlists img =

	let w, h = img#get_size in
	let boxes = BoundingBoxes.get_boxes img () in
	let (avgw,avgh) = BoundingBoxes.average_box boxes in
	(* paragraphs *)
	let paragraphs = Segmentation.startRLSA img (||) (5 * avgw) (5 * avgh) 0 0 w h in
	let boxlist = BoundingBoxes.get_boxes paragraphs () in
		(* lines *)
		let boxlistlist = List.map
			(fun (x0, xmax, y0, ymax) ->
				let w, h = xmax - x0 + 1, ymax - y0 + 1 in
				let lines = Segmentation.startRLSA
					img (||) (4 * avgw) (avgh / 2) x0 y0 w h in
				BoundingBoxes.get_boxes lines ~offsetx:x0 ~offsety:y0 ()
			)
			boxlist in
		(BoundingBoxes.display_boxes img boxlist)#save_to_file "paragraphs.png";
  (* words *)
  let boxlistlistlist = List.map
    (fun boxlist ->
      List.map
        (fun (x0, xmax, y0, ymax) ->
          let w, h = xmax - x0 + 1, ymax - y0 + 1 in
          let words = Segmentation.startRLSA
            img (||) (avgw / 2) (avgh / 2) x0 y0 w h in
          BoundingBoxes.get_boxes words ~offsetx:x0 ~offsety:y0 ()
        )
        boxlist
    )
    boxlistlist in
  (* chars *)
  sort (List.map
    (fun boxlistlist ->
      List.map
        (fun boxlist ->
          List.map
            (fun (x0, xmax, y0, ymax) ->
              let w, h = xmax - x0 + 1, ymax - y0 + 1 in
              let chars = Segmentation.startRLSA
                img (&&) (avgw / 2) avgh x0 y0 w h in
              BoundingBoxes.get_boxes chars ~offsetx:x0 ~offsety:y0 ()
            )
            boxlist
        )
        boxlistlist
    )
 boxlistlistlist)
