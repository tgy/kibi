(*let getlists preprocessedimg =
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
  ignore (paragimg#save_to_file ("parags.bmp"));
  (charboxes, wordboxes, lineboxes, paragboxes)*)


let fusionchars l =
	let inside (xm,xM,_,_) (xm1,xM1,_,_) = (xm <= xm1 && xM1 <= xM) || (xm1 <= xm && xM <= xM1)
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
			| [] -> max_int, []
			| (x,_,_,_)::l -> x,nl
	in
	let sortwords line =
		let n = ref max_int in
		let rec aux = function
			| [] -> []
			| e::l -> let (newn,newe) = sortchars e in
			n := min newn !n;
			newe::aux l
		in (!n,aux line)
	in
	let sortlines parag =
		let n = ref max_int in
		let rec aux = function
			| [] -> []
			| e::l -> let (newn,newe) = sortwords e in
			n := min newn !n;
			newe::aux l
		in (!n,aux parag)
	in
	let sortparags text =
		let n = ref max_int in
		let rec aux = function
			| [] -> []
			| e::l -> let (newn,newe) = sortlines e in
			n := min newn !n;
			newe::aux l
		in (!n,aux text)
	in let (_,l) = sortparags l in l


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
