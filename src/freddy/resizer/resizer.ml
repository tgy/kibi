module O = OcsfmlGraphics

let get_new_size w h a =
  if w > h then
    (a, a * h / w)
  else
    (a * w / h, a)

let is_in_bounds x y w h = x >= 0 && y >= 0 && x < w && y < h

(* Color to float (1. or 0.) *)
let ctf color = if color.O.Color.r = 0 then 1. else 0.

let crop_bounds img (b1x, b1y) (b2x, b2y) =
  let nb1x = ref b2x and nb1y = ref b2y
  and nb2x = ref b1x and nb2y = ref b1y in
  for i = b1x to b2x do
    for j = b1y to b2y do
      if (img#get_pixel i j) = O.Color.black then
        begin
          if i < !nb1x then nb1x := i;
          if i > !nb2x then nb2x := i;
          if j < !nb1y then nb1y := j;
          if j > !nb2y then nb2y := j
        end
    done
  done; (!nb1x, !nb1y), (!nb2x, !nb2y)


(* Resize an image and return a square image *)
let resize img new_size =
  let new_img = new O.image (`Color (O.Color.white, new_size, new_size)) in
  let (w:int), (h:int) = img#get_size in
  let nw, nh = get_new_size w h new_size in
  let xr = (w lsl 16) / nw + 1
  and yr = (h lsl 16) / nh + 1 in
  let offx = (new_size - nw) / 2
  and offy = (new_size - nh) / 2 in
  for i = offx to offx + nw - 1 do
    for j = offy to offy + nh - 1 do
      let x2, y2 = (((j - offy) * xr) lsr 16), (((i - offx) * yr) lsr 16) in
      if is_in_bounds y2 x2 w h then
        new_img#set_pixel i j (img#get_pixel y2 x2)
    done
  done;
  new_img

(* Resize an image and return a vector of {0., 1.} *)
let resize img new_size =
  let (w:int), (h:int) = img#get_size in
  let new_img = new O.image (`Color (O.Color.white, new_size, new_size)) in
  let nw, nh = get_new_size w h new_size in
  let xr = (w lsl 16) / nw + 1
  and yr = (h lsl 16) / nh + 1 in
  let offx = (new_size - nw) / 2
  and offy = (new_size - nh) / 2 in
  for i = offx to offx + nw - 1 do
    for j = offy to offy + nh - 1 do
      let x2, y2 = (((j - offy) * xr) lsr 16), (((i - offx) * yr) lsr 16) in
      if is_in_bounds y2 x2 w h then
        new_img#set_pixel i j (img#get_pixel y2 x2)
    done
  done;
  new_img

let get_pixvector img (b1x, b1y) (b2x, b2y) new_size =
	let (b1x, b1y), (b2x, b2y) = crop_bounds img (b1x, b1y) (b2x, b2y) in
	(*print_endline (Printf.sprintf "b1x %d ; b1y %d ; b2x %d ; b2y %d" b1x b1y b2x
	b2y);*)
	let w, h = (b2x - b1x + 1), (b2y - b1y + 1)
	and pixvector = Array.make (new_size * new_size) 0. in
	(*print_endline (Printf.sprintf "w: %d ; h: %d" w h);*)
	let nw, nh = get_new_size w h new_size in
	(*print_endline (Printf.sprintf "nw: %d ; nh: %d" nw nh);*)
	let xr = (w lsl 16) / nw + 1
	and yr = (h lsl 16) / nh + 1 in
	let offx = (new_size - nw) / 2
	and offy = (new_size - nh) / 2 in
	(*print_endline (Printf.sprintf "offx: %d ; offy: %d" offx offy);*)
	for i = offy to offy + nh - 1 do
		for j = offx to offx + nw - 1 do
			(*print_endline (Printf.sprintf "(%d;%d)" i j);*)
			let x2, y2 = (((j - offx) * xr) lsr 16), (((i - offy) * yr) lsr 16) in
			(*print_endline (Printf.sprintf "(x2:%d;y2:%d)" x2 y2);*)
			pixvector.(i * new_size + j) <- ctf (img#get_pixel (b1x + x2) (b1y + y2))
		done
	done; pixvector

let pixvector_to_img pixvector size =
  let img = new O.image (`Color (O.Color.white, size, size)) in
  for i = 0 to Array.length pixvector - 1 do
    let x = i mod size
    and y = i / size in
    if pixvector.(i) = 1. then
      img#set_pixel x y O.Color.black
  done; img

