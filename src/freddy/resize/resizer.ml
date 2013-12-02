module O = OcsfmlGraphics

let get_new_size w h a =
  if w > h then
    (a, a * h / w)
  else
    (a * w / h, a)

let is_in_bounds x y w h = x >= 0 && y >= 0 && x < w && y < h

let resize img (bx1, by1) (bx2, by2) new_size =
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

