module O = OcsfmlGraphics

let get_nearest_bb (x, y) (w, h) =
  let clamp max x = if x < 0 then 0 else if x > max - 1 then max - 1 else x in
  let x1, y1 = clamp w (int_of_float x), clamp h (int_of_float y) in
  let x2, y2 = clamp w (x1 + 1), clamp h (y1 + 1) in
  (x1, y1), (x2, y2)

let rot x y (cos_theta, sin_theta) =
  let x = float_of_int x and y = float_of_int y in
  let x' = x *. cos_theta +. y *. sin_theta
  and y' = y *. cos_theta -. x *. sin_theta in x', y'

let rotinv x y (cos_theta, sin_theta) =
  let x = float_of_int x and y = float_of_int y in
  let x' = x *. cos_theta -. y *. sin_theta
  and y' = x *. sin_theta +. y *. cos_theta in x', y'

let rotate img theta =
  let w, h = img#get_size in let fw, fh = (float_of_int w, float_of_int h) in
  let is_in_bound x y = x >= 0 && x < w && y >= 0 && y < h in
  let cos_theta = cos theta and sin_theta = sin theta in
  let cs = cos_theta, sin_theta in
  let nw, nh =
    let ah = cos_theta *. fh and oh = sin_theta *. fh
    and aw = cos_theta *. fw and ow = sin_theta *. fw in
    (int_of_float (oh +. aw), int_of_float (ow +. ah)) in
  let new_img = new O.image (`Color (O.Color.white, nw, nh)) in
  for x = 0 to nw - 1 do
    for y = 0 to nh - 1 do
      let px, py = (x - nw / 2), (y - nh / 2) in
      let px, py = rot px py cs in
      let px, py = (px +. fw /. 2.), (py +. fh /. 2.) in
      if is_in_bound (int_of_float px) (int_of_float py) then
      let (x1, y1), (x2, y2) = get_nearest_bb (px, py) (w, h) in
      let c11 = img#get_pixel x1 y1 and c22 = img#get_pixel x2 y2
      and c12 = img#get_pixel x1 y2 and c21 = img#get_pixel x2 y1 in
      let x1, y1 = float_of_int x1, float_of_int y1
      and x2, y2 = float_of_int x2, float_of_int y2 in
      let aux1 = (x2 -. px) /. (x2 -. x1) and aux2 = (px -. x1) /. (x2 -. x1)
      and aux3 = (y2 -. py) /. (y2 -. y1) and aux4 = (py -. y1) /. (y2 -. y1)
      in
      let a comp11 comp21 comp12 comp22 =
        let comp11 = float_of_int comp11 and comp12 = float_of_int comp12
        and comp21 = float_of_int comp21 and comp22 = float_of_int comp22 in
        let r1 = aux1 *. comp11 +. aux2 *. comp21
        and r2 = aux1 *. comp12 +. aux2 *. comp22 in
        let comp = int_of_float (aux3 *. r1 +. aux4 *. r2) in
        if comp > 255 then 255 else if comp < 0 then 0 else comp
      in
      new_img#set_pixel x y {
        O.Color.r = a c11.O.Color.r c21.O.Color.r c12.O.Color.r c22.O.Color.r;
        O.Color.g = a c11.O.Color.g c21.O.Color.g c12.O.Color.g c22.O.Color.g;
        O.Color.b = a c11.O.Color.b c21.O.Color.b c12.O.Color.b c22.O.Color.b;
        O.Color.a = 255
      }
    done
  done; new_img

