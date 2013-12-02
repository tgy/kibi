module O = OcsfmlGraphics

let invert img =
  let w, h = img#get_size in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      if img#get_pixel i j = O.Color.black then
        img#set_pixel i j O.Color.white
      else
        img#set_pixel i j O.Color.black
    done
  done;
  img
