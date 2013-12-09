(* let noise_reduction img =
    let w, h = img#get_size
    and black = OcsfmlGraphics.Color.black
    and new_img = new OcsfmlGraphics.image (`Copy(img)) in
    for i = 0 to w - 1 do
        for j = 0 to h - 1 do
            if not (i > 0 && new_img#get_pixel (i - 1) j = black
            || i < w - 1 && new_img#get_pixel (i + 1) j = black
            || j > 0 && new_img#get_pixel i (j - 1) = black
            || j < h - 1 && new_img#get_pixel i (j + 1) = black) then
                new_img#set_pixel i j OcsfmlGraphics.Color.white;
        done
    done;
    new_img *)
let noise_reduction img =
    MedianFilter.median_filter img
