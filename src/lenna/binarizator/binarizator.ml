(** Binarizes an image with the given threshold *)
let binarize img threshold =
    let (w,h) = img#get_size in
    let new_img = 
        new OcsfmlGraphics.image (`Color (OcsfmlGraphics.Color.white,w,h)) in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            let gray = (img#get_pixel x y).OcsfmlGraphics.Color.r in
            if gray < threshold then
                new_img#set_pixel x y OcsfmlGraphics.Color.black
        done
    done;
    new_img;;

(** Binarizes an image using Otsu's method to find out wich thresold is
the best for the given image
@return img OcsfmlGraphics.image *)
let binarize img =
    let histogram = Array.make 256 0
    and max_between_class_variance = ref (0, min_float) in
    let (w,h) = img#get_size in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            let gray = (img#get_pixel x y).OcsfmlGraphics.Color.r in
            histogram.(gray) <- histogram.(gray) + 1
        done
    done;
    for threshold = 0 to 255 do
        let classb = ref 0 and classw = ref 0
        and meanb = ref 0 and meanw = ref 0 in
        for i = 0 to threshold - 1 do
            classb := !classb + histogram.(i);
            meanb := !meanb + i * histogram.(i)
        done;
        for i = threshold to 255 do
            classw := !classw + histogram.(i);
            meanw := !meanw + i * histogram.(i)
        done;
        let fmeanb = float_of_int !meanb /. float_of_int !classb
        and fmeanw = float_of_int !meanw /. float_of_int !classw
        and size = float_of_int (w * h) in
        let weightw = float_of_int !classw /. size
        and weightb = float_of_int !classb /. size in
        let tmp = fmeanb -. fmeanw in
        let between_class_variance = weightw *. weightb *. tmp *. tmp in
        if between_class_variance > (snd !max_between_class_variance) then
            max_between_class_variance := (threshold, between_class_variance)
    done;
    binarize img (fst !max_between_class_variance)

