let draw_line img teta r path =
    let (w, h) = img#get_size in
    let new_img = 
        new OcsfmlGraphics.image (`Copy(img))
    and a = (-. cos teta /. sin teta) and b = r /. sin teta in
    for x = 0 to w - 1 do
        let y = -(int_of_float (a *. float_of_int x +. b)) in
        new_img#set_pixel x (if y < 0 then 0 
        else if y >= h then h - 1 else y) OcsfmlGraphics.Color.red;
    done;
    let _ = new_img#save_to_file path in ()

let hough_transform img =
    let (w, h) = img#get_size in
    let rmax = (w + h) in
    let m = ref (1, 0, 0)
    and pi = 3.14159265358979323846264338327950288419716939937510
    and acc = Array.make_matrix 180 (rmax * 2) 0 in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            if (img#get_pixel x y) = OcsfmlGraphics.Color.black then
                for teta = 0 to 179 do
                    let fteta = float_of_int teta *. 2. *. pi /. 360. in
                    let r = (int_of_float ((float_of_int x *. cos fteta) 
                    +. (float_of_int (-y) *. sin fteta))) in
                    acc.(teta).(r + rmax) <- acc.(teta).(r + rmax) + 1;
                    let (n, _, _) = !m in
                    if acc.(teta).(r + rmax) > n then
                        m := (acc.(teta).(r + rmax), teta, r)
                done;
        done
    done;
    let (n, teta, r) = !m in
    let fteta = float_of_int teta *. 2. *. pi /. 360. in
    (fteta, float_of_int r)

let get_angle img =
    let teta, r = hough_transform img in
    (
        teta +.
        if (r >= 0.) then
            3.14 /. 2.
        else
            3. *. 3.14 /. 2.
    )


