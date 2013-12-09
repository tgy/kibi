open OcsfmlGraphics

let get_bounds = function
    | [] -> raise (Failure 
    "Lena.Hough.get_bounds: trying to get bounds of an empty pixel list.")
    | (x,y)::l ->
            let rec aux = function
                | (a,b,c,d),[] -> (a,b,c,d)
                | (a,b,c,d),(x,y)::l ->
                        aux ((
                            (min a x),
                            (max b x),
                            (min c y),
                            (max d y)),l)
            in
            aux ((x,x,y,y), l)

let get_bounds_center (min_x,max_x,min_y,max_y) =
    ((max_x + min_x)/2,((max_y + min_y)/2))

let get_center l =
    get_bounds_center (get_bounds l)

let is_in_range w h (x,y) =
    x >= 0 && y >= 0 && x < w && y < h
(*
let get_dotted img =
    let (w,h) = img#get_size in
    let new_img = new OcsfmlGraphics.image(`Copy(img)) in
    for x = 0 to w - 2 do
        for y = 0 to h - 2 do
            let rec aux l = function
                | (x,y) when is_in_range w h (x,y)
                && new_img#get_pixel x y = Color.black ->
                    new_img#set_pixel x y Color.white;
                    let x1 = x + 1 in
                    let y1 = y + 1 in
                    let xm1 = x - 1 in
                    let ym1 = y - 1 in
                    (x,y)::(aux (aux (aux (aux (aux (aux (aux (aux l
                    (x1,y1)) (x,y1)) (xm1,y1)) (x1,y)) (xm1,y)) (x1,ym1))
                    (x,ym1)) (xm1,ym1))
                | _ -> l
            in
            let l = aux [] (x,y) in
            if l != [] then
                let (c_x,c_y) = get_center l in
                new_img#set_pixel c_x c_y Color.black;
        done
    done;
    new_img
*)

let get_neighbors (x,y) img =
    let (w,h) = img#get_size
    and l = ref [] in
    
    let add (x,y) l =
        if is_in_range w h (x,y) && img#get_pixel x y = Color.black then
            l := (x,y)::!l;
    in

    let x1 = x + 1 and y1 = y + 1
    and x_1 = x - 1 and y_1 = y - 1 in

    add (x_1,y_1) l; add (x_1,y) l;
    add (x_1,y1) l; add (x,y_1) l;
    add (x,y1) l; add (x1,y_1) l;
    add (x1,y) l; add (x1,y1) l;
    
    !l
    
let get_dotted img =
    let (w,h) = img#get_size in
    let new_img = new OcsfmlGraphics.image(`Copy(img)) in
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            if new_img#get_pixel x y = OcsfmlGraphics.Color.black then
            begin
                let l = ref [(x,y)]
                and l2 = ref [(x,y)] in
                new_img#set_pixel x y OcsfmlGraphics.Color.white;
                while !l <> [] do
                    match !l with
                    | [] -> ()
                    | (x,y)::tl ->
                        l := tl;
                        let neighbors = get_neighbors (x,y) new_img in
                        let rec aux = function
                            | [] -> ()
                            | (x1, y1) :: tl ->
                                l := (x1, y1) :: !l;
                                l2 := (x1, y1) :: !l2;
                                new_img#set_pixel x1 y1
                                OcsfmlGraphics.Color.white;
                                aux tl
                        in aux neighbors;
                done;
                let (c_x,c_y) = get_center !l2 in
                new_img#set_pixel c_x c_y OcsfmlGraphics.Color.black
            end
        done
    done;
    new_img


