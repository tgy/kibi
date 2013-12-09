open OcsfmlGraphics

let get_bounds = function
    | [] -> raise (Failure 
    "Lena.Hough.get_bounds: trying to get bounds of an empty pixel list.")
    | (x,y)::l ->
            let rec aux = function
                | (a,b,c,d),[] -> (a, b, c, d)
                | (a,b,c,d),(x,y)::l ->
                        aux ((
                            (min a x),
                            (max b x),
                            (min c y),
                            (max d y)),l)
            in
            aux ((x,x,y,y),l)

let is_in_range w h (x,y) =
    x >= 0 && y >= 0 && x < w && y < h

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

let display_boxes img boxes =
    let new_img = new OcsfmlGraphics.image(`Copy(img)) in
    let rec aux = function
        | [] -> new_img
        | (minx, maxx, miny, maxy) :: l ->
            for i = minx to maxx do
                new_img#set_pixel i miny OcsfmlGraphics.Color.red;
                new_img#set_pixel i maxy OcsfmlGraphics.Color.red
            done;
            for j = miny to maxy do
                new_img#set_pixel minx j OcsfmlGraphics.Color.red;
                new_img#set_pixel maxx j OcsfmlGraphics.Color.red
            done; aux l
    in aux boxes
    
let get_boxes img ?(offsetx = 0) ?(offsety = 0) () =
    let (w,h) = img#get_size in
    let tmp_img = new OcsfmlGraphics.image(`Copy(img)) in
    let boxes = ref [] in
    for x = w - 1 downto 0 do
        for y = h - 1 downto 0  do
            if tmp_img#get_pixel x y = OcsfmlGraphics.Color.black then
            begin
                let l = ref [(x,y)]
                and l2 = ref [(x,y)] in
                tmp_img#set_pixel x y OcsfmlGraphics.Color.white;
                while !l <> [] do
                    match !l with
                    | [] -> ()
                    | (x,y)::tl ->
                        l := tl;
                        let neighbors = get_neighbors (x,y) tmp_img in
                        let rec aux = function
                            | [] -> ()
                            | (x1, y1) :: tl ->
                                l := (x1, y1) :: !l;
                                l2 := (x1, y1) :: !l2;
                                tmp_img#set_pixel x1 y1
                                OcsfmlGraphics.Color.white;
                                aux tl
                        in aux neighbors;
                done;
                let (minx,maxx,miny,maxy) = get_bounds !l2 in
                boxes := (minx + offsetx, maxx + offsetx, miny + offsety, maxy + offsety) :: !boxes
            end
        done
    done; !boxes

let average_box boxes =
		if List.length boxes = 0 then
			0,0
		else begin
			let rec aux = function
					| [] -> (0, 0, 0)
					| (minx, maxx, miny, maxy) :: l -> let (n, avgw, avgh) = aux l in
							(n + 1), (maxx - minx) + avgw, (maxy - miny) + avgh
			in
			let (n, avgw, avgh) = aux boxes in
			(avgw / n, avgh / n)
		end

let remove_small_and_large boxes (avgw, avgh) =
    let espwinf, espwsup, esphinf, esphsup, nwinf, nwsup, nhinf, nhsup =
        let rec aux = function
            | [] -> (0, 0, 0, 0, 0, 0, 0, 0)
            | (minx, maxx, miny, maxy) :: l ->
                let dw = maxx - minx - avgw and dh = maxy - miny - avgh in
                let (tavgwinf, tavgwsup, tavghinf, tavghsup, nwinf, nwsup, nhinf, nhsup) = aux l in
                if dw < 0 then
                    if dh < 0 then
                        (abs dw + tavgwinf, tavgwsup, abs dh + tavghinf, tavghsup, nwinf + 1, nwsup, nhinf + 1, nhsup)
                    else
                        (abs dw + tavgwinf, tavgwsup, tavghinf, abs dh + tavghsup, nwinf + 1, nwsup, nhinf, nhsup + 1)
                else
                    if dh < 0 then
                        (tavgwinf, abs dw + tavgwsup, abs dh + tavghinf, tavghsup, nwinf, nwsup + 1, nhinf + 1, nhsup)
                    else
                        (tavgwinf, abs dw + tavgwsup, tavghinf, abs dh + tavghsup, nwinf, nwsup + 1, nhinf, nhsup + 1) in
        aux boxes in
    let varwinf, varwsup, varhinf, varhsup = espwinf / nwinf, espwsup / nwsup, esphinf / nhinf, esphsup / nhsup in
    let rec aux = function
        | [] -> []
        | (minx, maxx, miny, maxy) as box :: l -> match (maxx - minx) - avgw, (maxy - miny) - avgh with
            | (dw, dh) when dw < 0 && dh < 0 ->
                if abs dw > 2 * varwinf || abs dh > 2 * varhinf then
                    aux l
                else
                    box :: aux l
            | (dw, dh) when dw < 0 && dh > 0 ->
                if abs dw > 2 * varwinf || abs dh > 2 * varhsup then
                    aux l
                else
                    box :: aux l
            | (dw, dh) when dw > 0 && dh < 0 ->
                if abs dw > 2 * varwsup || abs dh > 2 * varhinf then
                    aux l
                else
                    box :: aux l
            | (dw, dh) ->
                if abs dw > 2 * varwsup || abs dh > 2 * varhsup then
                    aux l
                else
                    box :: aux l
    in aux boxes
