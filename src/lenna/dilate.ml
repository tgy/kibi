let best f = function
    | [] -> raise (Failure "empty list")
    | hd :: tl ->
            let rec best_rec m = function
                | [] -> m
                | e :: tail -> best_rec (if f e m then e else m) tail
            in best_rec hd tl

let max = best (fun a b -> a < b)
let min = best (fun a b -> a > b)

let filter img best =
    let (width, height) = img#get_size in
    let new_img = new OcsfmlGraphics.image
    (`Color (OcsfmlGraphics.Color.white, width, height))
    and w = 3 in
    for i = 0 to width - 1 do
        for j = 0 to height - 1 do
            let l = ref [] in
            for di = - w / 2 to w / 2 do
                for dj = - w / 2 to w / 2 do
                    let ni =
                        if i + di < 0 then
                            0
                        else if i + di >= width then
                            width - 1
                        else
                            i + di
                    and nj =
                        if j + dj < 0 then
                            0
                        else if j + dj >= height then
                            height - 1
                        else
                            j + dj
                    in
                    l := (img#get_pixel ni nj).OcsfmlGraphics.Color.r :: !l
                done
            done;
            let best_color = best !l in
            new_img#set_pixel i j {
                OcsfmlGraphics.Color.r = best_color;
                OcsfmlGraphics.Color.g = best_color;
                OcsfmlGraphics.Color.b = best_color;
                OcsfmlGraphics.Color.a = 255
            }
        done
    done;
    new_img

let dilate img = filter img max
let thin img = filter img min

