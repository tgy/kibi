let median a =
    Array.sort compare a;
    a.(Array.length a / 2)

let median_filter img =
    let w = 3 and width, height = img#get_size in
    let new_img = new OcsfmlGraphics.image (`Create(width, height)) in
    for i = 0 to width - 1 do
        for j = 0 to height - 1 do
            let a = Array.make (w * w) 0 in
            for di = -(w / 2) to (w / 2) do
                for dj = -(w / 2) to (w / 2) do
                    let ni =
                        if i + di < 0 then
                            0
                        else if i + di >= width then
                            width - 1
                        else i + di
                    and nj =
                        if j + dj < 0 then
                            0
                        else if j + dj >= height then
                            height - 1
                        else j + dj
                    in
                    let c = img#get_pixel ni nj in
                    a.((di + w / 2) + w * (dj + w / 2)) <- c.OcsfmlGraphics.Color.r
                done
            done;
            new_img#set_pixel i j {
                OcsfmlGraphics.Color.r = median a;
                OcsfmlGraphics.Color.g = median a;
                OcsfmlGraphics.Color.b = median a;
                OcsfmlGraphics.Color.a = 255
            }
        done
    done;
    new_img
