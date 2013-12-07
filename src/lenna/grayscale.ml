let grayscale img =
    let m = ref 0 and (w, h) = img#get_size
    and new_img = new OcsfmlGraphics.image (`Copy(img)) in
    for i = 0 to w - 1 do
        for j = 0 to h - 1 do
            let color = img#get_pixel i j in
            let gray = (
                color.OcsfmlGraphics.Color.r +
                color.OcsfmlGraphics.Color.g +
                color.OcsfmlGraphics.Color.b) / 3 in
            m := max !m gray;
            new_img#set_pixel i j {OcsfmlGraphics.Color.r = gray;
                 OcsfmlGraphics.Color.g = gray;
                 OcsfmlGraphics.Color.b = gray;
                 OcsfmlGraphics.Color.a = 255}
        done
    done;
    for i = 0 to w - 1 do
        for j = 0 to h - 1 do
            let gray = (new_img#get_pixel i j).OcsfmlGraphics.Color.r in
            new_img#set_pixel i j {
                OcsfmlGraphics.Color.r = gray * 255 / !m;
                OcsfmlGraphics.Color.g = gray * 255 / !m;
                OcsfmlGraphics.Color.b = gray * 255 / !m;
                OcsfmlGraphics.Color.a = 255
            }
        done
    done;
    new_img;;
