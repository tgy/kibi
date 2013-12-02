let color_scal_mult s c = 
    {
        OcsfmlGraphics.Color.r = int_of_float
        (( float_of_int c.OcsfmlGraphics.Color.r) *. s);
        OcsfmlGraphics.Color.g = int_of_float
        (( float_of_int c.OcsfmlGraphics.Color.g) *. s);
        OcsfmlGraphics.Color.b = int_of_float
        (( float_of_int c.OcsfmlGraphics.Color.b) *. s);
        OcsfmlGraphics.Color.a = 255
    }

let color_bound c = 
    let  restrain = function
        | n when n < 0 -> 0
        | n when n > 255 -> 255
        | n -> n
    in
    {
        OcsfmlGraphics.Color.r = restrain c.OcsfmlGraphics.Color.r;
        OcsfmlGraphics.Color.g = restrain c.OcsfmlGraphics.Color.g;
        OcsfmlGraphics.Color.b = restrain c.OcsfmlGraphics.Color.b;
        OcsfmlGraphics.Color.a = 255
    }

let plus c1 c2 =
    {
        OcsfmlGraphics.Color.r = c1.OcsfmlGraphics.Color.r +
        c2.OcsfmlGraphics.Color.r;
        OcsfmlGraphics.Color.g = c1.OcsfmlGraphics.Color.g +
        c2.OcsfmlGraphics.Color.g;
        OcsfmlGraphics.Color.b = c1.OcsfmlGraphics.Color.b +
        c2.OcsfmlGraphics.Color.b;
        OcsfmlGraphics.Color.a = 255
    }

let convolution img matrix = 
    if (Array.length matrix >= 3) then
        let (width,height) = img#get_size in
        let w = Array.length matrix in
        if w mod 2 = 1 && w = Array.length matrix.(0) then
            let new_img = new OcsfmlGraphics.image (`Create(width, height)) in
            for i = 0 to width - 1 do
                for j = 0 to height - 1 do
                    let sum = ref {OcsfmlGraphics.Color.r = 0;
                    OcsfmlGraphics.Color.g = 0;
                    OcsfmlGraphics.Color.b = 0;
                    OcsfmlGraphics.Color.a = 255} in
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
                            let s = matrix.(di + w / 2).(dj + w / 2) in
                            let c = color_scal_mult s (img#get_pixel ni nj) in
                            sum := plus !sum c
                        done
                    done;
                    sum := color_bound !sum;
                    new_img#set_pixel (i) (j) !sum
                done
            done;
            new_img
        else
            raise
            (Failure "convolution matrix must be a square with an odd size")
    else
        raise (Failure "convolution matrix size must be at least 3");;


let gaussian img =
    let matrix = [|
        [|1./. 273.;4./. 273.;7./. 273.;4./. 273.;1./. 273.|];
        [|4./. 273.;16./. 273.;26./. 273.;16./. 273.;4./. 273.|];
        [|7./. 273.;26./. 273.;41./. 273.;26./. 273.;7./. 273.|];
        [|4./. 273.;16./. 273.;26./. 273.;16./. 273.;4./. 273.|];
        [|1./. 273.;4./. 273.;7./. 273.;4./. 273.;1./. 273.|]
    |] in
    convolution img matrix;;

let edgedetect img =
    let matrix = [|[|0.;1.;0.|];[|1.;-4.;1.|];[|0.;1.;0.|]|] in
    convolution img matrix;;
