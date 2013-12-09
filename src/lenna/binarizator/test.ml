let main () =
    begin
        let img = new OcsfmlGraphics.image 
        (`File "../test/in/to-binarize.png") in
        let binarized_img = Binarizator.binarize img in
        binarized_img#save_to_file "../test/out/binarized.png"
    end

let _ = main ()
