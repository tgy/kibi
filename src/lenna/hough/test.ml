open OcsfmlGraphics

let main () =
    begin
        let img = new image 
        (`File "../test/in/big.jpg") in
        let new_img = HoughPreprocess.get_dotted img in
        new_img#save_to_file "new_img.png"
    end

let _ = main ()
