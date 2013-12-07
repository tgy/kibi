let printf s f =
    print_endline (Printf.sprintf "time -> %s: %f" s f)

let main () =
    begin
        let input_img = ref "test/in/default.jpg"
        and output_dir = ref "test/out/" in
        Arg.parse [
            ("-i", Arg.String(fun s -> input_img := s), "input image");
            ("-o", Arg.String(fun s -> output_dir := s), "output dir")
        ] (fun _ -> ()) "test.native [-i input_img] [-o output_dir]";
        if (!output_dir).[String.length !output_dir - 1] <> '/' then
            output_dir := !output_dir ^ "/";
				let img = new OcsfmlGraphics.image (`File !input_img) in
        printf "reading base image from file" (Sys.time ());
				(*let grayscaled = Grayscale.grayscale denoised in
				printf "Grayscale.grayscale" (Sys.time());
				ignore (grayscaled#save_to_file (!output_dir ^ "3grayscaled.png"));*)
        let binarized = Binarizator.binarize img in
        printf "Binarizator.binarize" (Sys.time ());
        ignore (binarized#save_to_file (!output_dir ^ "4binarized.png"));
        let denoised = binarized in (*NoiseReduction.noise_reduction binarized in
        printf "NoiseReduction.noise_reduction" (Sys.time ());
        ignore (denoised#save_to_file (!output_dir ^ "2denoised.png"));*)
        let preprocessed = HoughPreprocess.get_dotted denoised in
        printf "HoughPreprocess.get_dotted" (Sys.time ());
        let _ = preprocessed#save_to_file (!output_dir ^ "5dotted.png") in
        let teta, r = HoughTransform.hough_transform binarized in
        HoughTransform.draw_line binarized teta r (!output_dir ^ "6hough.png");
        printf "HoughTransform.hough_transform" (Sys.time ());
        let angle = (teta +.
            if (r >= 0.) then 3.14 /. 2.
            else 3. *. 3.14 /. 2.) in
        let rotated = Binarizator.binarize (Rotation.rotate denoised angle) in
        printf "Rotation.rotate" (Sys.time ());
        ignore (rotated#save_to_file (!output_dir ^ "7rotated.png"));
        let boxes = BoundingBoxes.get_boxes rotated in
        let (mwidth, mheight) = BoundingBoxes.average_box boxes in
        let boxes = BoundingBoxes.remove_small_and_large boxes (mwidth, mheight) in
        Printf.printf "average box : %dx%d\n" mwidth mheight;
        ignore ((BoundingBoxes.display_boxes rotated boxes)#save_to_file (!output_dir ^ "bbox.png"));
        img#save_to_file (!output_dir ^ "1img.png")
    end

let _ =
    main ()
