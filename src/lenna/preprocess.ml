module O = OcsfmlGraphics
let preprocess input_file =
	let img = new O.image (`File !input_file) in
	let binarized = Binarizator.binarize img in
	let preprocessed = HoughProcess.get_dotted denoised in
	let theta, r = HoughTransform.hough_transform binarized in
	let angle = (theta +.
		if (r >= 0.) then 3.14 /. 2.
		else 3. *. 3.14 /. 2.) in
	let rotated = Binarizator.binarize (Rotation.rotate denoised angle) in
	let boxes = BoundingBoxes.get_boxes rotated in
	let mwidth, mheight = BoundingBoxes.average_box boxes in
	let boxes = BoundingBoxes.remove_small_and_large boxes (mwidth, mheight) in
	(binarized,preprocessed,rotated,boxes)
