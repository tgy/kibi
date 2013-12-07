let preprocess img =
	let binarized = Binarizator.binarize img in
	let preprocessed = HoughPreprocess.get_dotted binarized in
	let theta, r = HoughTransform.hough_transform preprocessed in
	let angle = (theta +.
		if (r >= 0.) then 3.14 /. 2.
		else 3. *. 3.14 /. 2.) in
	let rotated = Binarizator.binarize (Rotation.rotate binarized angle) in
	let boxes = BoundingBoxes.get_boxes rotated in
	let mwidth, mheight = BoundingBoxes.average_box boxes in
	(*let boxes = BoundingBoxes.remove_small_and_large boxes (mwidth, mheight) in
	let removed = (BoundingBoxes.display_boxes rotated boxes) in*)
	(binarized,preprocessed,rotated)

