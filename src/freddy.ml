let getlists preprocessedimg =
  let boxes = BoundingBoxes.get_boxes preprocessedimg in
  let (avgw,avgh) = BoundingBoxes.average_box boxes in
  let chars = Segmentation.startRLSA preprocessedimg (&&) avgw avgh in
  let words = Segmentation.startRLSA preprocessedimg (||) (avgw) (avgh / 2) in
  let lines = Segmentation.startRLSA preprocessedimg (||) (2 * avgw) (avgh / 2) in
  let parags = Segmentation.startRLSA preprocessedimg (||) (5 * avgw) (5 * avgh) in
  let charboxes = BoundingBoxes.get_boxes chars in
  let charimg = BoundingBoxes.display_boxes preprocessedimg charboxes in
  let wordboxes = BoundingBoxes.get_boxes words in
  let wordimg = BoundingBoxes.display_boxes preprocessedimg wordboxes in
  let lineboxes = BoundingBoxes.get_boxes lines in
  let lineimg = BoundingBoxes.display_boxes preprocessedimg lineboxes in
  let paragboxes = BoundingBoxes.get_boxes parags in
  let paragimg = BoundingBoxes.display_boxes preprocessedimg paragboxes in
  ignore (charimg#save_to_file ("chars.bmp"));
  ignore (wordimg#save_to_file ("words.bmp"));
  ignore (lineimg#save_to_file ("lines.bmp"));
  ignore (paragimg#save_to_file ("parags.bmp"));
  (charboxes, wordboxes, lineboxes, paragboxes)
