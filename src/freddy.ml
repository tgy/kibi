(*let getlists preprocessedimg =
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
  (charboxes, wordboxes, lineboxes, paragboxes)*)

let getlists img =
  let w, h = img#get_size in
  let boxes = BoundingBoxes.get_boxes img () in
  let (avgw,avgh) = BoundingBoxes.average_box boxes in
  (* paragraphs *)
  let paragraphs = Segmentation.startRLSA img (||) (5 * avgw) (5 * avgh) 0 0 w h in
  let boxlist = BoundingBoxes.get_boxes paragraphs () in
  (* lines *)
  let boxlistlist = List.map
    (fun (x0, xmax, y0, ymax) ->
      let w, h = xmax - x0 + 1, ymax - y0 + 1 in
      let lines = Segmentation.startRLSA
        img (||) (4 * avgw) (avgh / 2) x0 y0 w h in
      BoundingBoxes.get_boxes lines ~offsetx:x0 ~offsety:y0 ()
    )
    boxlist in
  (* words *)
  let boxlistlistlist = List.map
    (fun boxlist ->
      List.map
        (fun (x0, xmax, y0, ymax) ->
          let w, h = xmax - x0 + 1, ymax - y0 + 1 in
          let words = Segmentation.startRLSA
            img (||) (avgw / 2) (avgh / 2) x0 y0 w h in
          BoundingBoxes.get_boxes words ~offsetx:x0 ~offsety:y0 ()
        )
        boxlist
    )
    boxlistlist in
  (* chars *)
  List.map
    (fun boxlistlist ->
      List.map
        (fun boxlist ->
          List.map
            (fun (x0, xmax, y0, ymax) ->
              let w, h = xmax - x0 + 1, ymax - y0 + 1 in
              let chars = Segmentation.startRLSA
                img (&&) (avgw / 2) avgh x0 y0 w h in
              BoundingBoxes.get_boxes chars ~offsetx:x0 ~offsety:y0 ()
            )
            boxlist
        )
        boxlistlist
    )
    boxlistlistlist
