(** apply any square matrix as a convolution matrix on the image *)
val convolution :
  < get_pixel : int -> int -> OcsfmlGraphics.Color.t; get_size : int * int;
    .. > ->
  float array array -> OcsfmlGraphics.image

(** gaussian blur *)
val gaussian :
  < get_pixel : int -> int -> OcsfmlGraphics.Color.t; get_size : int * int;
    .. > ->
  OcsfmlGraphics.image

(** edge detection *)
val edgedetect :
  < get_pixel : int -> int -> OcsfmlGraphics.Color.t; get_size : int * int;
    .. > ->
  OcsfmlGraphics.image
