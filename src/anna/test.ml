module O = OcsfmlGraphics

(*print_string
(CharIdentification.identify "weights/weights1024x90x90.txt"
(CharIdentification.image_to_input "../setgen/out/65/0.png"))*)

let font_number = "5"

while true do
  let c = (read_line ()).[0] in
  let ascii = string_of_int (int_of_char c) in
  let path = "../setgen/out/" ^ ascii ^ "/" ^ font_number ^ ".png" in
  let pixvect = image_to_input path in

done
