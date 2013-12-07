(*Is the box a = ((axl,axh),(ayl,ayh)) included in b = ((bxl,bxh),(byl,byh))?*)
let isainb a b =
  let ((axl,axh),(ayl,ayh)) = a and
  ((bxl,bxh),(byl,byh)) = b in
  axl <= bxl && axh <= bxh && ayl <= byl && ayh <= byh

let main () =
  Printf.sprintf "%B\n" (isainb ((0,10),(0,10)) ((4,6),(4,6)));
  flush stdout

let _ = main ()
