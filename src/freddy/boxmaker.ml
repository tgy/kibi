(*Is the box a = (axl,axh,ayl,ayh) included in b = (bxl,bxh,byl,byh)?*)
let isainb a b =
  let (axl,axh,ayl,ayh) = a and
  (bxl,bxh,byl,byh) = b in
  axl >= bxl && axh <= bxh && ayl >= byl && ayh <= byh

let formatit charlst wordlst linelist paraglist =


let main () =
  let a = (0,10,0,10) and b = (0,10,0,10) in
  Printf.printf "%B\n" (isainb a b);
  flush stdout

let _ = main ()
