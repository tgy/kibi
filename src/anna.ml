let sum a1 a2 =
  let a = Array.make (Array.length a1) 0. in
  for i = 0 to Array.length a1 - 1 do
    a.(i) <- a1.(i) +. a2.(i)
  done; a

let avg a n =
  for i = 0 to Array.length a - 1 do
    a.(i) <- a.(i) /. float n
  done; a

let identify_char nets pixvector =
  let rec aux n acc = function
    | [] -> CharIdentification.interpret_network_output (avg acc n)
    | net :: l ->
        let a = net#propagate pixvector
        in aux (n + 1) (sum a acc) l
  in aux 0 (Array.make (Array.length (List.hd nets)#get_out_a) 0.) nets
