let mult m1 m2 =
    let h1, w1 = Array.length m1, Array.length m1.(0)
    and h2, w2 = Array.length m2, Array.length m2.(0) in
    if w1 = h2 then
        let m = Array.make_matrix h1 w2 0. in
        for i =  0 to h1 - 1 do
            for j = 0 to w2 - 1 do
                for k = 0 to w1 - 1 do
                    m.(i).(j) <- m.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
                done
            done
        done; m
    else
        raise (Failure "matrix size do not match")
