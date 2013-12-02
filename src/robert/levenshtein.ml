let levenshtein str1 str2 = 
    let n = String.length str1
    and m = String.length str2 in
    let	matrix = Array.make_matrix (n+1) (m+1) 0
    and cost = ref 0
    in
    for i = 1 to n do
        matrix.(i).(0) <- i
    done;
    for j = 1 to m do
        matrix.(0).(j) <- j
    done;
    for i = 1 to n do
        for j = 1 to m do
            if str1.[i-1] = str2.[j-1] then
                cost := 0
            else 
                cost := 1;
                matrix.(i).(j) <- min (min	(matrix.(i-1).(j) + 1)
                (matrix.(i).(j-1) + 1))
                (matrix.(i-1).(j-1) + !cost)
        done;
    done;
    matrix.(n).(m)
