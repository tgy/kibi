Random.self_init ()

let add_bias input =
    let ninput = Array.make (Array.length input + 1) 0. in
    ninput.(0) <- 1.;
    for i = 1 to Array.length ninput - 1 do
        ninput.(i) <- input.(i - 1)
    done;
    ninput

let feed_forward input weights =
    Array.init (Array.length weights) (fun i ->
        let a = ref 0. in
        for j = 0 to Array.length weights.(i) - 1 do
            a := !a +. weights.(i).(j) *. input.(j)
        done;
        !a
    )

let g a =
    1. /. (1. +. (2.71828182846 ** -.a))

let g m =
    Array.init (Array.length m) (fun i -> g m.(i))

let random_weights neurons transitions =
    Array.init neurons (fun _ -> 
        Array.init transitions (fun _ -> Random.float 1. -. 0.5)
    )

class network (* paramaters *) (c, a, t, m) (* neuron number *) (incount, hidcount, outcount) =

    object (self)

        val mutable hid_weights = Array.make_matrix hidcount (incount + 1) 0.

        val mutable out_weights = Array.make_matrix outcount (hidcount + 1) 0.

        val mutable hid_a = Array.make (hidcount + 1) 0.

        val mutable out_a = Array.make (outcount) 0.

        val mutable last_delta_hid =
            Array.make_matrix hidcount (incount + 1) 0.

        val mutable last_delta_out =
            Array.make_matrix outcount (hidcount + 1) 0.

        method randomize_weights =
            hid_weights <- random_weights hidcount (incount + 1);
            out_weights <- random_weights outcount (hidcount + 1)

        method show_result =
            for i = 0 to (Array.length out_a - 1) do
                print_endline (Printf.sprintf "result: %f" out_a.(i))
            done

        method set_hid_weights weights =
            hid_weights <- weights

        method set_out_weights weights =
            out_weights <- weights

        method get_out_a = out_a

        method get_hid_weights = hid_weights

        method propagate input =
            hid_a <- add_bias(g (feed_forward (add_bias input) hid_weights));
            out_a <- g (feed_forward hid_a out_weights); out_a

        method backpropagate iter (x,y) =
        begin 
            let input = add_bias x in
            
            let _ = self#propagate x in
            
            let delta_out =
                Array.init (outcount) (fun i ->
                        let g' = out_a.(i) *. (1. -. out_a.(i)) in
                        (y.(i) -. out_a.(i)) *. g'
                    )
            in
            
            let delta_hid =
                Array.init (hidcount) (fun i ->
                    let d = ref 0. in
                    let g' = hid_a.(i + 1) *. (1. -. hid_a.(i + 1)) in
                    for j = 0 to (outcount - 1) do
                        let wij = out_weights.(j).(i + 1) in
                        d := !d +. wij *. delta_out.(j);
                    done;
                    d := !d *. g';
                    !d
                )
            in

            let i = float iter in
            let alpha = a *. (1. +. (c /. a) *. (i /. t)) /.  (1. +. (c /. a) *. (i /. t) +. t *. (t *. t) /. (t *. t)) in
            for i = 0 to Array.length hid_weights - 1 do
                for j = 0 to Array.length hid_weights.(i) - 1 do
                    let delta = alpha *. input.(j) *. delta_hid.(i) +. m
                    *. last_delta_hid.(i).(j) in
                    hid_weights.(i).(j) <- hid_weights.(i).(j) +. delta;
                    last_delta_hid.(i).(j) <- delta
                done
            done;

            for i = 0 to Array.length out_weights - 1 do
                for j = 0 to Array.length out_weights.(i) - 1 do
                    let delta = alpha *. hid_a.(j) *. delta_out.(i) +. m
                    *. last_delta_out.(i).(j) in
                    out_weights.(i).(j) <- out_weights.(i).(j) +. delta;
                    last_delta_out.(i).(j) <- delta
                done
            done
        end

        method error ex =
            let acc = ref 0. in
            for i = 0 to Array.length ex - 1 do
                let o = self#propagate (fst ex.(i)) in
                for j = 0 to Array.length (snd ex.(i)) - 1 do
                    let err = o.(j) -. (snd ex.(i)).(j) in
                    acc := !acc +. err *. err
                done
            done; !acc

        method learn examples n print_error error_print_period =
            for iter = 0 to n - 1 do
                let ex = examples.(Random.int (Array.length examples)) in
                self#backpropagate iter ex;
                if (iter mod error_print_period = 0) && print_error then
                    Printf.printf "Error: %f\n%!" (self#error (examples));
            done

        method save_weights file =
            let oc = open_out file in
            Printf.fprintf oc "%dx%d\n" hidcount (incount + 1);
            Printf.fprintf oc "%dx%d\n" outcount (hidcount + 1);
            for i = 0 to hidcount - 1 do
                for j = 0 to incount do
                    Printf.fprintf oc "%f\n" hid_weights.(i).(j)
                done
            done;
            for i = 0 to outcount - 1 do
                for j = 0 to hidcount do
                    Printf.fprintf oc "%f\n" out_weights.(i).(j)
                done
            done;
            close_out oc

        method load_weights file =
            let ic = open_in file in
            let h1 = (Scanf.fscanf ic "%dx" (fun d -> d)) in
            let h2 = (Scanf.fscanf ic "%d\n" (fun d -> d)) in
            let o1 = (Scanf.fscanf ic "%dx" (fun d -> d)) in
            let o2 = (Scanf.fscanf ic "%d\n" (fun d -> d)) in
            if h1 = hidcount && h2 = incount + 1
            && o1 = outcount && o2 = hidcount + 1 then
            begin
                for i = 0 to h1 - 1 do
                    for j = 0 to h2 - 1 do
                        hid_weights.(i).(j) <- Scanf.fscanf ic "%f\n" (fun d -> d)
                    done
                done;
                for i = 0 to o1 - 1 do
                    for j = 0 to o2 - 1 do
                        out_weights.(i).(j) <- Scanf.fscanf ic "%f\n" (fun d -> d)
                    done
                done;
            end;
            close_in ic
    end

let read_size weights =
    let ic = open_in weights in
    let h1 = (Scanf.fscanf ic "%dx" (fun d -> d)) in
    let h2 = (Scanf.fscanf ic "%d\n" (fun d -> d)) in
    let o1 = (Scanf.fscanf ic "%dx" (fun d -> d)) in
    close_in ic;
    (h2 - 1, h1, o1)
