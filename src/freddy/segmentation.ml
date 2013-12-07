module O = OcsfmlGraphics
(*let input = "images/-1"
let output = "output/output.png"*)
let treshold_size_line = 3
let treshold_nb_pix = 2
let treshold_bounding_box = 0.2	

let bounded_boxes (xa,xb,ya,yb) (x1,x2,y1,y2) =
  let sa = (xa - xb) * (yb - ya)
  and s1 = (x2 - x1) * (y2 - y1) in
  let dx = ref 0 and dy = ref 0 in
  if x2 < xa || xb < x1 then
    false
  else 	if y2 < ya || yb < y1 then
    true
  else	begin
    if x1 < xa then begin	
      dx := (min x2 xb) - xa;
      dy := (min y2 yb) - ya;
    end else begin
      dx := (min xb x2) - x1;
      dy := (min yb y2) - y1;
    end;
    let ds = !dx * !dy in
    float_of_int (min sa s1) *.	treshold_bounding_box <  float_of_int ds
    end

let boxes_fusion (xa,xb,ya,yb) (x1,x2,y1,y2) =
  (min xa x1, max xb x2,
  min ya y1, max yb y2)

let is_in_range w h (x,y) =
  x >= 0 && y >= 0 && x < w && y < h

  (* return valid neighbors from a pixel in a image *)
let get_neighbors (x,y) img =
  let (w,h) = img#get_size
  and l = ref [] in 
  let add (x,y) l =
    if is_in_range w h (x,y) && img#get_pixel x y = O.Color.black then
      l := (x,y)::!l;
  in
  let x1 = x + 1 and y1 = y + 1
  and x_1 = x - 1 and y_1 = y - 1 in 
  add (x_1,y_1) l; add (x_1,y) l;
  add (x_1,y1) l; add (x,y_1) l;
  add (x,y1) l; add (x1,y_1) l;
  add (x1,y) l; add (x1,y1) l;
  !l


  (* Get the global bounds of a list of pixels *)
let get_bounds = function
  | [] 				-> raise (Failure "BOUYAH")
  | (x,y)::l	->
      let rec aux = function
        | (a,b,c,d),[]				-> (a,b,c,d)
        | (a,b,c,d),(x,y)::l	->
            aux (( (min a x), (max b x),
            (min c y), (max d y)), l)
  in
  aux ((x,x,y,y),l)

  (* Get list of alls boxes in a specified range *)
let get_boxes img minY maxY =
  let (w,_) = img#get_size in
  let tmp_img = new O.image(`Copy(img)) in
  let rec aux l l2 = function
    | []				-> ()
    | (x,y)::tl ->
        l := (x,y)::!l;
        l2 := (x,y)::!l2;
        aux l l2 tl
  in
  let boxes = ref [] in
  for y = minY to maxY do
    for x = 0 to w - 1 do
      if tmp_img#get_pixel x y = O.Color.black then
        begin
          let l = ref [(x,y)]
          and l2 = ref [(x,y)] in
          while !l <> [] do
            match !l with
            | []				-> ()
            | (x,y)::tl	->
                tmp_img#set_pixel x y O.Color.white;
                l := tl;
                let neighbors = get_neighbors (x,y) tmp_img in
                aux l l2 neighbors
          done;
          let (minX,maxX,minY,maxY) = get_bounds !l2 in
          boxes := (minX,maxX,minY,maxY)::!boxes
  end
          done	
    done;
    !boxes


    (* Fusionne les boxes incluses *)
let fusion =
  let rec lines = function
    | []		-> []
    |	e::l	-> (aux e)::(lines l)
  and innerfusion b = function
    | []													-> [b]
    | e::l when bounded_boxes e b -> innerfusion (boxes_fusion b e) l
    |	e::l												-> e::(innerfusion b l)
  and aux = function
    | []		-> []
    | e::l	->
        match (innerfusion e l) with
        | []		->	[]
        | e::l	-> e::(aux l)
          in
          function boxes -> lines boxes

let printf s =
  Printf.printf "%f : %s\n%!" (Sys.time()) s

let makeHistoH input =
  let (w,h) = input#get_size in
  let histo = Array.make h 0 in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      if (input#get_pixel x y).O.Color.r = 0 then
        histo.(y) <- histo.(y) + 1
  done;
    done;
    histo

let printHLine input y minX maxX c =
  for x = minX to maxX do
    input#set_pixel x y c
  done

let findLines histo =
  let l = ref []
  and s = ref (-1) in
  for y = 0 to Array.length histo - 1 do
    if histo.(y) >= treshold_nb_pix then begin
      if !s = -1 then
        s := y
        end else begin
          if !s != -1 && y - !s >= treshold_size_line then
            l := (!s,y - 1)::!l;
            s := -1
    end		
  done;
  !l


(*let main () = 
  printf "Starting";
  let input = new O.image (`File input) in
  let (w,h) = input#get_size in
  printf "Image Loaded";
  let histo = makeHistoH input in
  printf "Vertical Histograme made";
  let lines = findLines histo in
  printf "Lines found";
  let rec aux = function 
    | []				-> []
    | (a,b)::l	->
        (get_boxes input a b)::aux l
  in
  let boxes = aux lines in
  printf "Boxes found";
  (* Fusion des boxes *)
  let boxes = fusion boxes in
  printf "Boxes merged";
  (* Affichage des lignes *)		
  let rec aux = function
    | []      	-> ()
    | (a,b)::l	->
        printHLine input a 0 (w - 1) O.Color.red;
        printHLine input b 0 (w - 1) O.Color.red;						
        aux l	
  in
  aux lines;
  (* Affichage des boxes des caractères *)
  let rec aux c = function 
    | []								-> ()
    | (mx,mX,my,mY)::l	->
        for i = mx to mX do
          input#set_pixel i my c;
          input#set_pixel i mY c;
  done;
  for i = my to mY do
    input#set_pixel mx i c;
    input#set_pixel mX i c;
        done;
        aux c l
      and aux2 = function
        | []	-> ()
        | e::l	->
            aux O.Color.blue  e;
            aux2 l
  in
  aux2 boxes;

  input#save_to_file(output)

  let _ = main ()*)


  (*##########   RLSA    ##########*)


let makeinputarray ocsfmlimg =
  let (w,h) = ocsfmlimg#get_size in
  let inputarray = Array.make_matrix w h false in
  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
      if (ocsfmlimg#get_pixel x y).O.Color.r <> 255 then (*if not white*)
        inputarray.(x).(y) <- true;
  done;
    done;
    inputarray

let makevertarray imgarray hc =
  let (w,h) = Array.length imgarray, Array.length imgarray.(0) in
  let cw = ref 0 in
  let vertarray = Array.make_matrix w h false in
  for x = 0 to (w - 1) do
    cw := 0;
    for y = 0 to (h - 1) do
      if imgarray.(x).(y) then (*if not white*)
        begin
          vertarray.(x).(y) <- true;
          if (!cw < hc) then
            for yp =  (y - !cw) to (y - 1) do
              vertarray.(x).(yp) <- true
  done;
  cw := 0
        end
      else
        cw := !cw + 1;
            done;
    done;
    vertarray

let makehoriarray imgarray wc =
  let (w,h) = Array.length imgarray, Array.length imgarray.(0) in
  let cw = ref 0 in
  let horiarray = Array.make_matrix w h false in
  for y = 0 to (h - 1) do
    cw := 0;
    for x = 0 to (w - 1) do
      if imgarray.(x).(y) then (*if not white*)
        begin
          horiarray.(x).(y) <- true;
          if (!cw < wc) then
            for xp =  (x - !cw) to (x - 1) do
              horiarray.(xp).(y) <- true
  done;
  cw := 0
        end
          else
            cw := !cw + 1;
            done;
    done;
    horiarray

let fctarray vert hori fct = 
  let (w,h) = Array.length vert, Array.length hori.(0) in
  let newarray = Array.make_matrix w h false in
  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
      newarray.(x).(y) <- fct vert.(x).(y) hori.(x).(y)
  done;
    done;
    newarray

let getocsfmlimgfromarray imgarray = 
  let (w,h) = Array.length imgarray, Array.length imgarray.(0) in
  let output = new O.image (`Color (O.Color.white, w, h)) in
  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
      if imgarray.(x).(y) then
        output#set_pixel x y O.Color.black
  done;
    done;
    output


let startRLSA ocsfmlimg fct wc hc =
  let imgarray = makeinputarray ocsfmlimg in 
  let vertarray = makevertarray imgarray hc in
  let horiarray = makehoriarray imgarray wc in
  let newarray = fctarray vertarray horiarray fct in
  getocsfmlimgfromarray newarray
