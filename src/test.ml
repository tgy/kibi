let main () =
	let output = "output.json" in
	let line = ["bongour";"voir";"salope"]
	and line2 = ["fleurissons";"vairre";"cheine"] in
	let para = [line;line2] in
	let whole = [para] in 
  Robert.detect whole;
	let l = Robert.correct whole in
	Printf.printf "%s\n%!" (Robert.tojson l);
	Robert.savejson output l

let _ = main ()
