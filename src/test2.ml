let main () =
	let text = [[["/nadgees;SerenOnnc];^il!meDi]ree:-7X:"]]] in
	Robert.detect text;
	let a = Robert.correct text in
	print_string (Robert.tojson a)
	
let _ = main ();
