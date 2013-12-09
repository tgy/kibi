while true; do [ -a data/input_image.jpg] && kibiocr -i data/input_image.jpg -o data/ && rm data/input_image.jpg; sleep 1; done;
