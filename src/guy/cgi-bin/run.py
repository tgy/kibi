import os

input_image = "data/input_image.png"

output_dir = "data/"

os.system('kibiocr -i ' + input_image + ' -o ' + output_dir)

#with open('filename.txt', mode='wt', encoding='utf-8') as myfile:
#    myfile.write('ta gueule')
