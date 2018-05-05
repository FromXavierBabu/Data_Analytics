import json
from pprint import pprint

def read_json_data(file):
	with open(file) as data_file:    
		data1 = json.load(data_file)
		#pprint(data)
	pprint(data1["data"][2][1])
	return data1

# example to call read_json_data() function
#read_json_data("Food_Inspection_data.json")
