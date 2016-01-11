import csv
import collections


def convert(str):
    try:
        val = float(str)
    except ValueError:
        val = str
    return val

#Open scrapy database and create dictionary of airports 
#using airport id(IATA) as  key 
ifile  = open('IATA_cities.csv', "rb")
reader = csv.reader(ifile)
headers = reader.next()
State = headers.index("State")
City = headers.index("City")
IATA = headers.index("IATA")
dicti = {}
dict_air = {}
for row in reader :
	B = []
	B.append(row[State])
	B.append(row[City])
	dicti[row[IATA]] = B
dicti = collections.OrderedDict(sorted(dicti.items()))
#Show all airports
for k, v in dicti.items():
           print k, v
ifile.close()

#Open Database and output Database
ifile  = open('2001.csv', "rb")
reader = csv.reader(ifile)
ofile  = open('filtered.csv', "w")
writer = csv.writer(ofile)

colnum = 0
rownum = 0
airport = 0
add = 0
#Dictionary of airports not found in the dictionary
dict_fail = {}
for row in reader:
    row_out = []
    colnum = 0
    add = 0
	#For each row filter some data
    for col in row :
        if colnum != 8 and colnum != 9 and  colnum != 19 and colnum != 20 and colnum != 22 and colnum != 24 and colnum != 25 and colnum != 26 and colnum != 27 and colnum != 28 :
            if colnum == 18 and rownum != 0:
              #print col
              col = int(convert(col) * convert(1.609344))
              #print col
            row_out.append(col)
        colnum +=1
    # Add aditional data headers and data  
    if rownum == 0:
        row_out.append('STATE FROM')
        row_out.append('CODE FROM')
        row_out.append('STATE TO')
        row_out.append('CODE TO')
    else :
        try : 
            row_out.append(dicti[row[16]][0])
            row_out.append(dicti[row[16]][1])               
        except:
            add = 1
            if row[16] in dict_fail.keys():
                dict_fail[row[16]] += 1
            else :
                dict_fail[row[16]] = 1
        try:
            row_out.append(dicti[row[17]][0])
            row_out.append(dicti[row[17]][1])
        except:
            add = 1
    if add == 0 :
        writer.writerow(row_out)
    rownum = 1
ifile.close()
ofile.close()
print "Done filtering"
#Show airports that weren't addded to the output Database
print "Erased:"
dict_fail = collections.OrderedDict(sorted(dict_fail.items()))
for key, value  in dict_fail.items():
    print key, value