import csv
import re
import os


ORIGINAL_FILE = 'datasets/CardiacDataOriginal.csv'
TEMP_FILE = 'datasets/CardiacDataAdj.csv'

reader = csv.reader(open(ORIGINAL_FILE,'r'))
writer = csv.writer(open(TEMP_FILE,'w'))
header= next(reader)
writer.writerow(header)
row = next(reader)
while row:
    new_row = []
    for value in row:
        new_row.append(re.sub(',', '.', value))
    writer.writerow(new_row)
    try:
        row = next(reader)
    except StopIteration:
        row = None
