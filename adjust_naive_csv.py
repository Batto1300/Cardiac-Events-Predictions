import csv
import re


ORIGINAL_FILE = 'datasets/CardiacDataOriginal.csv'
TEMP_FILE = 'datasets/CardiacDataAdj.csv'
YES = 'Yes'
NO = 'No'

reader = csv.reader(open(ORIGINAL_FILE,'r'))
writer = csv.writer(open(TEMP_FILE,'w'))
header= next(reader)
writer.writerow(header)
row = next(reader)
while row:
    new_row = []
    for value in row:
        if value == '0':
            new_value = YES
        elif value == '1':
            new_value = NO
        else:
            new_value = re.sub(',', '.', value)
        new_row.append(new_value)
    writer.writerow(new_row)
    try:
        row = next(reader)
    except StopIteration:
        row = None
