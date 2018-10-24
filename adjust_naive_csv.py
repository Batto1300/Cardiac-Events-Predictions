import csv
import re


ORIGINAL_FILE = 'datasets/CardiacDataOriginal.csv'
TEMP_FILE = 'datasets/CardiacDataAdj.csv'
USELESS_COLUMNS = [33,34,36,37,38,39,40,41]
YES = 'Yes'
NO = 'No'

reader = csv.reader(open(ORIGINAL_FILE,'r'))
writer = csv.writer(open(TEMP_FILE,'w'))
old_header = next(reader)
new_header= [x for x in old_header
    if old_header.index(x) not in USELESS_COLUMNS]
writer.writerow(new_header)
row = next(reader)
while row:
    new_row = []
    for i,value in enumerate(row):
        if i in USELESS_COLUMNS:
            continue
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
