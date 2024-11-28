import csv
from csvw.dsv import UnicodeDictReader
from collections import defaultdict
from tabulate import tabulate


def read_cl(path):
    concept_dict = defaultdict()
    with UnicodeDictReader(path, delimiter='\t') as reader:
        for line in reader:
            concept_dict[line["CONCEPTICON_GLOSS"]] = line['CONCEPTICON_ID']

    return concept_dict


BASE = "../cldf_resources/concepticon-data/concepticondata/conceptlists/"
JOHANSSON = BASE + "Johansson-2020-344.tsv"
TAD_100 = BASE + "Tadmor-2009-100.tsv"
SWAD_200 = BASE + "Swadesh-1952-200.tsv"
ASJP_40 = BASE + "Holman-2008-40.tsv"

original = defaultdict()
with UnicodeDictReader(JOHANSSON, delimiter='\t') as reader:
    for line in reader:
        original[line["CONCEPTICON_GLOSS"]] = line['ENGLISH']

tadmor = read_cl(TAD_100)
swadesh = read_cl(SWAD_200)
holman = read_cl(ASJP_40)

lists = defaultdict()

lists['Swadesh-200'] = swadesh
lists['Tadmor-100'] = tadmor
lists['Holman-40'] = holman

results = []
with open('data/final_results.csv', mode='r', encoding="utf8") as file:
    data = csv.reader(file)
    headers = next(data)

    for line in data:
        results.append([line[0], line[1], line[8], line[11]])

table = []
header = ['List', 'Strong (n)', 'Strong (%)', 'Weak (n)', 'Weak (%)']

for bsc_vcb in lists:
    strong = []
    all = []
    for entry in results:
        if entry[2] == 'New Results' and original[entry[0]] in lists[bsc_vcb]:
            if entry[0] not in all:
                all.append(entry[0])
            if entry[3] == 'Strong' and entry[0] not in strong:
                strong.append(entry[0])

            print(bsc_vcb, entry[0], entry[1], entry[2], entry[3])

    print(bsc_vcb, strong)
    sym_strong = len(strong)
    sym_all = len(all)

    table.append([bsc_vcb, sym_strong, sym_strong/len(lists[bsc_vcb]), sym_all, sym_all/len(lists[bsc_vcb])])


print(tabulate(
    table,
    headers=header,
    floatfmt='.2f',
    tablefmt='latex'
    ))
