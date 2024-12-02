import csv
from csvw.dsv import UnicodeDictReader
from collections import defaultdict
from tabulate import tabulate


def read_cl(path):
    concept_dict = defaultdict()
    with UnicodeDictReader(path, delimiter='\t') as f:
        for lin in f:
            concept_dict[lin["CONCEPTICON_GLOSS"]] = lin['CONCEPTICON_ID']

    return concept_dict


BASE = "../cldf_resources/concepticon-data/concepticondata/conceptlists/"
JOHANSSON = BASE + "Johansson-2020-344.tsv"
TAD_100 = BASE + "Tadmor-2009-100.tsv"
SWAD_200 = BASE + "Swadesh-1952-200.tsv"
ASJP_40 = BASE + "Holman-2008-40.tsv"

original = defaultdict()
change = defaultdict()
with UnicodeDictReader(JOHANSSON, delimiter='\t') as reader:
    for line in reader:
        original[line["CONCEPTICON_GLOSS"]] = line['ENGLISH']
        change[line['LEXIBANK_GLOSS']] = line["CONCEPTICON_GLOSS"]

tadmor = read_cl(TAD_100)
swadesh = read_cl(SWAD_200)
holman = read_cl(ASJP_40)

lists = defaultdict()

lists['Swadesh-200'] = swadesh
lists['Tadmor-100'] = tadmor
lists['Holman-40'] = holman

orig_results = []
with open('original_results.csv', mode='r', encoding="utf8") as file:
    data = csv.reader(file)
    for i, line in enumerate(data):
        mod = change[line[0].replace('_', ' ').replace('fs', '(fs)').replace('ms', '(ms)')]
        if i == 0:
            orig_results.append(line)
        elif mod != '':
            line[0] = mod
            orig_results.append(line)

with open('original_results_mapped.csv', 'w', encoding='utf8', newline='') as f:
    writer = csv.writer(f, delimiter='\t')
    writer.writerows(orig_results)

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
    all_results = []
    for entry in results:
        if entry[2] == 'New Results' and original[entry[0]] in lists[bsc_vcb]:
            if entry[0] not in all_results:
                all_results.append(entry[0])
            if entry[3] == 'Strong' and entry[0] not in strong:
                strong.append(entry[0])

            print(bsc_vcb, entry[0], entry[1], entry[2], entry[3])

    print(bsc_vcb, strong)

    table.append([
        bsc_vcb,
        len(strong),
        len(strong)/len(lists[bsc_vcb]),
        len(all_results),
        len(all_results)/len(lists[bsc_vcb])
        ])

print(tabulate(
    table,
    headers=header,
    floatfmt='.2f',
    tablefmt='latex'
    ))
