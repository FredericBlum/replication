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
SWAD_100 = BASE + "Swadesh-1955-100.tsv"
ASJP_40 = BASE + "Holman-2008-40.tsv"

original = defaultdict()
change = defaultdict()
with UnicodeDictReader(JOHANSSON, delimiter='\t') as reader:
    for line in reader:
        original[line["CONCEPTICON_GLOSS"]] = line['ENGLISH']
        change[line['LEXIBANK_GLOSS']] = line["CONCEPTICON_GLOSS"]

tadmor = read_cl(TAD_100)
swadesh = read_cl(SWAD_100)
holman = read_cl(ASJP_40)

lists = defaultdict()

lists['Swadesh-100'] = swadesh
lists['Tadmor-100'] = tadmor
lists['Holman-40'] = holman

results = []
with open('data/final_results.csv', mode='r', encoding="utf8") as file:
    data = csv.reader(file)
    headers = next(data)

    for line in data:
        results.append([
            line[0],  # Concept
            line[1],  # Feature
            line[7],  # New/original
            line[9]   # Strong/Weak
            ])

table = []
header = ['List',  # 'Strong (n)', 'Strong (%)',
          'Weak (n)', 'Weak (%)', 'Replicated']

rep_score = defaultdict()
# Cycling through all basic vocabulary l ists that interest us
for bsc_vcb in lists:
    print('------------')
    print('Results for', bsc_vcb)
    strong = []
    all_results = []
    rep = defaultdict()
    unique_hit = []
    hit = 0

    # Establish baseline for quantiyfying the replication
    for item in results:
        if item[0] in lists[bsc_vcb] and item[2] == 'Original Results':
            if item[0] not in rep:
                rep[item[0]] = []
            rep[item[0]].append(item[1])  # Feature

    # Go through new results
    for entry in results:
        if entry[2] == 'New Results' and entry[0] in lists[bsc_vcb]:
            if entry[0] not in all_results:
                all_results.append(entry[0])
            if entry[3] == 'Strong':
                strong.append(entry[0])

            # Check if pattern had been observed previously
            if entry[0] in rep:
                if entry[1] in rep[entry[0]]:
                    print(entry)
                    if entry[0] not in unique_hit:
                        unique_hit.append(entry[0])
                        hit += 1

            # print(bsc_vcb, entry[0], entry[1], entry[2], entry[3])
    rep_score[bsc_vcb] = hit / len(lists[bsc_vcb])
    print(len(rep))
    table.append([
        bsc_vcb,
        # No strong results, so I can leave that out
        # len(strong),
        # len(strong)/len(lists[bsc_vcb]),
        len(all_results),
        len(all_results)/len(lists[bsc_vcb]),
        hit / len(rep)
        ])

print(tabulate(
    table,
    headers=header,
    floatfmt='.2f',
    tablefmt='latex'
    ))
