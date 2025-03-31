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


BASE = "../../cldf_resources/concepticon-data/concepticondata/conceptlists/"
# Use local manipulation of Johansson to account for replaced '_' and brackets
JOHANSSON = "Johansson-2020-344.tsv"
TAD_100 = BASE + "Tadmor-2009-100.tsv"
SWAD_100 = BASE + "Swadesh-1955-100.tsv"
ASJP_40 = BASE + "Holman-2008-40.tsv"

# for item in original_effects:
#     print(item, original_effects[item])

# Get Concepticon mappings of the Johansson concept list
change = defaultdict()
symbolic_concepts = []
with UnicodeDictReader(JOHANSSON, delimiter='\t') as reader:
    for line in reader:
        change[line['LEXIBANK_GLOSS']] = line["CONCEPTICON_GLOSS"]
        if line['SOUND_SYMBOLIC'] == str(1):
            symbolic_concepts.append(line["CONCEPTICON_GLOSS"])

orig_results = []
missing_concepts = []
# Read in original results
with open('original_results.csv', mode='r', encoding="utf8") as file:
    data = csv.reader(file)
    next(data)
    for i, line in enumerate(data):
        mod = change[line[0].replace('_', ' ').replace('fs', '(fs)').replace('ms', '(ms)')]
        if mod != '':
            line[0] = mod
            orig_results.append([
                line[0],  # Concept
                line[1]   # Feature
                ])

        # Check the unmapped concepts
        elif line[0] not in missing_concepts:
            missing_concepts.append(line[0])

# Check which concepts had not been added to Concepticon
# only 'maybe' and kinship-terms
# for item in missing_concepts:
#     print(item)

# Write mapped original results to file for R plots
with open('../analysis/original_results_mapped.csv', 'w', encoding='utf8', newline='') as f:
    writer = csv.writer(f, delimiter='\t')
    writer.writerows(orig_results)

# Read in the vocabulary lists
lists = defaultdict()
lists = {name: read_cl(file) for name, file in [
    ('Swadesh-100', SWAD_100),
    ('Tadmor-100', TAD_100),
    ('Holman-40', ASJP_40)
    ]}

results = []
table = []

with open('../analysis/data/final_results.csv', mode='r', encoding="utf8") as file:
    data = csv.reader(file)
    headers = next(data)
    for line in data:
        results.append([
            line[0],  # Concept
            line[1],  # Feature
            line[7],  # New/Original
            line[9]   # Strong/Weak
            ])

rep_score = defaultdict()
# Cycling through all basic vocabulary lists that interest us
for bsc_vcb in lists:
    original_effects = defaultdict()
    replicated_effects = defaultdict()
    all_results = []
    # Establish baseline for quantiyfying the replication
    # Dictionary with structure key:CONCEPT value: [features]
    # e.g. STONE ['nasal', 'nasal+voice']
    for item in results:
        # Check if concept is in basic vocabulary list
        if item[0] in lists[bsc_vcb] and item[2] == 'Original Results' and item[0] in symbolic_concepts:
            # Add concept if not yet in dictionary
            if item[0] not in original_effects:
                # print(item[0])
                original_effects[item[0]] = []
            original_effects[item[0]].append(item[1])  # Feature

    # Go through new results
    for entry in results:
        if entry[0] in lists[bsc_vcb] and entry[2] == 'New Results':
            # Get number of concepts for which an effect has been found
            if entry[0] not in all_results:
                all_results.append(entry[0])
            # Check if pattern had been observed previously, for concept and feature
            if entry[0] in original_effects and entry[1] in original_effects[entry[0]]:
                if entry[0] not in replicated_effects:
                    replicated_effects[entry[0]] = []
                replicated_effects[entry[0]].append(entry[1])
    rep_score[bsc_vcb] = len(replicated_effects) / len(lists[bsc_vcb])

    print('------------')
    print('Results for', bsc_vcb)
    print('Original:', len(original_effects))
    print('Replicated:', len(replicated_effects))

    for item in replicated_effects:
        print(item, replicated_effects[item])

    table.append([
        bsc_vcb,
        len(all_results),
        len(all_results)/len(lists[bsc_vcb]),
        len(original_effects),
        len(replicated_effects),
        len(replicated_effects) / len(original_effects)
        ])

header = ['List', 'Weak (n)', 'Weak (%)', 'Original (n)', 'Replicated (n)', 'Replicated (%)']
print(tabulate(
    table,
    headers=header,
    floatfmt='.3f',
    tablefmt='latex'
    ))
