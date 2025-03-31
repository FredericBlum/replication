"""
Pre-Process Lexibank data for inclusion in sound symbolism replication study.
"""
import csv
import itertools
import sqlite3
import numpy as np
from tqdm import tqdm

# SQLite queries
CLTS = """ATTACH DATABASE "data/clts.sqlite3" AS clts;"""
JSS = """ATTACH DATABASE "data/johanssonsoundsymbolic.sqlite3" AS jss;"""
QUERY = 'query.sql'


def distance(clf1, clf2):
    """Compute distance between two values."""
    inter = clf1[1].intersection(clf2[1])
    if not inter:
        dist = 0
    else:
        dist = round(0.25 / (1 + (len(clf1[1].union(clf2[1])) - len(inter))), 6)
    return dist


# Load Lexibank
db = sqlite3.connect('data/lexibank.sqlite3')
cursor = db.cursor()

# Load query
with open(QUERY, encoding='utf8') as f:
    query = f.read()

# Attach databases
cursor.execute(CLTS)
cursor.execute(JSS)

# Run query
cursor.execute(query)

# Write output
queried_data = cursor.fetchall()
header = [
    'wd_id', 'unicode', 'word', 'language', 'macroarea', 'family', 'latitude', 'longitude',
    'concept', 'name', 'nPhonemesPerWord', 'roundedness', 'backness', 'height', 'extreme',
    'voicing', 'manner', 'position'
    ]


with open('data.csv', 'w', encoding='utf8', newline='') as f:
    writer = csv.writer(f, delimiter=',')
    writer.writerow(header)
    writer.writerows(queried_data)

# Retrieve unique glottocodes
# Add 'if row[5] == 'Pano-Tacanan'  if testing for a specific language family
unique_langs = set(row[3] for row in queried_data)

# SQL query
SQL = """
select cldf_languageReference as lid, cldf_value as clf from ValueTable where cldf_parameterReference = 'classification'
"""

# Run query
conn = sqlite3.connect('data/glottolog.sqlite3')
# Check if glottocode in data
clfs = [(r[0], set(r[1].split('/'))) for r in conn.execute(SQL) if r[0] in unique_langs]

# Indices and base matrix
langs = [lang[0] for lang in clfs]
lang_to_idx = {l[0]: idx for idx, l in enumerate(clfs)}
matrix = np.ones((len(clfs), len(clfs)))

# Build matrix
for i, (l1, l2) in tqdm(enumerate(itertools.combinations(clfs, 2))):
    dist_langs = distance(l1, l2)
    idx1, idx2 = lang_to_idx[l1[0]], lang_to_idx[l2[0]]
    matrix[idx1, idx2] = dist_langs
    matrix[idx2, idx1] = dist_langs

# Write Matrix to file
with open('distances.csv', 'w', newline='', encoding='utf8') as file:
    writer = csv.writer(file)

    writer.writerow([''] + langs)
    for i, lang in enumerate(langs):
        row = [lang] + [f"{matrix[i, j]}" for j in range(len(langs))]
        writer.writerow(row)
