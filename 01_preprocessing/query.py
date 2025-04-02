"""
Pre-Process Lexibank data for inclusion in sound symbolism replication study.
"""
import csv
import itertools
import sqlite3
from geopy.distance import geodesic
from tqdm import tqdm
import numpy as np

CLTS = """ATTACH DATABASE "data/clts.sqlite3" AS clts;"""
JSS = """ATTACH DATABASE "data/johanssonsoundsymbolic.sqlite3" AS jss;"""


def path_length(clf1, clf2):
    """Compute distance between two values."""
    inter = clf1[1].intersection(clf2[1])
    path = 0 if not inter else round(0.5 / (1 + (len(clf1[1].union(clf2[1])) - len(inter))), 6)
    return path


def write_value_to_matrix(matrix, distance, i, j):
    """Writes the distance value of two langs to a matrix"""
    matrix[i, j] = distance
    matrix[j, i] = distance

    return matrix


def write_matrix(path, matrix, languages):
    """Writes a matrix to file."""
    with open(path, 'w', newline='', encoding='utf8') as file:
        w = csv.writer(file)

        w.writerow([''] + list(languages))
        for idx, l in enumerate(languages):
            row = [l] + [f"{matrix[idx, j]}" for j in range(len(languages))]
            w.writerow(row)


# Load query
with open('query.sql', encoding='utf8') as f:
    query = f.read()

# Write output
cursor = sqlite3.connect('data/lexibank.sqlite3').cursor()
queried_data = cursor.execute(CLTS).execute(JSS).execute(query).fetchall()
header = [
    'wd_id', 'unicode', 'word', 'language', 'macroarea', 'family', 'latitude', 'longitude',
    'concept', 'name', 'nPhonemesPerWord', 'roundedness', 'backness', 'height', 'extreme',
    'voicing', 'manner', 'position'
    ]

with open('data.csv', 'w', encoding='utf8', newline='') as f:
    writer = csv.writer(f, delimiter=',')
    writer.writerow(header)
    writer.writerows(queried_data)

#############################
# Retrieve unique glottocodes
# Add <if row[5] == 'Pano-Tacanan'>  if testing for a specific language family
langs = set(row[3] for row in queried_data)
lang_to_idx = {l: idx for idx, l in enumerate(langs)}

gl = sqlite3.connect('data/glottolog.sqlite3')

#############################
PHYLO = """
select cldf_languageReference as lid, cldf_value as clf from ValueTable where cldf_parameterReference = 'classification'
"""

clfs = {item[0]: item[1] for item in gl.execute(PHYLO)}
trees = [(r, set(clfs[r].split('/'))) if r in clfs else (r, set()) for r in langs]
phylo_matrix = np.ones((len(trees), len(trees)))

#############################
for l1, l2 in tqdm(itertools.combinations(trees, 2)):
    dist = path_length(l1, l2)
    phylo_matrix = write_value_to_matrix(phylo_matrix, dist, lang_to_idx[l1[0]], lang_to_idx[l2[0]])

#############################
GEO = """select cldf_id as lid, cldf_latitude as lat, cldf_longitude as long from LanguageTable"""

coords = [(r[0], r[1], r[2]) for r in gl.execute(GEO) if r[0] in langs]
geo_matrix = np.ones((len(coords), len(coords)))

for l1, l2 in tqdm(itertools.combinations(coords, 2)):
    dist = geodesic((l1[1], l1[2]), (l2[1], l2[2])).km
    dist = 1 if l1 == l2 else 0.99 if dist <= 1 else 0 if dist > 1000 else round((1/dist), 4)
    geo_matrix = write_value_to_matrix(geo_matrix, dist, lang_to_idx[l1[0]], lang_to_idx[l2[0]])

# Write matrices to file
write_matrix('vcv_phylo.csv', phylo_matrix, langs)
write_matrix('vcv_geo.csv', geo_matrix, langs)
