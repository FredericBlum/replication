"""
Pre-Process Lexibank data for inclusion in sound symbolism replication study.
"""
import csv
import sqlite3

# SQLite queries
CLTS = """ATTACH DATABASE "clts.sqlite" AS clts;"""
JSS = """ATTACH DATABASE "johanssonsoundsymbolic.sqlite" AS jss;"""
QUERY = 'query.sql'

# Load Lexibank
db = sqlite3.connect('lexibank2.sqlite3')
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
table = cursor.fetchall()
header = [
    'wd_id', 'unicode', 'word', 'glottocode', 'macroarea', 'family', 'latitude', 'longitude',
    'concept', 'name', 'nPhonemesPerWord', 'roundedness', 'backness', 'height', 'extreme',
    'voicing', 'manner', 'position'
    ]


with open('data.csv', 'w', encoding='utf8', newline='') as f:
    writer = csv.writer(f, delimiter=',')
    writer.writerow(header)
    writer.writerows(table)
