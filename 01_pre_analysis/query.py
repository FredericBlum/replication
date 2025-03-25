"""
Pre-Process Lexibank data for inclusion in sound symbolism replication study.
"""
import csv
import sqlite3

# SQLite queries
CLTS = """ATTACH DATABASE "data/clts.sqlite3" AS clts;"""
JSS = """ATTACH DATABASE "data/johanssonsoundsymbolic.sqlite3" AS jss;"""
QUERY = 'query.sql'

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
table = cursor.fetchall()
header = [
    'wd_id', 'unicode', 'word', 'language', 'macroarea', 'family', 'latitude', 'longitude',
    'concept', 'name', 'nPhonemesPerWord', 'roundedness', 'backness', 'height', 'extreme',
    'voicing', 'manner', 'position'
    ]


with open('../02_analysis/data/data.csv', 'w', encoding='utf8', newline='') as f:
    writer = csv.writer(f, delimiter=',')
    writer.writerow(header)
    writer.writerows(table)
