from collections import defaultdict
from pyconcepticon import Concepticon

conc = Concepticon()

joh = conc.conceptlists['Johansson-2020-344'].concepts
tad = conc.conceptlists['Tadmor-2009-100'].concepts

concepts = defaultdict()

for concept in joh:
    concepts[joh[concept].concepticon_id] = joh[concept].concepticon_gloss

for item in tad:
    if tad[item].concepticon_id not in concepts:
        print(tad[item].concepticon_gloss)
