from collections import defaultdict
from pyconcepticon import Concepticon

conc = Concepticon()

joh = conc.conceptlists['Johansson-2020-344'].concepts
check = conc.conceptlists['Tadmor-2009-100'].concepts
check = conc.conceptlists['Holman-2008-40'].concepts
check = conc.conceptlists['Swadesh-1955-100'].concepts

concepts = defaultdict()

for concept in joh:
    concepts[joh[concept].concepticon_id] = joh[concept].concepticon_gloss

for item in check:
    if check[item].concepticon_id not in concepts:
        print(check[item].concepticon_gloss)

# tadmor: FLESH OR MEAT
# ARM OR HAND --> no
# DO OR MAKE --> no
# STONE OR ROCK --> no
# STRIKE OR BEAT --> no
# FOOT OR LEG --> no
# CHILD (DESCENDANT) --> no
# BURNING --> no
# OLD --> no
# WOOD --> no
# SALT --> no
# GRIND --> no

# Holman:
# WE --> yes

# Swadesh
# CLAW --> no
# KNOW (SOMETHING) --> no
# LIE (REST) --> no
# FLESH OR MEAT --> no
# RAINING OR RAIN --> no
# ROAD --> no
# WALK --> no
# HOT OR WARM --> no
# WE --> yes
