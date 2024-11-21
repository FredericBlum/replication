# Preparing the data

```shell
git clone https://github.com/cldf-clts/clts
cd clts
git checkout v2.3.0
cd ../
cldf createdb clts/cldf-metadata.json clts.sqlite

git clone https://github.com/lexibank/johanssonsoundsymbolic
cldf createdb johanssonsoundsymbolic/cldf/cldf-metadata.json johanssonsoundsymbolic.sqlite
```
