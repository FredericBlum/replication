# Preparing the data

```shell
git clone https://github.com/cldf-clts/clts
git checkout @v2.3.0
cd clts
cldf createdb cldf-metadata.json clts.sqlite

git clone https://github.com/lexibank/johanssonsoundsymbolic
cd johanssonsoundsymbolic/
cldf createdb cldf/cldf-metadata.json johanssonsoundsymbolic.sqlite
cd ../
```
