# Replicating sound symbolism

This repository is the current status of a replication study of Johansson et al. (2020, [link](https://doi.org/10.1515/lingty-2020-2034)). So far, only the original study is reproduced with new cut-off criteria and a Gaussian Process term that controls for spatial biases. The replication on Lexibank data will be submittd as a Pre-Registered report.

## Requirements

In order to use `cmdstanr` as backend for brms/Stan, you need to have a running installation of `cmdstan` on your computer. You can follow the install recommendations [here](https://github.com/stan-dev/cmdstanr?tab=readme-ov-file#installation).

## Downloading the data

```shell
pip install pycldf
pip install pyconcepticon
```

```shell
cd 01_preprocessing
git clone https://github.com/cldf-clts/clts data/clts --branch v2.3.0
git clone https://github.com/lexibank/lexibank-analysed.git data/lexibank-analysed
git clone https://github.com/lexibank/johanssonsoundsymbolic data/johanssonsoundsymbolic
git clone https://github.com/glottolog/glottolog-cldf data/glottolog
```

We have to add the missing concepts to the database from the original study.

```shell
python check_conceptlists.py
cat missing_concepts.csv >> data/johanssonsoundsymbolic/cldf/parameters.csv
```

```shell
rm data/*.sqlite3
cldf createdb data/clts/cldf-metadata.json data/clts.sqlite3
cldf createdb data/lexibank-analysed/cldf/wordlist-metadata.json data/lexibank.sqlite3
cldf createdb data/johanssonsoundsymbolic/cldf/cldf-metadata.json data/johanssonsoundsymbolic.sqlite3
cldf createdb data/glottolog/cldf/cldf-metadata.json data/glottolog.sqlite3
```

Now you can create the `data.csv` file for further processing: `python query.py`.

## File overview

- `01_formatting.R` is the preprocessing script from the original authors. Not really modified from my side and will not be relevant for the replication
- `02_priors.R` - The distributions for my priors.
- `02a_priorModel.R` - The model that samples only from the prior distributions. Outputs are log-odds ratios from -1.5 to 1.5
- `03_models.R` - Running the model for all 10 phonetic categories.
- `04_plots.R` - Loading the individual results and comparing them to the original results.

The models and posterior predictive simulations can be accessed on OSF: <https://osf.io/m76eb/>

Please extract the two folders (`data_derived/` and `models/`) and put them into the main folder in order to have the directory-setup working.

## New Thresholds

### Evaluation thresholds

The original authors use the logaritm with base 2 for computing the log odds ratio, and use `log2(1.2)` as a threshold that gets added and substracted to 1, in order to prepare the evaluation thresholds. To follow more standard practices in statistics and machine learning, we use the natural logarithm instead. The authors claimed in the paper to use the factor of 1.25 as factor for the threshold, while in the published script they used a factor of 1.2. It is thus unclear which one was indeed used, but I suspect that it was 1.2, since this is what is published in the script.

- Lower threshold: `log(1/1.25)`
- Upper threshold: `log(1*1.25)`

### Computing Proportions

Mirroring the approach from the original authors, I use dirichlet models for the individual phonetic features. This involves computing the proportions of features per word. In many cases, those will be 0. However, due to the set-up of the script, an `NA` term is produced. In order to correctly represent this, those `NA` get changed to `0.001` (Dirichlect requires a response above 0). This is exactly the same procedure as in the original study, and is justified by design.

## References

Erben Johansson, N., Anikin, A., Carling, G., & Holmer, A. (2020). The typology of sound symbolism: Defining macro-concepts via their semantic and phonetic features. Linguistic Typology, 24(2), 253–310. https://doi.org/10.1515/lingty-2020-2034
