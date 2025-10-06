# Replicating sound symbolism

This repository is the replication study of Johansson et al. (2020, [link](https://doi.org/10.1515/lingty-2020-2034)). The original study built a model to investigate the statistical over-representation of certain phonetic features in concepts of basic vocabulary.

## Requirements

### Python packages

We recommend to install the Python packages in a fresh virtual environment. For example, you can follow this code to create one:

```shell
python3 -m venv venv/replication
source venv/replication/bin/activate
```

You can now install the packages with pip. We have used Python 3.13 for the code. Older versions might not work for the provided package versions.

```shell
pip install -r requirements.txt
```

For Windows, please follow this tutorial: <https://doi.org/10.15475/calcip.2024.2.6>

### R packages

Some code is also in R. We have used R version v4.5.0. You can install all the necessary packages through `renv`. Just run the code that you can find in `02_analysis/packages.R`:

```R
install.packages('renv')
library(renv)
renv::restore()
```

This should install all the required packages with working versions.

In order to use `cmdstanr` as backend for brms/Stan, you need to have a running installation of `cmdstan` on your computer. You can follow the install recommendations [here](https://github.com/stan-dev/cmdstanr?tab=readme-ov-file#installation).

## Downloading the data

```shell
cd 01_preprocessing
git clone https://github.com/cldf-clts/clts data/clts --branch v2.3.0
git clone https://github.com/lexibank/lexibank-analysed.git data/lexibank-analysed --branch v2.1
git clone https://github.com/lexibank/johanssonsoundsymbolic data/johanssonsoundsymbolic --branch v1.3
git clone https://github.com/glottolog/glottolog-cldf data/glottolog --branch v5.2
git clone https://github.com/concepticon/concepticon-data data/concepticon --branch v3.4.0
```

We have to add the missing concepts to the database from the original study.

```shell
python check_conceptlists.py
cat data/missing_concepts.csv >> data/johanssonsoundsymbolic/cldf/parameters.csv
```

You can now create the SQLite3 databases for all the data necessary to run the query and the models.

```shell
rm -f data/*.sqlite3
cldf createdb data/clts/cldf-metadata.json data/clts.sqlite3
cldf createdb data/lexibank-analysed/cldf/wordlist-metadata.json data/lexibank.sqlite3
cldf createdb data/johanssonsoundsymbolic/cldf/cldf-metadata.json data/johanssonsoundsymbolic.sqlite3
cldf createdb data/glottolog/cldf/cldf-metadata.json data/glottolog.sqlite3
```

Now you can create the `data.csv` file for further processing by retrieving the data from Lexibank:

```shell
python query.py
```

## Running the analysis

We can now switch to the `02_analysis` folder.

```shell
cd ../02_analysis
```

### Running the shell script

```shell
sbatch --exclude=dlcenode[01-16] --export=PARAM=voicing runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=roundedness runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=height runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=backness runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=extreme runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=position runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=manner runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=manner_voicing runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=extreme_roundedness runs.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=position_voicing runs.sh
```

```shell
sbatch --exclude=dlcenode[01-16] --export=PARAM=voicing model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=roundedness model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=height model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=backness model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=extreme model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=position model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=manner model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=manner_voicing model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=extreme_roundedness model_comp.sh
sbatch --exclude=dlcenode[01-16] --export=PARAM=position_voicing model_comp.sh
```

### Downloading model files

Since the models and posterior simulations result in very large files, you can download them from OSF: <https://osf.io/4kg52/?view_only=a96702c55db14528b9a3e7ed3701588b>

Due to the large file size that exceeds what is allowed from OSF, the files are split. Please run the following command in the shell to re-create the model files for further processing in R.

```shell
for f in *_split_aa; do 
  base="${f%_split_aa}"; 
  cat "${base}_split_"* > "${base}.tar.gz" && \
  tar -xvf "${base}.tar.gz" && \
  rm "${base}.tar.gz" "${base}_split_"*; 
done
```

The models have been split using the following shell command:

```shell
for f in *.rds; do 
  base="${f%.rds}"; 
  tar -czf "${base}.tar.gz" "$f" && \
  split -b 500M "${base}.tar.gz" "${base}_split_" && \
  rm "${base}.tar.gz" "$f" && \
done
```

### 02_analysis files overview

The R code was run using 

- `packages.R` - installs the necessary R packages
- `01_priors.R` - The distributions for my priors.
- `02_preprocess.R` - This script returns the data files for the individual phonetic categories. Creates
- `03_models.R` - Running the model for all 10 phonetic categories.
- `04_plots.R` - Loading the individual results and comparing them to the original results.

You can run the code either line by line, or using Rscript from the command line.

```shell
Rscript packages.R
Rscript 01_priors.R
Rscript 02_preprocess.R
Rscript 03_models.R
Rscript 04_plots.R
```

Please note that the scripts `02_` and `03_` are written to work with only one of the ten phonological variables at a time. To run the processing and models for all of them, you need to manually change the variable `myvar` to one of the following values: `backness`, `extreme_roundedness`, `extreme`, `height`, `manner_voicing`, `manner`, `position_voicing`, `position`, `roundedness`, `voicing`.

Please extract the two folders (`posterior_draws/` and `models/`) and put them into the folder 02_analysis in order to have the directory-setup working.

### Evaluation thresholds

The original authors use the logaritm with base 2 for computing the log odds ratio, and use `log2(1.2)` as a threshold that gets added and substracted to 1, in order to prepare the evaluation thresholds. To follow more standard practices in statistics and machine learning, we use the natural logarithm instead. The authors claimed in the paper to use the factor of 1.25 as factor for the threshold, while in the published script they used a factor of 1.2. It is thus unclear which one was indeed used, but I suspect that it was 1.2, since this is what is published in the script. The following are the new thresholds used in our paper:

- Lower threshold: `log(1/1.25)`
- Upper threshold: `log(1*1.25)`

### Computing Proportions

Mirroring the approach from the original authors, I use dirichlet models for the individual phonetic features. This involves computing the proportions of features per word. In many cases, those will be 0. However, due to the set-up of the script, an `NA` term is produced. In order to correctly represent this, those `NA` get changed to `0.001` (Dirichlect requires a response above 0). This is exactly the same procedure as in the original study, and is justified by design.

## Evaluating the results

The final folder, `03_evaluation`, represents a numeric comparison between the old and the new results. You can run `intersection.py` to receive an overview of all the effects found for concepts in the basic vocabulary lists.

## References

Erben Johansson, N., Anikin, A., Carling, G., & Holmer, A. (2020). The typology of sound symbolism: Defining macro-concepts via their semantic and phonetic features. Linguistic Typology, 24(2), 253–310. https://doi.org/10.1515/lingty-2020-2034
