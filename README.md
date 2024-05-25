# Replicating sound symbolism

This repository is the current status of a replication study of Johansson et al. (2020). So far, only the original study is reproduced with new cut-off criteria and a Gaussian Process term that controls for spatial biases. The replication on Lexibank data will be submittd as a Pre-Registered report.

## File overview

- `01_formatting.R` is the preprocessing script from the original authors. Not really modified from my side and will not be relevant for the replication
- `02_priors.R` - The distributions for my priors.
- `02a_priorModel.R` - The model that samples only from the prior distributions. Outputs are log-odds ratios from -1.5 to 1.5
- `03_models.R` - Running the model for all 10 phonetic categories.
- `04_plots.R` - Loading the individual results and comparing them to the original results.

## New Thresholds

### Evaluation thresholds

The original authors use the logaritm with base 2 for computing the log odds ratio, and use `log2(1.2)` as a threshold that gets added and substracted to 1, in order to prepare the evaluation thresholds. I think this is wrong in various ways and use the natural logarithm instead. Also, I do not add/substract the same number (`log2(1.2)`) to the log-output, but add/substract __before__ computing the logarithm. I think this is a more accurate representation of what is going on.

- Lower threshold: `log(1-0.25)`
- Upper threshold: `log(1+0.25)`

### Computing Proportions

Mirroring the approach from the original authors, I use dirichlet models for the individual phonetic features. This involves computing the proportions of features per word. In many cases, those will be 0. However, due to the set-up of the script, an `NA` term is produced. In order to correctly represent this, those `NA` get changed to `0.001` (Dirichlect requires a response above 0). This is exactly the same procedure as in the original study, and is justified by design.

## References

Erben Johansson, N., Anikin, A., Carling, G., & Holmer, A. (2020). The typology of sound symbolism: Defining macro-concepts via their semantic and phonetic features. Linguistic Typology, 24(2), 253–310. https://doi.org/10.1515/lingty-2020-2034
