# Contents:

"Results" folder has the full BN model's dot file and pdf, and also the snapshot used in the paper.

"Scripts" folder has the code

# What the script does

The R script is used for :

1. Constructing the Bayesian Network model from combined Chrome and Qt data.
1. Getting the precision and kappa values from 10 fold cross-validation.
1. Obtaining the conditional dependency tables for different variables (using gRain package). 

There are also pieces of code for obtaining the coefficients for the different links in the BN model,
and also for comparing the scores between an empty graph and the BN model to estimate the goodness-of-fit.

# Instructions on How to run the R script to generate the Bayesian Network plots for the Inspections data

## Dependencies in the R script (with version):

1. bnlearn_4.1.1
1. arules_1.5-2
1. e1071_1.6-8
1. caret_6.0-76
1. gRain_1.3-0

## Instructions for running the script

1. Load the script in R (or Rstudio, which was used for development).
1. Change the file path arguments to point to the folder containing the data files in Line 21 (for Chrome) and line 25 (for Qt).
1. Run up to line 57 to generate the bootstrap plot.
1. Adjust the threshold parameter in line 60 based on the plot, typically a high value between 0.8 and 0.9 is recommended.
1. Plot the BN model (line 63).
1. Run additional Cross-Validation / Inference / Goodness-of-fit test compared to an empty graph, as required. Pieces are highlighted in the code.

