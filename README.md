
# A-T-Statistics
[![DOI](https://zenodo.org/badge/313591727.svg)](https://doi.org/10.5281/zenodo.4587146)
## 1. Data files
There are three data files:  
>**Main dataframe:**  "Eardata.csv"    
**Phylogeny:**  "JZ tree Prum merged hackett.tree"  
**Dive depth data:**  "Depth list.csv"  

The column names of the main dataframe are described here: 
>**"columnnames.csv"**

## 2. Setting up the analyses
The script to import data and set things up in the right format for analyses is:
> Set up data.R

This script loads the dataframe with auditory measurements, the phylogeny, and the dive depth dataset. The last section of the script sets up the data for aquatic-only analyses. The script calls other script files:

>"SW_HW_.R"    (This computes the regression to interpolate head mass for a few species of unknown head mass)  
>"load phylogeny and make CDO.R"  
>"add_dive_depth_data.R"  


## 3. Phylogenetic principal components (pPCA) analses
These scripts run the pPCAs and plots the pPCA results.  
> "Run phyPCA.R"   (calls "Extract pgls residuals.R" to get the residuals used for input for the pPCA)    
"Plot PCA.R"  

## 4. Phylogenetic general least squares regression (PGLS) analyses

This runs the pgls functions for ear measures and computes the AICc.
> run PGLS and AIC.R

The script calls separate PGLS scripts, one for each model being compared:  
>"pgls_HM.R" (Head mass the only predictor)  
"pgls_HM_plus_ecol.R" (Head mass + ecology as predictors)                             
"pgls_HM_times_ecol.R"  (Head mass x ecology as predictors)  
"pgls_HM_plus_divescore.R" (Head mass + dive score as predictors) (aquatic-only analysis)
"pgls_HM_times_divescore.R" (Head mass x dive score as predictors) (aquatic-only analysis)

## 5. Bayesian ordinal phylogenetic regression
This runs the analysis comparing. There is a separate script for the interaural canal (IAC) and the interbullar passage (IBP).
> "brms IAC models.R"  
"brms IBP models.R"

## 6. Intra-class correlation coefficient for measurement repeatability
This script computes the correlation coefficient for measurement repeatability
> "**All ICC.csv**" - data file containing repeated measurements  
> "ICC_.R" - runs and plots the ICC 


## 7. Other plotting scripts
> "plot ecology on circular phylogeny.R" - plot ecological groupings on circular phylogeny (Fig 2), and sampling by phylogeny (Fig S2)  
"Residualphylogplots_a panel top.R - plot the residual plot (Fig 3)


