
# A-T-Statistics
[![DOI](https://zenodo.org/badge/313591727.svg)](https://doi.org/10.5281/zenodo.4587146)
## 1. Data files
There are three data files:  

| Content | File name |
| ------------- | ------------- |
| **Main dataframe**  | "Eardata.csv"    |
| **Phylogeny** | "JZ tree Prum merged hackett.tree"  |
| **Dive depth data**  | "Depth list.csv"    |

The column names of the main dataframe are described in **"columnnames.csv"**

## 2. Setting up the analyses
The **"Set up data.R"**. script loads the dataframes with auditory measurements, the phylogeny, and the dive depth dataset. The last section of the script sets up the data for aquatic-only analyses. The script calls other script files:
| File name | Purpose |
| ------------- | ------------- |
|"SW_HW_.R" |   Computes the regression to interpolate head mass for a few species of unknown head mass|  
|"load phylogeny and make CDO.R" | Loads the phylogeny and make a 'comparative data object'  |
|"add_dive_depth_data.R" | Joins the dive data into the main dataframe|


## 3. Phylogenetic principal components (pPCA) analyses
The **"Run phyPCA.R"** script run the pPCAs and **"Plot PCA.R"** plots the pPCA results.  **"Run phyPCA.R"** calls **"Extract pgls residuals.R"** to get the residuals used as input for the pPCA.  
 

## 4. Phylogenetic general least squares regression (PGLS) analyses

The **"run PGLS and AIC.R"** script runs the pgls functions for ear measures and computes the AICc. This script calls separate PGLS scripts, one for each model being compared:  
| Model | File name |
| ------------- | ------------- |
| **Head mass the only predictor**  | "pgls_HM.R"    |
| **(Head mass + ecology** | "pgls_HM_plus_ecol.R"  |
| **Head mass x ecology**  | "pgls_HM_times_ecol.R"    |
| **Head mass + dive score (aquatic-only analysis)**  | "pgls_HM_plus_divescore.R"    |
| **Head mass x dive score (aquatic-only analysis)** | "pgls_HM_times_divescore.R"  |
| **Head mass x dive score**  | "Depth list.csv"    |

## 5. Bayesian ordinal phylogenetic regression
This runs the analysis involving the connectivity of cranial air cavities. There is a separate script for the interaural canal (IAC) and the interbullar passage (IBP).
> "brms IAC models.R"  
"brms IBP models.R"

## 6. Intra-class correlation coefficient for measurement repeatability
This script computes the correlation coefficient for measurement repeatability
> "**All ICC.csv**" - data file containing repeated measurements  
> "ICC_.R" - runs and plots the ICC 


## 7. Other plotting scripts
> "plot ecology on circular phylogeny.R" - plot ecological groupings on circular phylogeny (Fig 2), and sampling by phylogeny (Fig S2)  
"Residualphylogplots_a panel top.R - plot the residual plot (Fig 3)


