# A_T-stats

## 1. Data files
There are 3 data files:  
Main dataframe (csv):  "data nov 17.csv"    
Phylogeny (TREE file):  "JZ tree Prum merged hackett.tree"  
Dive depth data (csv):  "Depth list_MC2_PGR_JZ_RP suggestions remove less than 0p5, rem allealle,corm.csv"  

The column names are described here: "columnnames.csv""

## 2. Setting up the data
Main script to set up the data and analyses is:
> Set up data.R

This loads the dataframe with auditory measurements, the phylogeny, and the dive depth dataset. It also sets outs the list of PGLS formula to use, and has to set up the data for aquatic-only species. The script calls other scripts:

"SW_HW_.R"    
"load phylogeny and make CDO.R"  
"add_dive_depth_data.R"  

## 3. phyPCA analses
The first script runs the pPCA, calling the second script to extract the pgls residuals for phyPCA input. Another one
to plot the pPCA results.  
> "Run phyPCA.R"   

"Extract pgls residuals.R"    
"Plot PCA.R"  

## 4. PGLS analyses

This runs the pgls functions for ear ear measure, and computes AIC.
> run PGLS and AIC.R

The script calls separate PGLS scripts, one for each model:  
"pgls_HM.R"                                                                                                               
"pgls_HM_plus_ecol.R"                                                                                                                
"pgls_HM_times_ecol.R"  
"pgls_HM_plus_divescore.R" 
"pgls_HM_times_divescore.R"

## 5. Bayesian ordinal phylogenetic regression
This runs the analysis comparing. Shown here for the interaural canal (IAC) only
> "brms phylogenetic.R"

## 6. Intra-class correlation coefficient for measurement repeatability
"All ICC.csv" - data file  
"ICC_.R" - runs and plots the ICC 

