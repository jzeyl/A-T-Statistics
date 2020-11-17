library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(stringr)

#get models that only like head mass and the variable
pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#make comparative data object for caper
birdCDO<-comparative.data(phy = birdtreels,data = avgdf, #avgdf[avgdf$Category!="Terrestrial",]
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

#get formulas list that regresses each ear measure with skull width only

#function to extract head size-correcteded PGLS residuals for each PGLS formula
pgls_resid_outputs<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, lambda = 'ML', 
                bounds = list(lambda = c(0.00000000000000000000001,1)))
  resids<-residuals(pglsfit)
}

#apply the residuals function to the list of formulas
all_pgls_resids<-lapply(pgls_todo_hm,pgls_resid_outputs)# returns a list of dataframes

#attach the residuals to the original data frame, matching by species name
avgdf$V1<-all_pgls_resids[[1]][match(avgdf$Binomial,row.names(all_pgls_resids[[1]]))]# attach
avgdf$V2<-all_pgls_resids[[2]][match(avgdf$Binomial,row.names(all_pgls_resids[[2]]))]
avgdf$V3<-all_pgls_resids[[3]][match(avgdf$Binomial,row.names(all_pgls_resids[[3]]))]
avgdf$V4<-all_pgls_resids[[4]][match(avgdf$Binomial,row.names(all_pgls_resids[[4]]))]
avgdf$V5<-all_pgls_resids[[5]][match(avgdf$Binomial,row.names(all_pgls_resids[[5]]))]
avgdf$V6<-all_pgls_resids[[6]][match(avgdf$Binomial,row.names(all_pgls_resids[[6]]))]
avgdf$V7<-all_pgls_resids[[7]][match(avgdf$Binomial,row.names(all_pgls_resids[[7]]))]
avgdf$V8<-all_pgls_resids[[8]][match(avgdf$Binomial,row.names(all_pgls_resids[[8]]))]
avgdf$V9<-all_pgls_resids[[9]][match(avgdf$Binomial,row.names(all_pgls_resids[[9]]))]
avgdf$V10<-all_pgls_resids[[10]][match(avgdf$Binomial,row.names(all_pgls_resids[[10]]))]
avgdf$V11<-all_pgls_resids[[11]][match(avgdf$Binomial,row.names(all_pgls_resids[[11]]))]
avgdf$V12<-all_pgls_resids[[12]][match(avgdf$Binomial,row.names(all_pgls_resids[[12]]))]

#rename the newly added residual columns to be more informative
oldnames = c("V1" , "V2" , "V3",  "V4" , "V5",  "V6",  "V7" , "V8",  "V9" , "V10" ,"V11", "V12")# check numbers of newly added columns to rename<-------------------------<----<-----
newnames = paste("RES_",str_replace_all(pgls_todo_hm,"[^[:alnum:]]",""), sep = "")

#create new dataframe with residuals
dfwithresids<-avgdf %>% rename_at(vars(oldnames), ~ newnames)#dplyr function
names(dfwithresids)
