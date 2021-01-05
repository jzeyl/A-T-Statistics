library(phytools)
library(ggalt)
library(ggrepel)

#get residuals from pgls of head mass vs each ear measure
source("Extract pgls residuals.R")

#this creates a new dataframe with residuals attached in the last columns:
names(dfwithresids)

#isolate columns with residuals to be used in PCA
PCAset<-dfwithresids[,37:48]# data to be used in PCA
row.names(PCAset)<-dfwithresids$Binomial#species names need to be in row names for use of phy.PCA function

names(PCAset)

#remove air volume from PCA if desired (air volume has more missing values than other measures)
#PCAset<-PCAset[,-c(10)]

#remove NAs
naomit_PCA<-na.omit(PCAset)

#trim phylogeny to the species used in the PCA
phyPCA<-keep.tip(birdtreels,row.names(naomit_PCA))
plot(phyPCA)

################run phyPCA####################
pPCA<- phyl.pca(phyPCA,naomit_PCA, 
                method = "lambda", 
                mode = "corr")

#plot PCA#################
#par(mar = c(4,3,3,3))
#plot(pPCA)
#biplot(pPCA)

#put PCA values and loadings in dataframes
speciesPCAvalues<-as.data.frame(pPCA$S)#PCA score, with each
pPCAloadings<-as.data.frame(pPCA$L)
pPCAloadings

#Create column in the PCA dataset for 
#species name, order, dive score, and ecological grouping (plunge-distinct)
speciesPCAvalues$Binomial<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                        dfwithresids$Binomial),"Binomial"])#<-------------<-------
speciesPCAvalues$Order<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"Order"])#<-------------<-------
speciesPCAvalues$divescore<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"divescore"])#<-------------<-------
speciesPCAvalues$plungedistinct<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"plungedistinct"])#<-------------<-------


