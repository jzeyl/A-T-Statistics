library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyr)

#set working directory and load data
setwd("E:/ATclone/A_T-stats")

#load main dataframe
df<-read.csv("data Nov 17_.csv", stringsAsFactors = F, header = T) #, stringsAsFactors = FALSE

#####The pgls model function, which will be applied to list of formulas###
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.0001,1)))
}

#note some missing headmass values now imputed
df$Head.mass..g.

#load phylogeny and correct names that were different between birdtree.org and the up-to-date species names
source("load phylogeny and make CDO.R")

#add computed head mass from head mass~skullwidth pgls
source("SW_HM_.R")#add phylogeney here
df$Head.mass..g.

#Since PGLS uses one point per species,I make the dataframe to have average values for species with more than one specimen:
#First make a dataframe with only one species per line
distinctdf<-distinct(df, Binomial, .keep_all = TRUE)
distinctdforder<-arrange(distinctdf,Binomial)#sort by species name

#Then get averages for columns with continuous data
avgdf<-df %>% group_by(Binomial) %>% summarise_at(vars(Skull.width..mm.:area_ratio),
                                                           mean, na.rm = TRUE)                         
avgdf<-as.data.frame(avgdf)

#And add back columns from distinct df which don't require averaging
avgdf$Species<-distinctdforder$Species
avgdf$Low.Hz<-distinctdforder$Low.Hz
avgdf$Order<-distinctdforder$Order
avgdf$Family<-distinctdforder$Family
avgdf$maxdivedepth<-distinctdforder$max
avgdf$Category<-as.character(distinctdforder$Category)
avgdf$birdtree<-gsub(" ","_",distinctdforder$Birdtree)
avgdf$IAC_detail<-distinctdforder$IAC_detail
avgdf$IBP_detail<-distinctdforder$IBP_detail
avgdf$Behind.TM<-distinctdforder$Behind.TM
avgdf$`fluid.filled.`<-distinctdforder$`fluid.filled.`

#add dive depth dataframe here. This script groups the 'surface foraging' based on more detailed ecologies
source("add_dive_depth_data.R")

#'plungedistin
#set 'terrestrial' as reference level 
avgdf$plungedistinct<-as.character(avgdf$catfeeding2)
avgdf$plungedistinct[which(is.na(avgdf$plungedistinct))]<-"Terrestrial"
avgdf$plungedistinct<-relevel(as.factor(avgdf$plungedistinct), ref = "Terrestrial")
avgdf$plungedistinct[avgdf$Binomial=="Ardea_melanocephala"]<-"Surface"

#made data frame object
birdCDO<-comparative.data(phy = birdtreels,data = avgdf,#[avgdf$Category!="Terrestrial",]
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

#create list of pgls models to run (only models with head mass are used)
pgls_todo_nogeomet <- c("log(TMtotalarea)~log(Skull.width..mm.)",
  "log(TMtotalarea)~log(Head.mass..g.)",# 
  
  "log(FPtotalarea)~log(Skull.width..mm.)",
  "log(FPtotalarea)~log(Head.mass..g.)",#    
  
  "log(area_ratio)~log(Skull.width..mm.)",
  "log(area_ratio)~log(Head.mass..g.)",
  
  "log(dis_coltip_TMcentroid)~log(Skull.width..mm.)",
  "log(dis_coltip_TMcentroid)~log(Head.mass..g.)",
  
  "log(Umbo_distancetoTMplane)~log(Skull.width..mm.)",
  "log(Umbo_distancetoTMplane)~log(Head.mass..g.)",
  
  "log(meanTMangle)~log(Skull.width..mm.)",
  "log(meanTMangle)~log(Head.mass..g.)",
  
  "log(totalEClength)~log(Skull.width..mm.)",
  "log(totalEClength)~log(Head.mass..g.)",
  
  "log(RWtotalarea)~log(Skull.width..mm.)",
  "log(RWtotalarea)~log(Head.mass..g.)", 
  
  "log(CAtotalarea)~log(Skull.width..mm.)",
  "log(CAtotalarea)~log(Head.mass..g.)",
  
  "log(Behind.TM)~log(Skull.width..mm.)",
  "log(Behind.TM)~log(Head.mass..g.)",#   
  
  "log(Columella.length.mm)~log(Skull.width..mm.)",
  "log(Columella.length.mm)~log(Head.mass..g.)",  
  
  "log(Columella.volume.mm3)~log(Skull.width..mm.)",
  "log(Columella.volume.mm3)~log(Head.mass..g.)")

#select models with head mass
pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]


################### Data object for aquatic-only analyses -----------------------------
birdCDO<-comparative.data(phy = birdtreels,data = avgdf[avgdf$Category!="Terrestrial",],
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tips dropped between linking phylogeny and dataframe
birdCDO$dropped

#Remove 'terrestrial' as a level
birdCDO$data$plungedistinct<-droplevels(birdCDO$data$plungedistinct, exclude = "Terrestrial")
levels(birdCDO$data$plungedistinct)
birdCDO$data$plungedistinct<-relevel(birdCDO$data$plungedistinct, ref = "Surface")

