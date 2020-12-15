library(ICC)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

#import data set
df<-read.csv(file.choose())
#ID column = specimen
#Code column = replications
#all other columns = measurements (raw values)
df<-df[4:nrow(df),]

#make list of measurements for which the ICC stat is desired
todo<-names(df)[c(4:6,8,11,15:20)]

#apply it across the list of measurements for which ICC is desired
#combine into single dataframe
byspecies<-split(df,df$Species)
ICCrd<- do.call(rbind.data.frame, lapply(todo,function(i){
  ICC<-ICCest(ID,i,data = byspecies$`Rock dove`)#chek dataset
  ICC$measure<-i
  ICC$species<-"Rock dove"
  return(ICC)
}))

ICCSAL<- do.call(rbind.data.frame, lapply(todo,function(i){
  ICC<-ICCest(ID,i,data = byspecies$`Salvin's prion`)#chek dataset
  ICC$measure<-i
  ICC$species<-"Salvin's prion"
  return(ICC)
}))

ICCboth<-rbind(ICCrd,ICCSAL)

#write.csv(ICCboth,"E:/Analysis_plots/ICCmay8.csv")

################plot ICCs
all<-ggplot(data = ICCboth, aes(x = reorder(measure,-ICC), y = ICC, shape = species)) +
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1), col = "grey") +
  geom_pointrange(aes(ymin = LowerCI, ymax = UpperCI), fill = "white", position=position_dodge(.3))+
  scale_shape_manual(values = c(21,24))+
  xlab("")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
all
