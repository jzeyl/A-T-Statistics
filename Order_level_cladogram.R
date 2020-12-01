library(ape)
library(phytools)
library(ggtree)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)

#count # in each order
orderdf<-avgdf %>% count(Order)


#g<-avgdf %>% group_by(Order)

#distinct df
g2<-avgdf[!duplicated(avgdf$Order),]#orders only

#subest remove terrestrial
#orderdf<-avgdf[avgdf$Category!="Terrestrial",] %>% count(Order)
#g2<-avgdf[avgdf$Category!="Terrestrial",]
#g2<-g2[!duplicated(g2$Order),]#orders only


g2order<-arrange(g2,Order)#sort to match order df
g2order$n<-orderdf$n#attach number of species in order
str(g2order$Binomial)
#drop timps for order

orderPhy<-keep.tip(birdtreels,g2order$Binomial)
#orderPhy$tip.label<-droplevels(orderPhy$tip.label, except = g2order$Binomial)

g2order$full<-paste0(g2order$Order," (",as.character(g2order$n),")")
g2order$Order<-as.character(g2order$Order)

orderPhy$tip.label<-as.character(orderPhy$tip.label)#######have as character!!!
str(g2order$full)

#order circular
ppiped <- ggtree(orderPhy, layout = "circular", 
                 branch.length = "none") %<+% g2order + 
  geom_tiplab(aes(label=full, angle = angle), offset = 1) + 
  xlim(NA,30) 
ppiped


#order rectangle
ppiped <- ggtree(orderPhy,  branch.length = "none") %<+% g2order + 
  geom_tiplab(aes(label=full), offset = 1) + 
  xlim(NA,30) +
  #geom_text(aes(label = node))+
  geom_cladelabel(51, "Paleognathae", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(50, "Galloanserae(8)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(22, "Apodiformes (1)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(48, "Columbaves (8)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(18, "Gruiformes (5)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(41, "Aequorlitornithes(59)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(9, "Accipitriformes(7)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(35, "Australaves(30)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(8, "Strigiformes (2)", offset=7, barsize=2, align = T, angle=0,offset.text=0)+#, fontsize=3
  geom_cladelabel(38, "Coraciimorphae (5)", offset=7, barsize=2, align = T, angle=0,offset.text=0)#, fontsize=3
ppiped

#change tip labels to orders
#put current correct binomials on the tree
new<-as.data.frame(cbind(orderPhy$tip.label,g2order$Order,g2order$Binomial))
colnames(new)<-c("tiplabel","Order","binomial")
#View(new)
#match(new$tiplabel,new$birdtree)#get ordered with correct binomials
new$binomialordered<-new$binomial[match(new$tiplabel,new$binomial)]
new$Orderordered<-new$Order[match(new$tiplabel,new$binomial)]
#orderPhy$tip.label<-as.character(orderPhy$tip.label)
new$binomialordered<-as.character(new$binomialordered)
new$Orderordered<-as.character(new$Orderordered)
str(orderPhy$tip.label)
str(new$binomialordered)
orderPhy$tip.label<-new$Orderordered
plot(orderPhy)