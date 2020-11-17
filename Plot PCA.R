library(ggrepel)
library(ggalt)
library(RColorBrewer)
library(viridis)

#SCREE PLOT
d<-as.data.frame(diag(pPCA$Eval)/sum(pPCA$Eval)*100)
d$PC1<-row.names(d)
d$percentexplained<-d$`diag(pPCA$Eval)/sum(pPCA$Eval) * 100`

p<-ggplot(d, aes(x = reorder(PC1,-percentexplained), y = percentexplained))+
  geom_bar(stat = "identity")+
  theme_classic()+
  xlab("Principal component")+
  ylab("Percent variance \n explained")
p

#Make a grouping of orders that includes species with one underwater-pursuit species
speciesPCAvalues$Order<-as.character(speciesPCAvalues$Order)
speciesPCAvalues$Order2<-NA
speciesPCAvalues$Order2[
  which(speciesPCAvalues$Order=="Procellariiformes"|
          speciesPCAvalues$Order=="Sphenisciformes"|
          speciesPCAvalues$Order=="Charadriiformes"|
          speciesPCAvalues$Order=="Anseriformes"|
          #speciesPCAvalues$Order=="Phaethontiformes"|
          speciesPCAvalues$Order=="Suliformes"
  )]<-speciesPCAvalues$Order[
    which(speciesPCAvalues$Order=="Procellariiformes"|
            speciesPCAvalues$Order=="Sphenisciformes"|
            speciesPCAvalues$Order=="Charadriiformes"|
            speciesPCAvalues$Order=="Anseriformes"|
            #speciesPCAvalues$Order=="Phaethontiformes"|
            speciesPCAvalues$Order=="Suliformes"
    )]

#Create columns in the PCA dataset for different groupings
speciesPCAvalues$Binomial<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                        dfwithresids$Binomial),"Binomial"])#<-------------<-------
speciesPCAvalues$Order<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"Order"])#<-------------<-------
speciesPCAvalues$divescore<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"divescore"])#<-------------<-------
speciesPCAvalues$plungedistinct<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"plungedistinct"])#<-------------<-------
speciesPCAvalues$plungedistinct<-relevel(as.factor(speciesPCAvalues$plungedistinct),"Terrestrial")
speciesPCAvalues$divescore<-as.numeric(as.character(speciesPCAvalues$divescore))
speciesPCAvalues$IAC<-avgdf$IAC_detail[match(row.names(speciesPCAvalues),avgdf$Binomial)]
speciesPCAvalues$IBP<-avgdf$IBP_detail[match(row.names(speciesPCAvalues),avgdf$Binomial)]

#Categories to label species of particular interest
speciesPCAvalues$Binomial0<-ifelse(grepl("Phalacrocorax", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),"")
speciesPCAvalues$Binomial2<-ifelse(grepl("Tyto", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),"")
speciesPCAvalues$Binomial2<-ifelse(grepl("Bubo", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),speciesPCAvalues$Binomial2)

speciesPCAvalues$Binomial3<-ifelse(grepl("Somateria", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),"")

speciesPCAvalues$Binomial4<-ifelse(grepl("Burhinus", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),"")
speciesPCAvalues$Binomial4<-ifelse(grepl("Vanellus", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),speciesPCAvalues$Binomial4)

speciesPCAvalues$Binomial4p5<-ifelse(grepl("Stercorarius_antarcticus", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),"")
speciesPCAvalues$Binomial4p5<-ifelse(grepl("Larus_dominicanus", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),speciesPCAvalues$Binomial4p5)

speciesPCAvalues$Binomial5<-ifelse(grepl("Alca", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),"")
speciesPCAvalues$Binomial5<-ifelse(grepl("Fratercula", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),speciesPCAvalues$Binomial5)
speciesPCAvalues$Binomial5<-ifelse(grepl("Cepphus", speciesPCAvalues$Binomial),as.character(speciesPCAvalues$Binomial),speciesPCAvalues$Binomial5)

speciesPCAvalues$Order3<-ifelse(grepl("Anseriformes", speciesPCAvalues$Order),as.character(speciesPCAvalues$Order),"")

speciesPCAvalues$divescore<-as.factor(speciesPCAvalues$divescore)

######################plot PC1 vs PC2 coloured by different factors
###############BIPLOT BASIC FUNCTION####################
runPCAplot<-function(group, p1,p2,n1,n2){
scattercat1<-ggplot(speciesPCAvalues, aes_string(x = p1, y = p2, label = "Binomial")) +
  theme_classic()+
  xlab(label = paste0(as.character(p1),"(",
                      as.character(signif(d$percentexplained[n1], digits = 3)),
                      "%)"))+
  ylab(label = paste0(as.character(p2),"(",
                      as.character(signif(d$percentexplained[n2], digits = 3)),
                      "%)"))+
  #theme(legend.position = "none")+
geom_encircle(aes_string(fill = group),s_shape=1, expand=0, color = "black", alpha = 0.2)+#s_shape = 1 and expan = 0 are convex hull
  geom_point(aes_string(color = group), shape = 21, size = 3, color = "black")
  #scale_fill_manual(values = alpha(c("black","black","black","black","black","black",0.2))
scattercat1
}

#set up color palette
mypal <- colorRampPalette(brewer.pal(6, "Blues"))

#plot by orders
order<-runPCAplot("Order2","PC1","PC2",1,2) +
  geom_point(aes(fill = Order2), size = 3, shape = 21, col = "black")+
  scale_fill_brewer(palette = "Set1", na.value = "green")+
  scale_color_brewer(palette = "Set1", na.value = "green")+
  theme(legend.position = "none")+
  #theme(legend.position = "bottom")+
  #ylim(c(-50,50))+
  #ylim(c(-70,70))+
  #geom_text_repel(aes(label = Binomial3))+
  #geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Anseriformes",],  aes(fill = Order3, col = "blue"),col = "blue",s_shape=1, expand=0, alpha = 0.3)+#s_shape = 1 and expan = 0 are convex hull

  #geom_point(data = speciesPCAvalues[speciesPCAvalues$Binomial4p5!="",], size = 8, shape = 23, color = "black", fill = "grey")+
  #geom_point(data = speciesPCAvalues[speciesPCAvalues$Binomial4!="",], size = 8, shape = 23, color = "black", fill = "green")+

  #geom_point(data = speciesPCAvalues[speciesPCAvalues$Binomial5!="",], size = 8, shape = 23, color = "black", fill = "blue")
  
  #geom_text_repel(aes(label = Binomial5))
  #geom_text_repel(aes(label = Binomial3), vjust = 2, nudge_y = 50-speciesPCAvalues$PC1)+
  #geom_text_repel(aes(label = Binomial4),vjust = 2, nudge_x = 70-speciesPCAvalues$PC1)+
  #geom_text_repel(aes(label = Binomial5),vjust = 2, nudge_x = -60+speciesPCAvalues$PC1)
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Charadriiformes",], aes_string(),s_shape=1, expand=0, alpha = 0.2)
order    +  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Charadriiformes",],
                      aes_string(),s_shape=1, expand=0, alpha = 1)

ORDER<-order    +  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Charadriiformes",], 
                          aes_string(),s_shape=1, expand=0, alpha = 1)

#geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Anseriformes",], 
#                aes_string(),s_shape=1, expand=0, alpha = 1)

#plot by aquatic groupings
plunge<-runPCAplot("plungedistinct","PC1","PC2",1,2)+
  geom_point(aes(fill = plungedistinct), size = 3, shape = 21, col = "black")+
  scale_color_manual(values=c("green","black","darkgrey","blue"))+
  geom_point(data = speciesPCAvalues[speciesPCAvalues$Binomial0!="",], size = 8, shape = 23, color = "black", fill = "blue")+
  geom_point(data = speciesPCAvalues[speciesPCAvalues$Binomial2!="",], size = 8, shape = 23, color = "black", fill = "green")+
  scale_fill_manual(values=alpha(c("green","black","darkgrey","blue"), 0.7))+
  theme(legend.position = "none")
plunge

#plot by dive score
mypal <- colorRampPalette(rev(brewer.pal(6, "Blues")))
divescore<-runPCAplot("divescore","PC1","PC2",1,2)+
  geom_point(aes(fill = divescore), size = 3, shape = 21,col = "black")+
  scale_fill_manual(values = rev(mypal(5)), na.value = "green")+
  scale_color_manual(values = rev(mypal(5)), na.value = "green")
  #theme(legend.position = "none")+
  geom_point(aes(shape = IBP))
divescore

########plot LOADINGS
pPCAloadings$factor<-row.names(pPCAloadings)
pPCAloadings$factor<-gsub("RES_log"," ",pPCAloadings$factor)
pPCAloadings$factor<-gsub("logHeadmassg","",pPCAloadings$factor)


library(ggrepel)
loadings1<-ggplot(pPCAloadings, aes(x = PC1, y = PC2, label =factor)) +
  #geom_point(aes(), size = 0.005) +
  #geom_point(data = pPCAloadings)+
  geom_text_repel(aes()) +
  theme_classic()+
  xlim(-1.5,+1)+
  ylim(-1,1)+
  geom_segment(aes(x = 0, y = 0,xend = PC1, yend = PC2), arrow = arrow(type = "closed"))+
  theme(legend.position = "bottom")
#geom_encircle(aes(colour = Category, fill = Category),s_shape=1, expand=0)#s_shape = 1 and expan = 0 are convex hull
loadings1

loads<-loadings1+ annotation_custom(ggplotGrob(p), xmin = 0.4, xmax = 1, 
                  ymin = 0.2, ymax = 1)

ggarrange(loadings1,ORDER,plunge,divescore)

ggarrange(loadings1,p,
          order,cat,
          labels = c("A","B","C","D","E","F"),nrow = 2, ncol = 2)

ggsave("D:/00_Manuscripts/0Avian aquatic hearing project/___Oct 1 version/PCAOct 4_noair.pdf",width = 10, height = 10)
ggsave("D:/00_Manuscripts/0Avian aquatic hearing project/___Oct 1 version/PCAOct 4_withair.pdf",width = 10, height = 10)

ggsave("D:/Analysis_plots/PCAsept4_with.pdf",width = 10, height = 15)







