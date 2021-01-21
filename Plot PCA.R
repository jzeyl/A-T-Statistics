library(ggrepel)
library(ggalt)
library(RColorBrewer)
library(viridis)
library(colorspace)
library(forcats)

##############################SCREE PLOT################
d<-as.data.frame(diag(pPCA$Eval)/sum(pPCA$Eval)*100)
d$PC1<-row.names(d)
d$percentexplained<-d$`diag(pPCA$Eval)/sum(pPCA$Eval) * 100`

p<-ggplot(d, aes(x = reorder(PC1,-percentexplained), y = percentexplained))+
  geom_bar(stat = "identity")+
  theme_classic()+
  xlab("Principal component")+
  ylab("Percent variance \n explained")
p

################Make a grouping of orders that includes species with one underwater-pursuit species#####################
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

####################Create columns in the PCA dataset for different groupings#################
speciesPCAvalues$Binomial<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                        dfwithresids$Binomial),"Binomial"])#
speciesPCAvalues$Order<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"Order"])#
speciesPCAvalues$divescore<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"divescore"])#
speciesPCAvalues$plungedistinct<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                     dfwithresids$Binomial),"plungedistinct"])#
speciesPCAvalues$plungedistinct<-relevel(as.factor(speciesPCAvalues$plungedistinct),"Terrestrial")
speciesPCAvalues$divescore<-as.numeric(as.character(speciesPCAvalues$divescore))
speciesPCAvalues$IAC<-avgdf$IAC_detail[match(row.names(speciesPCAvalues),avgdf$Binomial)]
speciesPCAvalues$IBP<-avgdf$IBP_detail[match(row.names(speciesPCAvalues),avgdf$Binomial)]

##########################Categories to label species of particular interest################
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

###color palettes####
qualitative_hcl(5, palette = "Dark 3")

###############BIPLOT BASIC FUNCTION####################
runPCAplot<-function(group, p1,p2,n1,n2){
scattercat1<-ggplot(speciesPCAvalues, aes_string(x = p1, y = p2, label = "Binomial")) +
  theme_classic()+
  xlab(label = paste0("p",as.character(p1),"(",
                      as.character(signif(d$percentexplained[n1], digits = 3)),
                      "%)"))+
  ylab(label = paste0("p",as.character(p2),"(",
                      as.character(signif(d$percentexplained[n2], digits = 3)),
                      "%)"))+
  #theme(legend.position = "none")+
  geom_encircle(data = speciesPCAvalues[which(is.na(speciesPCAvalues$Order2)),],
                aes_string(),s_shape=1, expand=0, alpha = 0, col = "black")+
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Charadriiformes",],
                aes_string(),s_shape=1, expand=0, fill = "#E16A86", alpha = 1, col = "black")+
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Procellariiformes",],
                aes_string(),s_shape=1, expand=0, fill = "#00AA5A", alpha = 1)+
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Suliformes",],
                aes_string(),s_shape=1, expand=0, fill = "#B675E0", alpha = 1)+
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Sphenisciformes",],
                aes_string(),s_shape=1, expand=0, fill = "#AA9000", alpha = 1)+
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Anseriformes",],
                aes_string(),s_shape=1, expand=0, fill = "#00A6CA",alpha = 1)+
  geom_encircle(aes_string(fill = group),s_shape=1, expand=0, color = "black", alpha = 0.5)+#s_shape = 1 and expan = 0 are convex hull
  geom_point(aes_string(color = group), shape = 21, size = 3, color = "black")+
  #scale_color_manual(values = alpha(c("black","black","black","black","black","black",0.2))
  geom_point(aes_string(fill = group), size = 3, shape = 21, col = "black")+
  scale_color_manual(values = c("#00AA5A","#E16A86","#AA9000", "#B675E0", "#00A6CA"), na.value = "white")+
  theme(legend.position = "none")
  
}
runPCAplot("Order2","PC1","PC2",1,2)

#set up color palette
mypal <- colorRampPalette(brewer.pal(6, "Blues"))
qualitative_hcl(5, palette = "Dark 3")


##############plot by orders##################
speciesPCAvalues$Order2 <- fct_explicit_na(as.factor(speciesPCAvalues$Order2))
speciesPCAvalues$Order2 <-relevel(speciesPCAvalues$Order2,ref = "(Missing)")

speciesPCAvalues$Order2 <-factor(speciesPCAvalues$Order2,levels = c("(Missing)"  ,    "Procellariiformes", "Charadriiformes" , 
          "Sphenisciformes"  , "Suliformes", "Anseriformes" ))

order<-runPCAplot("Order2","PC1","PC2",1,2) +
  geom_point(aes(fill = Order2), size = 3, shape = 21, col = "black")+
  scale_fill_manual(values = c("white","#00AA5A","#E16A86","#AA9000", "#B675E0", "#00A6CA"))+
  theme(legend.position = "none")+

  theme(legend.position = "right")+  
  geom_encircle(data = speciesPCAvalues[speciesPCAvalues$Order=="Anseriformes",],
                                                   aes_string(),s_shape=1, expand=0, fill = "#00AA5A",alpha = 0.5)+

  geom_text_repel(aes(label = Binomial5),
                  nudge_y = 30 - speciesPCAvalues$PC2)+
  geom_text_repel(aes(label = Binomial4),
            nudge_y = 30 - speciesPCAvalues$PC2)
order   


# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c(	"#FFFFFF","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#########plot by aquatic groupings################
runPCAplotPlunge<-function(group, p1,p2,n1,n2){
  scattercat1<-ggplot(speciesPCAvalues, aes_string(x = p1, y = p2, label = "Binomial")) +
    theme_classic()+
    xlab(label = paste0("p",as.character(p1),"(",
                        as.character(signif(d$percentexplained[n1], digits = 3)),
                        "%)"))+
    ylab(label = paste0("p",as.character(p2),"(",
                        as.character(signif(d$percentexplained[n2], digits = 3)),
                        "%)"))+
    #theme(legend.position = "none")+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$plungedistinct =="Plunging",],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$plungedistinct =="Terrestrial",],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$plungedistinct =="Surface",],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$plungedistinct =="Underwater pursuit",],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(aes_string(fill = group),s_shape=1, expand=0, color = "black", alpha = 0.7)+#s_shape = 1 and expan = 0 are convex hull
    geom_point(aes_string(color = group), shape = 21, size = 3, color = "black")
  #scale_color_manual(values = alpha(c("black","black","black","black","black","black",0.2))
  scattercat1
}
runPCAplotPlunge("plungedistinct","PC1","PC2",1,2)

#plot by aquatic groupings
plunge<-runPCAplotPlunge("plungedistinct","PC1","PC2",1,2)+
  geom_point(aes(fill = plungedistinct), size = 3, shape = 21, col = "black")+
  scale_fill_manual(values = cbbPalette)+
  theme(legend.position = "right")+
  geom_text(aes(label = Binomial2))+
  geom_text(aes(label = Binomial0))
plunge 

####################plot by dive score##############
#mypal <- colorRampPalette(rev(brewer.pal(6, "Blues")))
sequential_hcl(5, palette = "Purple-Blue", rev = T)
BLUE<-c("white","#d0d1e6",
"#a6bddb",
"#74a9cf",
"#2b8cbe",
"#045a8d")

divecol<-c("white",sequential_hcl(6, palette = "Purple-Blue",rev = T)[2:6])

speciesPCAvalues$divescore <- fct_explicit_na(speciesPCAvalues$divescore)
speciesPCAvalues$divescore<-relevel(speciesPCAvalues$divescore,ref = "(Missing)")



runPCAplotdive<-function(group, p1,p2,n1,n2){
  scattercat1<-ggplot(speciesPCAvalues, aes_string(x = p1, y = p2, label = "Binomial")) +
    theme_classic()+
    xlab(label = paste0("p",as.character(p1),"(",
                        as.character(signif(d$percentexplained[n1], digits = 3)),
                        "%)"))+
    ylab(label = paste0("p",as.character(p2),"(",
                        as.character(signif(d$percentexplained[n2], digits = 3)),
                        "%)"))+
    #theme(legend.position = "none")+
    geom_encircle(data = speciesPCAvalues[which(is.na(speciesPCAvalues$divescore)),],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$divescore==0,],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$divescore==1,],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$divescore==2,],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$divescore==3,],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(data = speciesPCAvalues[speciesPCAvalues$divescore==4,],
                  aes_string(),s_shape=1, expand=0, alpha = 1)+
    geom_encircle(aes_string(fill = group),s_shape=1, expand=0, color = "black", alpha = 0.7)+#s_shape = 1 and expan = 0 are convex hull
    geom_point(aes_string(color = group), shape = 21, size = 3, color = "black")
  #scale_color_manual(values = alpha(c("black","black","black","black","black","black",0.2))
  scattercat1
}
runPCAplotdive("divescore","PC1","PC2",1,2)

divescore<-runPCAplotdive("divescore","PC1","PC2",1,2)+
  geom_point(aes(fill = divescore), size = 3, shape = 21,col = "black", alpha = 1)+
  scale_fill_manual(values = divecol)+
  #scale_alpha_manual(values = c(-.5,1,0,0,56))
  theme(legend.position = "right")
divescore

  

########plot LOADINGS###############
pPCAloadings$factor<-row.names(pPCAloadings)
pPCAloadings$factor<-gsub("RES_log"," ",pPCAloadings$factor)
pPCAloadings$factor<-gsub("logHeadmassg","",pPCAloadings$factor)


loadings1<-ggplot(pPCAloadings, aes(x = PC1, y = PC2, label =factor)) +
  xlab(label = "pPC1")+
  ylab(label = "pPC2")+
  
  #geom_point(aes(), size = 0.005) +
  #geom_point(data = pPCAloadings)+
  geom_text_repel(aes()) +
  theme_classic()+
  xlim(-1.5,+1)+
  ylim(-1,1)+
  geom_segment(aes(x = 0, y = 0,xend = PC1, yend = PC2), arrow = arrow(type = "closed", length = unit(0.10,"inches")))+
  theme(legend.position = "bottom")
loadings1


ggarrange(loadings1,plunge,divescore, order,labels = c("A","B","C","D"))

ggsave("D:/00_Manuscripts/0Avian aquatic hearing project/___Oct 1 version/PCAOct 4_noair.pdf",width = 10, height = 10)
ggsave("D:/00_Manuscripts/0Avian aquatic hearing project/___Oct 1 version/PCAOct 4_withair.pdf",width = 10, height = 10)

ggsave("D:/Analysis_plots/PCA_Dec1.pdf",width = 10, height = 10)







