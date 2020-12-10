#Plotting adapted from:
#https://thackl.github.io/ggtree-composite-plots
library(patchwork)
library(viridis)

#color palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c(	"#FFFFFF","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
barplot(1:5, col=cbbPalette)

#subset the df with resids datafram to include only the residuals
names(dfwithresids)
#dfwithresids$PC1<-speciesPCAvalues$PC1[match(dfwithresids$Binomial,row.names(speciesPCAvalues))]

#input column numbers that have residuals
residseach<-dfwithresids[,c(37:48)]

names(residseach)

#add 
residseach$order<-dfwithresids$Order
residseach$Binomial<-dfwithresids$Binomial
residseach$Category<-dfwithresids$Category
residseach$divescore<-dfwithresids$divescore
residseach$plungedistinct<-dfwithresids$plungedistinct
residseach$waterbirds<-dfwithresids$waterbirds
residseach$superorder<-dfwithresids$superorder
residseach$IAC<-dfwithresids$IAC_detail
residseach$IBP<-dfwithresids$IBP_detail
residseach$PC1<-dfwithresids$PC1


residseach$plungedistinct<-relevel(as.factor(residseach$plungedistinct),"Terrestrial")
#residseach$Category<-relevel(as.factor(residseach$Category),"Terrestrial")

#put in long format
longdfplotting<-gather(residseach,key = "earmeasuresresid", value = "earmeasureval", 
                       -c(order,Binomial,Category,divescore,plungedistinct,IAC,IBP))#waterbirds,superorder,
longdfplotting$earmeasuresresid<-as.factor(longdfplotting$earmeasuresresid)
longdfplotting$earmeasureval<-as.numeric(longdfplotting$earmeasureval)
#longdfplotting$label<-longdfplotting$order
#longdfplotting$label<-longdfplotting$waterbirds
#longdfplotting$label<-longdfplotting$superorder

longdfplotting$label<-longdfplotting$order
longdfplotting$earmeasuresresid<-as.character(longdfplotting$earmeasuresresid)

longdfplotting$Terr<-longdfplotting$Category
longdfplotting$Terr<-ifelse(longdfplotting$Category=="Terrestrial","Terrestrial",NA)

#make list of ear measures to plot
yvarnames<-c(
  "RES_logTMtotalarealogHeadmassg"            ,
  "RES_logFPtotalarealogHeadmassg"            ,
  "RES_logarearatiologHeadmassg"              ,
  "RES_logdiscoltipTMcentroidlogHeadmassg"    ,
  "RES_logUmbodistancetoTMplanelogHeadmassg",
  "RES_logmeanTManglelogHeadmassg"            ,
  "RES_logtotalEClengthlogHeadmassg"          ,
  "RES_logRWtotalarealogHeadmassg"            ,
  "RES_logCAtotalarealogHeadmassg"            ,
  "RES_logBehindTMlogHeadmassg"              ,
  "RES_logColumellalengthmmlogHeadmassg"      ,
  "RES_logColumellavolumemm3logHeadmassg"     ,"PC1")



#pointrange<-subset(longdfplotting,longdfplotting$earmeasuresresid==yvarnames[4])  %>% group_by(order) %>% 
#    summarize(mn = mean(earmeasureval, na.rm = T), sd = sd(earmeasureval, na.rm = T)/n())



source("Tblog.R")
#add medians
source("add median.R")

#make the order-level cladogram for aligning the residuals
source("Order_level_cladogram.R")
###
ggtree(orderPhy)+
  geom_text(aes(label = node))

gg_tr <- ggtree(orderPhy, branch.length = "none") + 
  geom_tiplab(align=TRUE) +
  #scale_x_continuous(expand=expand_scale(0.2)) + # make more room for the labels
  scale_y_tree()+ 
  xlim(0,40)+
  ylim(-3.5,26.5)+
  geom_text(aes(x = 13,y = -0,label = "Terrestrial"))+
  geom_text(aes(x = 13,y = -1,label = "Surface-foraging"))+
  geom_text(aes(x = 13,y = -2,label = "Plunge-diving"))+
  geom_text(aes(x = 13,y = -3,label = "Underwater-pursuit"))
gg_tr

#reversed phylogenetic tree
gg_tr_rev <- ggtree(orderPhy, branch.length = "none", col = "white") + 
  geom_tiplab(align=TRUE) +
  scale_x_continuous(expand=expand_scale(0.2)) + # make more room for the labels
  scale_y_tree()+
  xlim(10,20)+
  ylim(-3.5,26.5)+
  scale_x_reverse()+
  geom_text(aes(x = 11,y = -0,label = "Terrestrial"))+
  geom_text(aes(x = 11,y = -1,label = "Surface-foraging"))+
  geom_text(aes(x = 11,y = -2,label = "Plunge-diving"))+
  geom_text(aes(x = 11,y = -3,label = "Underwater-pursuit"))
gg_tr_rev

#plungedistinct
gg_plungedistinct<-function(index2, letter, box = "yes"){
  ggtreeplot(gg_tr, subset(longdfplotting,
                           longdfplotting$earmeasuresresid==yvarnames[index2]), aes(y=earmeasureval), flip=TRUE) +{
    
    if(box == "yes") geom_rect(aes(xmin = -3.5, xmax = 0.5, ymin = Inf, ymax = -Inf), fill = "grey", alpha = 0.1) else geom_rect(aes(xmin = 0.5, xmax = -4.5, ymin = Inf, ymax = -Inf), fill = "white", alpha = 0.001)
    } +
    #  geom_vline(xintercept = -4:30, col = "grey")+
    #geom_rect(aes(xmin = 9.5, xmax = 17.5, ymin = -Inf, ymax = -Inf), col = "black", fill = "white", alpha = 0.1)+
    geom_point(aes(fill = plungedistinct), size = 2,shape = 21, col = "black")+
     scale_fill_manual(values = cbbPalette)+
    coord_flip() + no_y_axis()+
    ylab("")+
    xlim(-3.5,26.5)+
    geom_hline(yintercept = 0, col = "grey")+
    theme_classic() +
    theme(axis.line.y = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90),
          legend.position = "none",
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))+
    geom_boxplot(data = subset(longdfplotting,
                               longdfplotting$earmeasuresresid==yvarnames[index2]&
                                 longdfplotting$plungedistinct=="Terrestrial"),
                 aes(x = 0, y = earmeasureval),fill= "white",outlier.size = 2,
                 outlier.colour = "black", outlier.fill = "white",
                 outlier.shape = 21)+
    geom_text(aes(x=Inf,y=-Inf,vjust = 1,
                  hjust = -1,label=letter))
   }

gg_plungedistinct(1, letter = "K",box = "no")


gg_plungedistinct(1, letter = "K", box = "yes")


addbxplt<-function(j,index2,letter,box = "yes"){
  d<-gg_plungedistinct(index2,letter,box)+
  geom_boxplot(data = subset(longdfplotting,
                             longdfplotting$earmeasuresresid==yvarnames[j]&
                               longdfplotting$plungedistinct=="Plunging"),
               aes(x = -2, y = earmeasureval),fill= "black",outlier.size = 2,
               outlier.fill = "#000000", outlier.shape = 21, outlier.color = "black")+
  geom_boxplot(data = subset(longdfplotting,
                             longdfplotting$earmeasuresresid==yvarnames[j]&
                               longdfplotting$plungedistinct=="Underwater pursuit"),
               aes(x = -3, y = earmeasureval),fill ="#56B4E9",outlier.size = 2,
               outlier.fill = "#56B4E9", outlier.shape = 21, outlier.color = "black")+
    
    geom_boxplot(data = subset(longdfplotting,
                               longdfplotting$earmeasuresresid==yvarnames[j]&
                                 longdfplotting$plungedistinct=="Surface"),
                 mapping = aes(x = -1, y = earmeasureval), fill = "#E69F00",
                 outlier.size = 2, outlier.fill = "#E69F00", outlier.shape = 21, outlier.color = "black")
  
    
  d
}

#one row, modified proportions
gg_tr|addbxplt(1,1,"a")+addbxplt(2,2,"b")|
  addbxplt(3,3,"c")+addbxplt(4,4,"d")|#
    addbxplt(5,5,"e")+addbxplt(6,6,"f")|#umbo height and TM angle
  addbxplt(8,8,"g")+addbxplt(9,9,"h", box = "no")|#ESlength & RW 
  addbxplt(7,7,"i")+addbxplt(11,11,"j")|#CA and collenght
    addbxplt(12,12,"k", box = "no")+addbxplt(10,10,"l")|#colvol and air
  IACfull("l")+IBPfull("m")|gg_tr_rev

gg_tr+IBPfull("g")

#connectivity plots
#data for plotting by order
summ2<-avgdf %>% group_by(Order,IBP_detail) %>% count(na.omit = T) 
summ2$IBP_detail<-ifelse(summ2$IBP_detail=="Pneumaticity present"|
                           summ2$IBP_detail=="Pneumaticity absent"|
                           summ2$IBP_detail=="Y",summ2$IBP_detail,NA)

summ2$label<-summ2$Order
summ2$number<-summ2$n
summ2<-as.data.frame(summ2)
names(summ2)
summb<-summ2[which(!is.na(summ2$IBP_detail)),]

#data for plotting by ecology
summpl<-avgdf %>% group_by(IBP_detail,plungedistinct) %>% count(na.omit = T) 
summpl$IBP_detail<-ifelse(summpl$IBP_detail=="Pneumaticity present"|
                            summpl$IBP_detail=="Pneumaticity absent"|
                            summpl$IBP_detail=="Y",summpl$IBP_detail,NA)

summpl$number<-summpl$n
summpl<-as.data.frame(summpl)
names(summpl)
summpl_<-summpl[which(!is.na(summpl$IBP_detail)),]


IBP<-function(letter){
  d<-ggtreeplot(gg_tr, summb, aes(y = number), flip=TRUE) +
    geom_bar(aes(fill = as.factor(IBP_detail)), position = "fill",
             color = "black",stat = "identity")+
    scale_fill_manual(values = c("black","grey","white"))+
    #no_legend()+
    xlim(-3.5,26.5)+
    coord_flip() + no_y_axis()+
    theme(axis.text.x = element_text(angle = 90))+
    #ylab("Percentage of counts by group")+
    theme_classic() +
    theme(axis.line.y = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90),
          legend.position = "none",
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))+
    geom_text(aes(x=Inf,y=-Inf,vjust = 1,
                  hjust = -1,label=letter))
  
  d
}
IBP("d")


IBPfull<-function(letter){
  IBP(letter)+
    geom_col(data = summpl_[summpl_$plungedistinct=="Underwater pursuit",],
             aes(x = -3, fill = IBP_detail),col = "black", position = "fill")+
    geom_col(data = summpl_[summpl_$plungedistinct=="Terrestrial",],
             aes(x = 0, fill = IBP_detail),col = "black", position = "fill")+
    geom_col(data = summpl_[summpl_$plungedistinct=="Plunging",],
             aes(x = -2, fill = IBP_detail),col = "black", position = "fill")+
    geom_col(data = summpl_[summpl_$plungedistinct=="Surface",],
             aes(x = -1, fill = IBP_detail),col = "black", position = "fill")
}
IBPfull("l")

###
summ<-avgdf %>% group_by(Order,IAC_detail) %>% count(na.omit = T) 
summ$IAC_detail<-ifelse(summ$IAC_detail=="Pneumaticity present"|
                          summ$IAC_detail=="Pneumaticity absent"|
                          summ$IAC_detail=="Y",summ$IAC_detail,NA)


summ$label<-summ$Order
summ$number<-summ$n
summ<-as.data.frame(summ)
summa<-summ[which(!is.na(summ$IAC_detail)),]


summpl2<-avgdf %>% group_by(IAC_detail,plungedistinct) %>% count(na.omit = T) 
summpl2$IAC_detail<-ifelse(summpl2$IAC_detail=="Pneumaticity present"|
                             summpl2$IAC_detail=="Pneumaticity absent"|
                             summpl2$IAC_detail=="Y",summpl2$IAC_detail,NA)

summpl2$number<-summpl2$n
summpl2<-as.data.frame(summpl2)
names(summpl2)
summpl2_<-summpl2[which(!is.na(summpl2$IAC_detail)),]


#IAC
IAC<-function(letter){
  d<-ggtreeplot(gg_tr, summa, aes(y = number), flip=TRUE) +
    geom_bar(aes(fill = as.factor(IAC_detail)), position = "fill",
             color = "black",stat = "identity")+
    scale_fill_manual(values = c("black","grey","white"))+
    #no_legend()+
    xlim(-3.5,26.5)+
    coord_flip() + no_y_axis()+
    theme(axis.text.x = element_text(angle = 90))+
    #ylab("Percentage of counts by group")+
    theme_classic() +
    theme(axis.line.y = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90),
          legend.position = "none",
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))+
    geom_text(aes(x=Inf,y=-Inf,vjust = 1,
                  hjust = -1,label=letter))
  
  d
}
IAC("d")


IACfull<-function(letter){
  
  IAC(letter)+
    geom_col(data = summpl2_[summpl2_$plungedistinct=="Underwater pursuit",],
             aes(x = -3, fill = IAC_detail),col = "black", position = "fill")+
    geom_col(data = summpl2_[summpl2_$plungedistinct=="Terrestrial",],
             aes(x = 0, fill = IAC_detail),col = "black", position = "fill")+
    geom_col(data = summpl2_[summpl2_$plungedistinct=="Plunging",],
             aes(x = -2, fill = IAC_detail),col = "black", position = "fill")+
    geom_col(data = summpl2_[summpl2_$plungedistinct=="Surface",],
             aes(x = -1, fill = IAC_detail),col = "black", position = "fill")
}
IACfull("m")