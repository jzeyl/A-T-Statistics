#https://thackl.github.io/ggtree-composite-plots
library(patchwork)
library(viridis)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c(	"#FFFFFF","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


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



source("D:/0Rbirdearanalysis/Tblog.R")
#add medians
source("D:/0Rbirdearanalysis/scripts Aq_terr/add median.R")


###
ggtree(orderPhy)+
  geom_text(aes(label = node))

gg_tr <- ggtree(orderPhy, branch.length = "none") + 
  geom_tiplab(align=TRUE, size =1.5) +
  #scale_x_continuous(expand=expand_scale(0.2)) + # make more room for the labels
  scale_y_tree()+ 
  xlim(0,40)+
  ylim(0,31)
gg_tr

#reversed phylogenetic tree
gg_tr_rev <- ggtree(orderPhy, branch.length = "none", col = "white") + geom_tiplab(align=TRUE) +
  scale_x_continuous(expand=expand_scale(0.2)) + # make more room for the labels
  scale_y_tree()+
xlim(0,40)+
  ylim(0,31)
  scale_x_reverse()
gg_tr_rev

#plungedistinct
gg_plungedistinct<-function(index2){
  ggtreeplot(gg_tr, subset(longdfplotting,
                           longdfplotting$earmeasuresresid==yvarnames[index2]), aes(y=earmeasureval), flip=TRUE) +
    geom_rect(aes(xmin = 25, xmax = 31, ymin = Inf, ymax = -Inf), fill = "grey", alpha = 0.1)+
    geom_rect(aes(xmin = 9.5, xmax = 17.5, ymin = Inf, ymax = -Inf), col = "black", fill = "white", alpha = 0.1)+
    geom_point(aes(fill = plungedistinct), size = 2,shape = 21, col = "black")+
    geom_vline(xintercept = 1:30, col = "grey")+
     scale_fill_manual(values = cbbPalette)+
    coord_flip() + no_y_axis()+
    ylab("")+
    xlim(0,31)+
    geom_hline(yintercept = 0, col = "grey")+
    theme_classic() +
    theme(axis.line.y = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          plot.margin = margin(0,0,0,0))+
   #geom_text(aes(label = "T"),x = 29,y = -1)+
   #geom_text(aes(label = "SF"),x = 30,y = -1)+
   #geom_text(aes(label = "P"),x = 28,y = -1)+
   #geom_text(aes(label = "UP"),x = 27,y = -1)+
    #geom_boxplot(aes(x = 29, y = earmeasureval[longdfplotting$plungedistinct=="Terrestrial"]))+
    geom_boxplot(data = subset(longdfplotting,
                               longdfplotting$earmeasuresresid==yvarnames[index2]&
                                 longdfplotting$plungedistinct=="Terrestrial"),
                 aes(x = 30, y = earmeasureval),fill= "white")
   }

gg_plungedistinct(1)

addbxplt<-function(j,index2){
  d<-gg_plungedistinct(index2)+
  geom_boxplot(data = subset(longdfplotting,
                             longdfplotting$earmeasuresresid==yvarnames[j]&
                               longdfplotting$plungedistinct=="Plunging"),
               aes(x = 28, y = earmeasureval),fill= "black")+
  geom_boxplot(data = subset(longdfplotting,
                             longdfplotting$earmeasuresresid==yvarnames[j]&
                               longdfplotting$plungedistinct=="Underwater pursuit"),
               aes(x = 27, y = earmeasureval),fill ="#56B4E9")+
    
    geom_boxplot(data = subset(longdfplotting,
                               longdfplotting$earmeasuresresid==yvarnames[j]&
                                 longdfplotting$plungedistinct=="Surface"),
                 mapping = aes(x = 29, y = earmeasureval), fill = "#E69F00")
  
    
  d
}
((gg_tr|addbxplt(1,1)|addbxplt(2,2)|addbxplt(3,3)|addbxplt(4,4)|
  addbxplt(5,5)|addbxplt(6,6)|addbxplt(7,7))/
(gg_tr||addbxplt(8,8)|
  addbxplt(9,9)|addbxplt(10,10)|addbxplt(11,11)|
  addbxplt(12,12)))+  plot_annotation(tag_levels="A")


gg_tr|addbxplt(1,1)|IAC
  
(gg_tr|addbxplt(1,1)|addbxplt(2,2)|addbxplt(3,3)|addbxplt(4,4)|
    addbxplt(5,5)|addbxplt(6,6)|gg_tr_rev|addbxplt(7,7)|addbxplt(8,8)|
     addbxplt(9,9)|addbxplt(10,10)|addbxplt(11,11)|
     addbxplt(12,12))

  plot_annotation(tag_levels="A")
  gg_tr_rev|
  
  
addbxT<-subset(longdfplotting,
       longdfplotting$earmeasuresresid==yvarnames[12]&
         longdfplotting$plungedistinct=="Terrestrial")

gg_plungedistinct(12) +
ggplot(subset(longdfplotting,
                  longdfplotting$earmeasuresresid==yvarnames[12]&
                    longdfplotting$plungedistinct=="Terrestrial"),
       aes(x = 29, y = earmeasureval))+
  geom_boxplot()

  geom_boxplot(addbxT,mapping = aes(x = 29, y = earmeasuresresid))
                      )


index2<-1
ggplot(subset(longdfplotting,
              longdfplotting$earmeasuresresid==yvarnames[index2]), aes(y=earmeasureval)) +
geom_boxplot(aes(group = plungedistinct, fill = plungedistinct))+
  geom_point(x = 2)
       
+ scale_shape_manual(values=c(1,2,3,4,5,6))

#TM,FP,Arearatio

gg_tr+scale_x_reverse()
gg_tr +  gg_divescore(1) +gg_divescore(2)+ gg_divescore(3) + plot_layout(widths = c(1, 1,1,1))+
  plot_annotation(tag_levels="A")

gg_tr +  gg_plungedistinct(1) +gg_plungedistinct(2)+ gg_plungedistinct(3) + plot_layout(widths = c(1, 1,1,1))+
  plot_annotation(tag_levels="A")

gg_tr +  ggh(9) +ggh(12) + ggh(9)  + 
  plot_layout(widths = c(1, 1,1,1))+
  plot_annotation(tag_levels="A")

gg_tr +  gg_plungedistinct(9) +gg_plungedistinct(12) + gg_plungedistinct(9)  + gg_tr
  plot_layout(widths = c(1, 1,1,1,1))+
  plot_annotation(tag_levels="A")

#3 lines
gg_tr |gg_plungedistinct(1) |gg_plungedistinct(2)|gg_plungedistinct(3)|gg_plungedistinct(4) |
gg_plungedistinct(5)| gg_plungedistinct(6)|gg_tr_rev|gg_plungedistinct(7)|gg_plungedistinct(8)|
gg_plungedistinct(9)|gg_plungedistinct(11) | gg_plungedistinct(12)| gg_plungedistinct(10)|IAC+IBP|gg_tr_rev
+
    plot_annotation(tag_levels="A") &theme(legend.position = "bottom")

ok +plot_layout(guides = "collect")
  combined <- p1 + p2 & theme(legend.position = "bottom")
  combined + plot_layout(guides = "collect")
  
(gg_tr |gg_plungedistinct(1) |gg_plungedistinct(2)|gg_plungedistinct(3)|gg_plungedistinct(4) |
      gg_plungedistinct(5)| gg_plungedistinct(6)|gg_plungedistinct(7)| gg_tr_rev)/
    (gg_tr |gg_plungedistinct(8)|gg_plungedistinct(9)|gg_plungedistinct(11) |
       gg_plungedistinct(12)| gg_plungedistinct(10)| gg_plungedistinct(13)|IAC|IBP|gg_tr_rev)+
    plot_annotation(tag_levels="A")
    

dfconnectivity<-avgdf[avgdf$`fluid.filled.by.columella.round.window.` != "fluid filled",]
dfconnectivity$IBP_detail_<-ifelse(dfconnectivity$IBP_detail=="Pneumaticity present"|
                                    dfconnectivity$IBP_detail=="Pneumaticity absent"|
                                    dfconnectivity$IBP_detail=="Y",dfconnectivity$IBP_detail,NA)

IACdetailclean<-dfconnectivity[-which(dfconnectivity$IAC_detail==""|
                                        dfconnectivity$IAC_detail ==" "),]
IACdetailclean<-dfconnectivity[which(!is.na(dfconnectivity$IAC_detail)),]
t(table(IACdetailclean$IAC_detail,IACdetailclean$Category))


IBPdetailclean<-dfconnectivity[-which(dfconnectivity$IBP_detail==""|dfconnectivity$IBP_detail ==" "),]
t(table(IBPdetailclean$IBP_detail,IBPdetailclean$Category))
IBPdetailclean$label<-IBPdetailclean$Order

IBPplot<-IBPdetailclean %>% group_by(Order,IBP_detail) %>% count(na.omit = T)
IBPplot$label<-IBPplot$Order

ggtreeplot(gg_tr, IBPplot, aes(y=n), flip=TRUE) +
  geom_col(aes(x = Order, y = n, fill = IBP_detail), position = "fill", color = "black") 

###
summ<-avgdf %>% group_by(Order,IAC_detail) %>% count(na.omit = T) 
summ$IAC_detail<-ifelse(summ$IAC_detail=="Pneumaticity present"|
                                     summ$IAC_detail=="Pneumaticity absent"|
                                     summ$IAC_detail=="Y",summ$IAC_detail,NA)


summ$label<-summ$Order
summ$number<-summ$n
summ<-as.data.frame(summ)
summa<-summ[which(!is.na(summ$IAC_detail)),]

##ggplot(summ) +
#  theme_classic()+
#  scale_y_continuous(labels = scales::percent) +
#  geom_col(aes(x = Order, y = n, fill = IBP_detail), position = "fill", color = "black") +
#  theme(axis.text.x = element_text(angle = 90))+
#  ylab("Percentage of counts by group")

IAC<-ggtreeplot(gg_tr, summa, aes(y=number), flip=TRUE) +
  geom_col(aes(fill = IAC_detail), position = "fill", color = "black")+
  scale_fill_brewer(palette = "Set1")+
  #no_legend()+
  xlim(0,31)+
  coord_flip() + no_y_axis()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Percentage of counts by group")+
  theme_classic() +
  theme(axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")#,
IAC   
display.brewer.all()

#gg_tr +IAC+ IBP + gg_divescore(1)+ plot_layout(widths = c(9, 1,1,1))+  
#  plot_annotation(tag_levels="A")#air

summ2<-avgdf %>% group_by(Order,IBP_detail) %>% count(na.omit = T) 
summ2$IBP_detail<-ifelse(summ2$IBP_detail=="Pneumaticity present"|
                          summ2$IBP_detail=="Pneumaticity absent"|
                          summ2$IBP_detail=="Y",summ2$IBP_detail,NA)

summ2$label<-summ2$Order
summ2$number<-summ2$n
summ2<-as.data.frame(summ2)
names(summ2)
summb<-summ2[which(!is.na(summ2$IBP_detail)),]

IBP<-ggtreeplot(gg_tr, summb, aes(y=number), flip=TRUE) +
  geom_col(aes(fill = IBP_detail), position = "fill", color = "black")+
  scale_fill_brewer(palette = "Set1")+
  coord_flip() + #no_y_axis()+
  xlim(0,31)+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Percentage of counts by group")+
  theme_classic() +
  theme(axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")
#scale_fill_brewer(palette="Set1")+
IBP

