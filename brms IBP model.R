library(brms)

dfconnectivity<-avgdf[avgdf$`fluid.filled.` != "fluid filled",]
IBPdetailclean<-dfconnectivity[which(!is.na(dfconnectivity$IBP_detail)),]
IBPdetailclean<-dfconnectivity[-which(dfconnectivity$IBP_detail==""|dfconnectivity$IBP_detail ==" "),]
table(IBPdetailclean$IBP,IBPdetailclean$Category)


#IACdetailclean<-dfconnectivity[-which(dfconnectivity$IAC_detail==""|
#                                        dfconnectivity$IAC_detail ==" "),]

#IACdetailclean<-dfconnectivity[which(!is.na(dfconnectivity$IAC_detail)),]
#IBPdetailclean<-dfconnectivity[which(!is.na(dfconnectivity$IBP_detail)),]



data_simple3 <- cbind.data.frame(IBPdetailclean$Binomial,as.factor(IBPdetailclean$plungedistinct),IBPdetailclean$IBP_detail, IBPdetailclean$divescore)
colnames(data_simple3)<-c("binomial","plungedistinct","IBP","divescore")
head(data_simple3)
data_simple3$plungedistinct<-as.factor(data_simple3$plungedistinct)
data_simple3$IBP<-as.factor(data_simple3$IBP)

data_simple3$plungedistinct<-relevel(data_simple3$plungedistinct, ref = "Terrestrial")
levels(data_simple3$plungedistinct)


data_simple3$IBP<-factor(data_simple3$IBP, order = T, levels = c("Y",
                                                                 "Pneumaticity present",
                                                                 "Pneumaticity absent"))
#data_simple3$IBP<-relevel(data_simple3$IBP, ref = "Y")
levels(data_simple3$IBP)
head(data_simple3)

#IBPdetailclean<-dfconnectivity[-which(dfconnectivity$IBP_detail==""|dfconnectivity$IBP_detail ==" "),]
#t(table(IBPdetailclean$IBP_detail,IBPdetailclean$Category))



phylokeep3<-keep.tip(birdtreels,data_simple3$binomial)
A3 <- ape::vcv.phylo(phylokeep3)

model_simple3 <- brm(
  IBP ~ 1+ plungedistinct + (1|gr(binomial, cov = A3)), 
  data = data_simple3, 
  family = cumulative(), 
  data2 = list(A3 = A3),
  chains = 2,
  iter = 6000
)

summary(model_simple3)

#plot conditional probabilities
p<-plot(conditional_effects(model_simple3,categorical = TRUE))
ggIBP<-p$`plungedistinct:cats__`
IBPplt<-ggIBP+theme_classic()+
  xlab("Ecological plungedistinct")
IBPplt

feIBP<-as.data.frame(fixef(model_simple3, summary = FALSE))
psIBP<-as.data.frame(posterior_summary(model_simple3))

#plot 95% credible interval by ecological group
topltIBP<-psIBP[3:5,]
topltIBP$cat<-row.names(topltIBP)
ggplot(topltIBP, aes(y = cat,x = Estimate))+
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5))

#plot posterior distrbution and 95% credible interval
IBPeffct<-ggplot(feIBP)+
  geom_density(aes(x = plungedistinctPlunging), col = "black", fill = "black", alpha = 0.5)+
  geom_density(aes(x = plungedistinctSurface), col = "grey", fill = "grey", alpha = 0.5)+
  geom_density(aes(x = plungedistinctUnderwaterpursuit), col = "blue", fill = "blue", alpha = 0.5)+
  theme_classic()+
  xlab("Estimate")+
  ylab("Posterior distribution")+
  geom_pointrange(data = topltIBP[topltIBP$cat=="b_plungedistinctPlunging",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.0125), col = "black", fill = "grey", alpha = 0.5)+
  geom_pointrange(data = topltIBP[topltIBP$cat=="b_plungedistinctSurface",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.01), col = "grey", fill = "grey", alpha = 0.5)+
  geom_pointrange(data = topltIBP[topltIBP$cat=="b_plungedistinctUnderwaterpursuit",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.015), col = "blue", fill = "blue", alpha = 0.5)
IBPeffct

###################3analysis aquatic only
data_simple3_aq<-data_simple3[data_simple3$plungedistinct!="Terrestrial",]
data_simple3_aq$plungedistinct<-droplevels(data_simple3_aq$plungedistinct,exclude = "Terrestrial")
levels(data_simple3_aq$plungedistinct)
data_simple3_aq$plungedistinct<-relevel(data_simple3_aq$plungedistinct, ref = "Surface")


phylokeep3<-keep.tip(birdtreels,data_simple3_aq$binomial)
A3 <- ape::vcv.phylo(phylokeep3)

model_simple3_aq <- brm(
  IBP ~ 1+ plungedistinct + (1|gr(binomial, cov = A3)), 
  data = data_simple3_aq, 
  family = cumulative(), 
  data2 = list(A3 = A3),
  chains = 2,
  iter = 6000
)

summary(model_simple3_aq)

#plot conditional probabilities
p<-plot(conditional_effects(model_simple3_aq,categorical = TRUE))
ggIBP<-p$`plungedistinct:cats__`
IBPplt<-ggIBP+theme_classic()+
  xlab("Ecological plungedistinct")
IBPplt

feIBP<-as.data.frame(fixef(model_simple3_aq, summary = FALSE))
psIBP<-as.data.frame(posterior_summary(model_simple3_aq))

#plot 95% credible interval by ecological group
topltIBP<-psIBP[3:4,]
topltIBP$cat<-row.names(topltIBP)
ggplot(topltIBP, aes(y = cat,x = Estimate))+
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5))

#plot posterior distrbution and 95% credible interval
IBPeffct<-ggplot(feIBP)+
  geom_density(aes(x = plungedistinctPlunging), col = "black", fill = "black", alpha = 0.5)+
  geom_density(aes(x = plungedistinctUnderwaterpursuit), col = "blue", fill = "blue", alpha = 0.5)+
  theme_classic()+
  xlab("Estimate")+
  ylab("Posterior distribution")+
  geom_pointrange(data = topltIBP[topltIBP$cat=="b_plungedistinctPlunging",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.0125), col = "black", fill = "grey", alpha = 0.5)+
  geom_pointrange(data = topltIBP[topltIBP$cat=="b_plungedistinctUnderwaterpursuit",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.015), col = "blue", fill = "blue", alpha = 0.5)
IBPeffct


##########

data_simple3_aq<-data_simple3[data_simple3$plungedistinct!="Terrestrial",]
data_simple3_aq$plungedistinct<-droplevels(data_simple3_aq$plungedistinct,exclude = "Terrestrial")
levels(data_simple3_aq$plungedistinct)
data_simple3_aq$plungedistinct<-relevel(data_simple3_aq$plungedistinct, ref = "Surface")


phylokeep3<-keep.tip(birdtreels,data_simple3_aq$binomial)
A3 <- ape::vcv.phylo(phylokeep3)

model_simple3_divescore <- brm(
  IBP ~ 1+ divescore + (1|gr(binomial, cov = A3)), 
  data = data_simple3_aq, 
  family = cumulative(), 
  data2 = list(A3 = A3),
  chains = 2,
  iter = 6000
)

summary(model_simple3_divescore)

#plot conditional probabilities
p<-plot(conditional_effects(model_simple3_divescore,categorical = TRUE))
ggIBP<-p$`divescore:cats__`
IBPplt<-ggIBP+theme_classic()+
  xlab("divescore")
IBPplt

feIBP<-as.data.frame(fixef(model_simple3_divescore, summary = FALSE))
psIBP<-as.data.frame(posterior_summary(model_simple3_divescore))

#plot 95% credible interval by ecological group
topltIBP<-psIBP[3,]
topltIBP$cat<-row.names(topltIBP)
ggplot(topltIBP, aes(y = cat,x = Estimate))+
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5))

#plot posterior distrbution and 95% credible interval
IBPeffct<-ggplot(feIBP)+
  geom_density(aes(x = divescore), col = "black", fill = "black", alpha = 0.5)+
  theme_classic()+
  xlab("Estimate")+
  ylab("Posterior distribution")+
  geom_pointrange(data = topltIBP[topltIBP$cat=="b_divescore",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.0125), col = "black", fill = "grey", alpha = 0.5)
IBPeffct
