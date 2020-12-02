#a-simple-phylogenetic-model

library(brms)

##############Example test from:#######################################################
####https://cran.r-project.org/web/packages/brms/vignettes/brms_phylogenetics.html
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt", 
  header = TRUE
)
head(data_simple)

#phylogeny
A <- ape::vcv.phylo(phylo)

#run the model
model_simple <- brm(
  phen ~ cofactor + (1|gr(phylo, cov = A)), 
  data = data_simple, 
  family = gaussian(), 
  data2 = list(A = A)
 # prior = c(
  #  prior(normal(0, 10), "b"),
  #  prior(normal(0, 50), "Intercept"),
  #  prior(student_t(3, 0, 20), "sd"),
  #  prior(student_t(3, 0, 20), "sigma")
  #)
)

summary(model_simple)

###############################################################################################
#
#remove samples that are omitted due to seepage filling the cranial airspace
dfconnectivity<-avgdf[avgdf$`fluid.filled.` != "fluid filled",]
IACdetailclean<-dfconnectivity[-which(dfconnectivity$IAC_detail==""|
                                        dfconnectivity$IAC_detail ==" "),]

#select variables needed for model
data_simple2 <- cbind.data.frame(IACdetailclean$Binomial,IACdetailclean$plungedistinct,IACdetailclean$IAC_detail,IACdetailclean$divescore)
colnames(data_simple2)<-c("binomial","plungedistinct","IAC","divescore")
head(data_simple2)
data_simple2$plungedistinct<-as.factor(data_simple2$plungedistinct)
data_simple2$IAC<-as.factor(data_simple2$IAC)

#relevel the data
levels(data_simple2$plungedistinct)
levels(data_simple2$IAC)
data_simple2$plungedistinct<-relevel(data_simple2$plungedistinct, ref = "Terrestrial")
data_simple2$IAC<-relevel(data_simple2$IAC, ref = "Y")
data_simple2$IAC<-factor(data_simple2$IAC, order = T, levels = c("Y",
                                                                           "Pneumaticity present",
                                                                           "Pneumaticity absent"))
levels(data_simple2$plungedistinct)
levels(data_simple2$IAC)
head(data_simple2)


##prune the phylogeny to match the dataset
phylokeep<-keep.tip(birdtreels,as.character(data_simple2$binomial))
A2 <- ape::vcv.phylo(phylokeep)

#run the model
model_simple2 <- brm(
  IAC ~ 1+ plungedistinct + (1|gr(binomial, cov = A2)), 
  data = data_simple2, 
  family = cumulative(), 
  data2 = list(A2 = A2),
  chains = 2,
  iter = 6000
)


summary(model_simple2)

#plot conditional probabilities
p<-plot(conditional_effects(model_simple2,categorical = TRUE))
ggIAC<-p$`plungedistinct:cats__`
IACplt<-ggIAC+theme_classic()+
  xlab("Ecological plungedistinct")
IACplt

feIAC<-as.data.frame(fixef(model_simple2, summary = FALSE))
psIAC<-as.data.frame(posterior_summary(model_simple2))

#plot 95% credible interval by ecological group
topltIAC<-psIAC[3:5,]
topltIAC$cat<-row.names(topltIAC)
ggplot(topltIAC, aes(y = cat,x = Estimate))+
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5))

#plot posterior distrbution and 95% credible interval
IACeffct<-ggplot(feIAC)+
  geom_density(aes(x = plungedistinctPlunging), col = "black", fill = "black", alpha = 0.5)+
  geom_density(aes(x = plungedistinctSurface), col = "grey", fill = "grey", alpha = 0.5)+
  geom_density(aes(x = plungedistinctUnderwaterpursuit), col = "blue", fill = "blue", alpha = 0.5)+
  theme_classic()+
  xlab("Estimate")+
  ylab("Posterior distribution")+
  geom_pointrange(data = topltIAC[topltIAC$cat=="b_plungedistinctPlunging",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.0125), col = "black", fill = "grey", alpha = 0.5)+
    geom_pointrange(data = topltIAC[topltIAC$cat=="b_plungedistinctSurface",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.01), col = "grey", fill = "grey", alpha = 0.5)+
  geom_pointrange(data = topltIAC[topltIAC$cat=="b_plungedistinctUnderwaterpursuit",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.015), col = "blue", fill = "blue", alpha = 0.5)
IACeffct

###################3analysis aquatic only
data_simple2_aq<-data_simple2[data_simple2$plungedistinct!="Terrestrial",]
data_simple2_aq$plungedistinct<-droplevels(data_simple2_aq$plungedistinct,exclude = "Terrestrial")
levels(data_simple2_aq$plungedistinct)
data_simple2_aq$plungedistinct<-relevel(data_simple2_aq$plungedistinct, ref = "Surface")


phylokeep3<-keep.tip(birdtreels,data_simple2_aq$binomial)
A3 <- ape::vcv.phylo(phylokeep3)

model_simple2_aq <- brm(
  IAC ~ 1+ plungedistinct + (1|gr(binomial, cov = A3)), 
  data = data_simple2_aq, 
  family = cumulative(), 
  data2 = list(A3 = A3),
  chains = 2,
  iter = 6000
)

summary(model_simple2_aq)

#plot conditional probabilities
p<-plot(conditional_effects(model_simple2_aq,categorical = TRUE))
ggIAC<-p$`plungedistinct:cats__`
IACplt<-ggIAC+theme_classic()+
  xlab("Ecological plungedistinct")
IACplt

feIAC<-as.data.frame(fixef(model_simple2_aq, summary = FALSE))
psIAC<-as.data.frame(posterior_summary(model_simple2_aq))

#plot 95% credible interval by ecological group
topltIAC<-psIAC[3:4,]
topltIAC$cat<-row.names(topltIAC)
ggplot(topltIAC, aes(y = cat,x = Estimate))+
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5))

#plot posterior distrbution and 95% credible interval
IACeffct<-ggplot(feIAC)+
  geom_density(aes(x = plungedistinctPlunging), col = "black", fill = "black", alpha = 0.5)+
  geom_density(aes(x = plungedistinctUnderwaterpursuit), col = "blue", fill = "blue", alpha = 0.5)+
  theme_classic()+
  xlab("Estimate")+
  ylab("Posterior distribution")+
  geom_pointrange(data = topltIAC[topltIAC$cat=="b_plungedistinctPlunging",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.0125), col = "black", fill = "grey", alpha = 0.5)+
  geom_pointrange(data = topltIAC[topltIAC$cat=="b_plungedistinctUnderwaterpursuit",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.015), col = "blue", fill = "blue", alpha = 0.5)
IACeffct


##########

data_simple2_aq<-data_simple2[data_simple2$plungedistinct!="Terrestrial",]
data_simple2_aq$plungedistinct<-droplevels(data_simple2_aq$plungedistinct,exclude = "Terrestrial")
levels(data_simple2_aq$plungedistinct)
data_simple2_aq$plungedistinct<-relevel(data_simple2_aq$plungedistinct, ref = "Surface")


phylokeep3<-keep.tip(birdtreels,data_simple2_aq$binomial)
A3 <- ape::vcv.phylo(phylokeep3)

model_simple2_divescore <- brm(
  IAC ~ 1+ divescore + (1|gr(binomial, cov = A3)), 
  data = data_simple2_aq, 
  family = cumulative(), 
  data2 = list(A3 = A3),
  chains = 2,
  iter = 6000
)

summary(model_simple2_divescore)

#plot conditional probabilities
p<-plot(conditional_effects(model_simple2_divescore,categorical = TRUE))
ggIAC<-p$`divescore:cats__`
IACplt<-ggIAC+theme_classic()+
  xlab("divescore")
IACplt

feIAC<-as.data.frame(fixef(model_simple2_divescore, summary = FALSE))
psIAC<-as.data.frame(posterior_summary(model_simple2_divescore))

#plot 95% credible interval by ecological group
topltIAC<-psIAC[3,]
topltIAC$cat<-row.names(topltIAC)
ggplot(topltIAC, aes(y = cat,x = Estimate))+
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5))

#plot posterior distrbution and 95% credible interval
IACeffct<-ggplot(feIAC)+
  geom_density(aes(x = divescore), col = "black", fill = "black", alpha = 0.5)+
  theme_classic()+
  xlab("Estimate")+
  ylab("Posterior distribution")+
  geom_pointrange(data = topltIAC[topltIAC$cat=="b_divescore",], aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = -0.0125), col = "black", fill = "grey", alpha = 0.5)
IACeffct




