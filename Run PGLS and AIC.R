library(flextable)
library(officer)

options(scipen=999, digits = 3)# take out of scientific notation and allow 3 significant digits

########################################pgls function####################################
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.0011,1)))#####
}


###########run different pgls models and save as a list of tables########
############Each table being the stats results for a different ear measure
source("pgls_HM.R") # model with only head mass
source("pgls_HM_plus_ecol.R") #model with ecological groupings
source("pgls_HM_times_ecol.R") #model with ecological groupings and groupin x head mass interaction term

#for aquatic-only analysis, go back and create a new data object lacking Terrestrial species, and also include models with dive score:
source("pgls_HM_plus_divescore.R") #model with dive score
source("pgls_HM_times_divescore.R") #model with divescore and head mass x dive score interaction

####for each ear measure, combine together the different model outputs into single dataframe
grouped_eachmeasure<-list()
for (i in 1:length(tbllist_HM)){
  grouped_eachmeasure[[i]]<-rbind(tbllist_HM[[i]],tbllist_HM_plus_ecol[[i]],
                   tbllist_HM_times_ecol[[i]])                   #,
}

##(for aquatic-only analysis, also include models with dive score)
grouped_eachmeasure<-list()
for (i in 1:length(tbllist_HM)){
  grouped_eachmeasure[[i]]<-rbind(tbllist_HM[[i]],tbllist_HM_plus_ecol[[i]],
                                  tbllist_HM_times_ecol[[i]],
                                  tbllist_HM_plus_divescore[[i]],tbllist_HM_times_divescore[[i]])                   #,
}

##########The following remaining code does two things:############### 
#(1) get the AIC values to select the best model
#(2) report the details of the best models (models with change AIC)

############(1)Get the summary stats for each model############
#Compute the AIC, relative likelihood and AIC weight
add_AIC<-list()

for (i in 1:length(tbllist_HM)){
add_AIC[[i]]<-grouped_eachmeasure[[i]][!duplicated(grouped_eachmeasure[[i]]$Model),c("Model","Adj_Rsquared","Lambda","Fstat","Fstat_numdf","Fstat_dendf",
                                        "AICc")]
add_AIC[[i]]$ChangeAIC<-add_AIC[[i]]$AICc-min(add_AIC[[i]]$AICc)
add_AIC[[i]]$relativelikelihood<-exp(-0.5*add_AIC[[i]]$ChangeAIC)
add_AIC[[i]]$AICweight_calc<-add_AIC[[i]]$relativelikelihood/sum(add_AIC[[i]]$relativelikelihood)
}

#Sort rows by AIC, low to high
add_AIC2<-list()
for (i in 1:length(add_AIC)){
  add_AIC2[[i]]<-arrange(add_AIC[[i]],add_AIC[[i]]$AICc)
}

##combine into one dataframe and print
all<-do.call(rbind.data.frame,add_AIC2)
all$Measurementgroup<-""#add extra column for category
all$Measurementgroup<-ifelse(grepl("Columella",all$Model),"Columella size",all$Model)
all$Measurementgroup<-ifelse(grepl("CA",all$Model),"Cochlear aqueduct",all$Measurementgroup)
all$Measurementgroup<-ifelse(grepl(
  "area_ratio|Umbo_distancetoTMplane|dis_coltip_TMcentroid|meanTMangle|FPtotalarea|TMtotalarea|RWtotalarea|totalEClength" ,all$Model),  
  "Impedance matching",all$Measurementgroup)
all$Measurementgroup<-ifelse(grepl("Behind.TM",all$Model),"Airvolume",all$Measurementgroup)
all<-all[,c(ncol(all),(1:(ncol(all)-1)))]
all$Measurementgroup[which(duplicated(all$Measurementgroup))]<-""
all$F_<-paste0(all$Fstat,"(",all$Fstat_numdf,",",all$Fstat_dendf,")")

#visualize the table better using the flextable package
flexall<-flextable(all) %>% add_header_lines(
  values = "Table X. Models for selection") %>%
  bold(i = ~ ChangeAIC < 0.01) %>% # select columns add: j = ~ Coefficients + P.val
  autofit()
flexall

######write table to word file####
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)

#print to file
print(toprint,target = "E:/Analysis_plots/AIC best model Mar31 no terr.docx")
print(toprint,target = "E:/Analysis_plots/AIC Mar 31 compare_terr.docx")


########(2)Get model details for best model###########
#arrange rows by AIC, low to high
details<-list()
for (i in 1:length(add_AIC)){
  details[[i]]<-arrange(grouped_eachmeasure[[i]],grouped_eachmeasure[[i]]$AICc)#sort table by IA
  details[[i]]$ChangeAIC<-details[[i]]$AICc-min(details[[i]]$AICc)#add change in AIC column
  details[[i]]<-details[[i]][details[[i]]$ChangeAIC<=3,c("Model","Coefficients","Estimate","Std. Error","P.val","ChangeAIC")]#select only the best model (first one where change = 0)
  details[[i]]$Model[2:nrow(details[[i]])]<-""
}

#print
modeldetails<-do.call(rbind.data.frame,details)#combine into one dataframe
modeldetails$Measurementgroup<-""#add extra column for category
modeldetails$Measurementgroup<-ifelse(grepl("Columella",modeldetails$Model),"Columella size",modeldetails$Model)
modeldetails$Measurementgroup<-ifelse(grepl("CA",modeldetails$Model),"Cochlear aqueduct",modeldetails$Measurementgroup)
modeldetails$Measurementgroup<-ifelse(grepl(
  "area_ratio|Umbo_distancetoTMplane|dis_coltip_TMcentroid|meanTMangle|FPtotalarea|TMtotalarea|RWtotalarea|totalEClength" ,modeldetails$Model),  
"Impedance matching",modeldetails$Measurementgroup)
modeldetails$Measurementgroup<-ifelse(grepl("Behind.TM",modeldetails$Model),"Airvolume",modeldetails$Measurementgroup)
modeldetails<-modeldetails[,c(ncol(modeldetails),(1:(ncol(modeldetails)-1)))]


#visualize the table better using the flextable package
flexall<-flextable(modeldetails) %>% add_header_lines(
                       values = "Table X. Model details for best model") %>%
bold(i = ~ P.val < 0.05) %>% # select columns add: j = ~ Coefficients + P.val
autofit()
flexall

#######print to word file###########
toprint<-read_docx() #create word doc object
body_add_flextable(toprint,flexall)#add pgls output table
body_end_section_landscape(toprint)

print(toprint,target = "E:/Analysis_plots/AIC Details Dec 31 no terr.docx")
print(toprint,target = "E:/Analysis_plots/DetailsMar 31 terr compare.docx")

