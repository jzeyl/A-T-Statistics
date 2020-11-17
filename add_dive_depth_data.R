divedf<-read.csv(file.choose())
divedf<-divedf[1:57,]
#unique(divedf$Categories_feeding)
#ggpairs(log(divedf[7:10]))
#ggcorr(log(divedf[7:10]))


#SURFACE
divedf$feedmode2<-NA #reduce ecological groups
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Dipping", "Surface", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Surface seizing - plunge diving?", "Surface", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Dipping - Surface plunging? ", "Surface", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Pattering", "Surface", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Surface seizing", "Surface", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Surface diving", "Surface", divedf$feedmode2)

#UNDERWATER PURSUIT (non-plunging)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Pursuit diving", "Underwater pursuit", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Shallow swimming", "Underwater pursuit", divedf$feedmode2)

#PLUNGING (surface & pursuit plunging)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Surface plunging", "Plunging", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Pursuit plunging", "Plunging", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Surface plunging", "Plunging", divedf$feedmode2)
divedf$feedmode2<-ifelse(divedf$Categories_feeding=="Surface plunging", "Plunging", divedf$feedmode2)



#species that didn't match
#ind<-which(divedf$IOC_2020_Binomial %in% avgdf$Binomial == T)#corresponding row number in main dataframe

#check of all from divedf match to main df
match(divedf$IOC_2020_Binomial,avgdf$Binomial) #a list of the rows in the main df.

#add dive data to main df
avgdf$maxdd<-NA
avgdf$maxdd[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-divedf$Max_dive_depth
avgdf$divescore<-NA
avgdf$divescore[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-divedf$Dive.score
avgdf$catfeeding<-NA
avgdf$catfeeding[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Categories_feeding)
avgdf$catfeeding2<-NA
avgdf$catfeeding2[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$feedmode2)
avgdf$Avg_max_depth<-NA
avgdf$Avg_max_depth[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Avg_max_depth)

avgdf$Max_duration..s.<-NA
avgdf$Max_duration..s.[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Max_duration..s.)

avgdf$Avg_max_dur<-NA
avgdf$Avg_max_dur[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Avg_max_dur)

avgdf$Method<-NA
avgdf$Method[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Method)

avgdf$Reference_depth<-NA
avgdf$Reference_depth[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Reference_depth)

avgdf$Reference_duration<-NA
avgdf$Reference_duration[match(divedf$IOC_2020_Binomial,avgdf$Binomial)]<-as.character(divedf$Reference_duration)


#avgdf$Category[which(avgdf$Category=="Pursuit diving")]<-"Underwater pursuit"
#avgdf$Category[which(avgdf$Category=="Surface foraging")]<-"Surface"
##avgdf$Category<-as.character(avgdf$Category)
#avgdf$Category<-as.factor(avgdf$Category)
#avgdf$Category<-relevel(avgdf$Category, ref = "Terrestrial")

#avgdf$plungeT<-as.character(avgdf$plungedistinct)
#avgdf$plungeT[which(is.na(avgdf$plungedistinct))]<-"Terrestrial"
