names(longdfplotting)
names(residseach)

mediandata<-longdfplotting %>% group_by(order,earmeasuresresid) %>% summarise(M = median(earmeasureval, na.rm = T))
mediandata<-as.data.frame(mediandata)
names(mediandata)

longdfplotting$cat<-paste0(longdfplotting$order,longdfplotting$earmeasuresresid)
mediandata$cat<-paste0(mediandata$order,mediandata$earmeasuresresid)
#match(mediandata$cat,longdfplotting$cat)
#match(longdfplotting$cat,mediandata$cat)
longdfplotting$median<-mediandata[match(longdfplotting$cat,mediandata$cat),"M"]



