
modellist<- paste(pgls_todo_hm,"*plungedistinct")
pgls_models_list<-lapply(modellist,pgls_models)#run pgls

#make list of dataframes with the PGLS outputs. 
tbllist_HM_times_ecol<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_HM_times_ecol[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_HM_times_ecol[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_HM_times_ecol[[i]]$Model<-modellist[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_HM_times_ecol[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_HM_times_ecol[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_HM_times_ecol[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_HM_times_ecol[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_HM_times_ecol[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
}

for(i in seq_along(tbllist_HM_times_ecol)){
  tbllist_HM_times_ecol[[i]]$Coefficients<-row.names(tbllist_HM_times_ecol[[i]])
  tbllist_HM_times_ecol[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_HM_times_ecol[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function 
  character_cols<-unlist(lapply(tbllist_HM_times_ecol[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_HM_times_ecol[[i]], is.numeric))# Identify numeric columns
  tbllist_HM_times_ecol[[i]]<-cbind(tbllist_HM_times_ecol[[i]][,which(character_cols)],signif(tbllist_HM_times_ecol[[i]][,which(numeric_cols)], digits = 2))
  colnames(tbllist_HM_times_ecol[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
  row.names(tbllist_HM_times_ecol[[i]])<-c()#remove row names
  print(tbllist_HM_times_ecol[[i]])
}