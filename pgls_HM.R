pgls_todo_hm<-pgls_todo_nogeomet[seq(2,length(pgls_todo_nogeomet),2)]

#model list with head mass as only independent variable
modellist<-pgls_todo_hm
pgls_models_list<-lapply(pgls_todo_hm,pgls_models)#run pgls

#make list of dataframes with the PGLS outputs. 
tbllist_HM<-list()
for (i in seq_along(pgls_models_list)){#change th 'Model' colume in this as appropriate
  tbllist_HM[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist_HM[[i]]$Adj_Rsquared<-summary(pgls_models_list[[i]])$'adj.r.squared'[1]#rsquared
  tbllist_HM[[i]]$Model<-modellist[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist_HM[[i]]$Lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
  tbllist_HM[[i]]$Fstat<-summary(pgls_models_list[[i]])$fstatistic[1]
  tbllist_HM[[i]]$Fstat_numdf<-summary(pgls_models_list[[i]])$fstatistic[2]
  tbllist_HM[[i]]$Fstat_dendf<-summary(pgls_models_list[[i]])$fstatistic[3]
  tbllist_HM[[i]]$AICc<-pgls_models_list[[i]]$aicc[1]
}

#organize the dataframe table (significant digist, remove redundant F stat & R squared)
for(i in seq_along(tbllist_HM)){
  tbllist_HM[[i]]$Coefficients<-row.names(tbllist_HM[[i]])
  tbllist_HM[[i]]$Coefficients<-gsub('[[:digit:]]+', '', tbllist_HM[[i]]$Coefficients)#regex to remove number automatically added during the loop
  #identify numeric cols and character cols to apply the significant digits function 
  character_cols<-unlist(lapply(tbllist_HM[[i]], is.character))
  numeric_cols <- unlist(lapply(tbllist_HM[[i]], is.numeric))# Identify numeric columns
  tbllist_HM[[i]]<-cbind(tbllist_HM[[i]][,which(character_cols)],signif(tbllist_HM[[i]][,which(numeric_cols)], digits = 2))
    colnames(tbllist_HM[[i]])[6]<-"P.val"#rename b/c flextable doesn't work will with the '>' sign
    row.names(tbllist_HM[[i]])<-c()#remove row names
  print(tbllist_HM[[i]])
}