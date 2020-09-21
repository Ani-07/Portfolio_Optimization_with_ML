
###########################################################################

V1 = ML_database[which(ML_database$Cat == 1),]
V2 = ML_database[which(ML_database$Cat == 2),]
V3 = ML_database[which(ML_database$Cat == 3),]
V4 = ML_database[which(ML_database$Cat == 4),]
V5 = ML_database[which(ML_database$Cat == 5),]



Normalize=function(Data){
  NewData=Data
  
  for(i in 1:length(colnames(NewData))) {
    if(class(NewData[,i]) == "numeric" || class(NewData[,i]) == "integer") {
      NewData[,i] <- as.vector(scale(NewData[,i])) }
  }
  return(NewData)
}



ret_norm <- Normalize(return_forecast[,4:13])



