# Tests the code on stocks

stockVector = list()
BestPrediction1 = vector()
preds = vector()

stockVector[[1]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\IR.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[2]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\ABC.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[3]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\ABM.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[4]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\ADC.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[5]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\ADM.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[6]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\AF.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[7]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\ADX.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[8]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\AFL.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[9]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\NOC.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[10]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\COP.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[11]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\TYC.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[12]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\Y.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[13]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\JAH.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[14]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\MAR.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[15]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\NE.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[16]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\NOV.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[17]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\WMB.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[18]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\MUR.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[19]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\AB.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[20]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\VLO.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[21]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\BAM.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[22]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\VNO.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[23]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\OKE.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[24]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\IP.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[25]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\IRM.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[26]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\CIG.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[27]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\BCS.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[28]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\TR.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[29]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\TKR.csv", sep=",", header=TRUE,as.is = TRUE)
stockVector[[30]] = read.table("C:\\Users\\Akarsh\\Desktop\\ASU\\input\\VGR.csv", sep=",", header=TRUE,as.is = TRUE)


  for(i in 1:length(stockVector))
 {
   cat("\n predicting stock number: ",i) ;
   train = stockVector[[i]]$Close[which(stockVector[[i]]$Date==max(stockVector[[i]]$Date))]
   #Calling the function for the predictions 
   result = BestPrediction(stockVector[[i]],i)
   name = paste('result', toString(i) ,'.csv',sep='')
   #Writing the data frame into csv file
   write.csv(result,file=paste("C:\\Users\\Akarsh\\Desktop\\ASU\\output\\",name,sep=''),col.names=TRUE,row.names=FALSE)
 }



