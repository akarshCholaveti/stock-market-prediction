library(forecast)
library(fpp)
library(ggplot2)
require(graphics)

BestPrediction <- function(Stockadd, stockName)
{
  tsStock = ts(rev(Stockadd$Close),start=c(2000, 1),frequency=12)
  #Getting the train and test data
  train <- window(tsStock, end=2011)
  test <- window(tsStock, start=2012)
  
  mae = matrix(NA,7,length(test)+1)
  
  #Training the models with train data
  tryCatch({
  #HoltWinters
  HoltWinterStock_noGamma = HoltWinters(train,gamma=FALSE) 
  },error=function(a){
    HoltWinterStock_noGamma=0
  })
  
  tryCatch({
    #HoltWinters
    HoltWinterStock = HoltWinters(train)
  },error=function(a){
    HoltWinterStock=0
  })
  
  tryCatch({
    #Neural Networks
    NeuralNetworksStock=nnetar(train)
  },error=function(a){
    NeuralNetworksStock=0
  })
  
  tryCatch({
    #ARIMA
    ArimaStock1 = arima(train, order=c(15,3,3))
  },error=function(a){
    ArimaStock1=0
  })
  
  tryCatch({
    #ARIMA
    ArimaStock2 =  arima(train, order=c(1,0,0), list(order=c(2,1,0), period=12))
  },error=function(a){
    ArimaStock2=0
  })
  
  tryCatch({
    #Time series linear model
    LinearModelStock = tslm(train ~ trend + season, lambda=0)
  },error=function(a){
    LinearModelStock=0
  })
  
  tryCatch({
    #Season Trend
    SeasonTrendStock = stl(train,s.window="periodic")
  },error=function(a){
    SeasonTrendStock=0
  })
  
  #Predicting the Stocks
  tryCatch({
    predArimaStock1 =  window(forecast(ArimaStock1,h=39)$mean, start=2012)
  },error=function(a){
    predArimaStock1=0
  })
  
  tryCatch({
    predArimaStock2  =   window(forecast(ArimaStock2,h=39)$mean, start=2012)
  },error=function(a){
    predArimaStock2=0
  })
  
  tryCatch({
    predNeuralNetworksStock =  window(forecast(NeuralNetworksStock,h=39)$mean, start=2012)
  },error=function(a){
    predNeuralNetworksStock=0
  })
  
  tryCatch({
    predHoltWinterStock =  window(predict(HoltWinterStock,n.ahead=39), start=2012)
  },error=function(a){
    predHoltWinterStock=0
  })
  
  tryCatch({
    predHoltWinterStock_noGamma =  window(predict(HoltWinterStock_noGamma,n.ahead=39), start=2012)
  },error=function(a){
    predHoltWinterStock_noGamma=0
  })
  
  tryCatch({
    predSeasonTrendStock =  window(forecast(SeasonTrendStock, h=39)$mean, start=2012)
  },error=function(a){
    predSeasonTrendStock=0
  })
  
  tryCatch({
    predLinearModelStock = window(forecast(LinearModelStock, h=39)$mean , start=2012)
  },error=function(a){
    predLinearModelStock=0
  })
  
  
  

  #Calculating the absolute error  
  preds = matrix(NA,7,length(test)+1)
  
  for(i in 1:length(test))
  {
    tryCatch({
      mae[1,i] =abs(predArimaStock1[i]-test[i])
      preds[1,i] = predArimaStock1[i]
    },error=function(a){
      
    })
    
    tryCatch({
      mae[2,i] = abs(predArimaStock2[i]-test[i])
      preds[2,i] = predArimaStock2[i]
    },error=function(a){
      
    })
    
    tryCatch({
      mae[3,i] = abs(predNeuralNetworksStock[i]-test[i])
      preds[3,i] = predNeuralNetworksStock[i]
    },error=function(a){
      
    })
    
    tryCatch({
      mae[4,i] = abs(predHoltWinterStock[i]-test[i])
      preds[4,i] =predHoltWinterStock[i]
    },error=function(a){
      
    })
    
    tryCatch({
      mae[5,i] = abs(predHoltWinterStock_noGamma[i]-test[i])
      preds[5,i] =predHoltWinterStock_noGamma[i]
    },error=function(a){
      
    })
    
    tryCatch({
      mae[6,i] <- abs(predSeasonTrendStock[i]-test[i])
      preds[6,i] =predSeasonTrendStock[i]
    },error=function(a){
      
    })
    
    tryCatch({
      mae[7,i] <- abs(predLinearModelStock[i]-test[i]) 
      preds[7,i] =predLinearModelStock[i]
    },error=function(a){
      predLinearModelStock=0
    })
    
  }
  
  #Writing the Data into Dataframe
  
  resultDf = data.frame(c(NULL,NULL,NULL,NULL,NULL,NULL))
 
  
  for( i in 1:length(test))
  {
   
    index = which.min(mae[1:7,i])
    minmae = mae[index,i]
    if(index==2){
      val= predArimaStock2[i]
    }
    else if(index==3){
      val= predNeuralNetworksStock[i]
    }
    else if(index==4){
      val= predHoltWinterStock[i]
    }
    else if(index==5){
      val= predHoltWinterStock_noGamma[i]
    }
    else if(index==6){
      val= predSeasonTrendStock[i]
    }
    else if(index==7){
      val= predLinearModelStock[i]
    }
    original = test[i]
    val_p=(minmae/original)*100
    
    resultDf= rbind(resultDf,c(stockName,original,val,minmae,val_p,index))
  }
  colnames(resultDf) = c('stockname','originalprice','PredictedPrice','mae','percentage','model')
  #Returning the Data frame
  return (resultDf)

}