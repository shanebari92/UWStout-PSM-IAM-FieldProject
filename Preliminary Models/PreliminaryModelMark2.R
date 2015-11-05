#########################
# PreliminaryModelMark2 #
#########################

#load quantmod and other packages
library(quantmod)

# import and set data
getSymbols('GS', src='yahoo', from='2014-01-01', to=Sys.Date()) #data gives Open, High, Low, Close, Volume, Adjusted

#check x input
X <- matrix(cbind(GS), ncol = 6)

##########################################################
# Function: Moving Averages All (mAvgsAll)               #
# Input: x <- getSymbol matrix + nday + mday             #
# Return: mAvgsData matrix x + SMAn + SMAm + EMAn + EMAm #
##########################################################

mAvgsAll <- function(x,n_day, m_day, open = TRUE, high = FALSE, low = FALSE, close = FALSE, adjusted = FALSE){ #x needs to be the data and time indicators
  if (open){
    k = 1
    #Calculate SMA
    SMA_n = SMA(x[,k], n_day)
    SMA_m = SMA(x[,k], m_day)
    
    #Calculate EMA
    EMA_n = EMA(x[,k], n_day)
    EMA_m = EMA(x[,k], m_day) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (high){
    k = 2
    #Calculate SMA
    SMA_n = SMA(x[,k], n_day)
    SMA_m = SMA(x[,k], m_day)
    
    #Calculate EMA
    EMA_n = EMA(x[,k], n_day)
    EMA_m = EMA(x[,k], m_day) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (low){
    k = 3
    #Calculate SMA
    SMA_n = SMA(x[,k], n_day)
    SMA_m = SMA(x[,k], m_day)
    
    #Calculate EMA
    EMA_n = EMA(x[,k], n_day)
    EMA_m = EMA(x[,k], m_day) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (close){
    k = 4
    #Calculate SMA
    SMA_n = SMA(x[,k], n_day)
    SMA_m = SMA(x[,k], m_day)
    
    #Calculate EMA
    EMA_n = EMA(x[,k], n_day)
    EMA_m = EMA(x[,k], m_day) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (adjusted){
    k = 6
    #Calculate SMA
    SMA_n = SMA(x[,k], n_day)
    SMA_m = SMA(x[,k], m_day)
    
    #Calculate EMA
    EMA_n = EMA(x[,k], n_day)
    EMA_m = EMA(x[,k], m_day) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else {
    print ("check your true/false entrys")
  }
  
  # creat fillable GMA
  GMA_n = c()
  GMA_m = c()
  
  # calc GMA
  for(i in n_day:length(x[,1])){
    if(abs(dist_SMA_n[i]) > abs(dist_EMA_n[i])){GMA_n[i] = EMA_n[i]}
    else{GMA_n[i] = SMA_n[i]}
  }  
  
  for(j in m_day:length(x[,1])){
    if(abs(dist_SMA_m[j]) > abs(dist_EMA_m[j])){GMA_m[j] = EMA_m[j]}
    else{GMA_m[j] = SMA_m[j]}
  }  
  
  # configure plots
  par(bg='black', mfrow=c(2,1),
      col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  plot(x[,k],type='l',col='red',fg='white',
       main=paste(n_day,'- day General Moving Average'),
       xlab='Day',ylab='Price')
  lines(GMA_n,col='green')
  
  plot(x[,k],type='l',col='red',fg='white',
       main= paste(m_day,'- day General Moving Average'),
       xlab='Day',ylab='Price')
  lines(GMA_m,col='green')
  
  #set up fillable matrix
  mAvgsAllData <- matrix(cbind(x[,k], GMA_n, GMA_m), ncol=3)
  return(mAvgsAllData)
}

####################################################
# Function: Give Prediction (gPred)                #
# Input: mAvgsAll(x) -- we'll use the data and GMA #
# Return: "sell" "buy" "wait"                      #
####################################################
gPred <- function(x){
  len <- length(x[,1])
  # case of short term, check if data above or below GMA
  if(x[(len-1),1]>x[(len-1),2]){
    if(x[len,1]>x[(len-1),2]){
      print("Prediction: wait")
    } else {
      print("Prediction: sell")
    }
  } else {
    if(x[len,1]<x[(len-1),2]){
      print("Prediction: wait")
    } else {
      print("Prediction: buy")
    }
  }
}

gPred(mAvgsAll(X,25,60))