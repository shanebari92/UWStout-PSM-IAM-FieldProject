#########################
# PreliminaryModelMark1 #
#########################

#load quantmod and other packages
library(quantmod)

# import and set data
getSymbols('GS', src='yahoo', from='2014-01-01', to=Sys.Date()) #data gives Open, High, Low, Close, Volume, Adjusted

GSi
#############################################################################################
### Problem: setting up data is constrained to what company we are using 'getSymbols' for ###
#############################################################################################

# create data matrix: open, high, low, close
data = matrix(cbind(GS[,1],GS[,2],GS[,3],GS[,4]),ncol=4)

# length of data
size = length(GS[,1]) #could be any of the columns
# calculate moving averages based on opening price
#

# specify number of days lag
n_day = 20
m_day = 200

# simple moving average
SMA_n = SMA(data[,1],n=n_day)
SMA_m = SMA(data[,1],n=m_day)

# exponential moving average
EMA_n = EMA(data[,1],n=n_day)
EMA_m = EMA(data[,1],n=m_day)

#check x input
X <- matrix(cbind(GS,20,50), ncol = 8)
X
##########################################################
# Function: Moving Averages (mAvgs)                      #
# Input: x <- getSymbol matrix + nday + mday             #
# Return: mAvgsData matrix x + SMAn + SMAm + EMAn + EMAm #
##########################################################

mAvgs <- function(x){ #x needs to be the data and time indicators
  #Calculate SMA
  SMA_n = SMA(x[,1], n = x[1,7])
  SMA_m = SMA(x[,1], m = x[1,8])
  
  #Calculate EMA
  EMA_n = EMA(x[,1],n = x[1,7])
  EMA_m = EMA(x[,1],m = x[1,8])
  
  #set up fillable matrix
  mAvgsData <- matrix(cbind(x, SMA_n, SMA_m, EMA_n, EMA_m), ncol=12)
  return(mAvgsData)
}

##########################################################
# Function: Moving Averages All (mAvgsAll)               #
# Input: x <- getSymbol matrix + nday + mday             #
# Return: mAvgsData matrix x + SMAn + SMAm + EMAn + EMAm #
##########################################################

mAvgsAll <- function(x,open = TRUE, high = FALSE, low = FALSE, close = FALSE, adjusted = FALSE){ #x needs to be the data and time indicators
  if (open){
    k = 1
    #Calculate SMA
    SMA_n = SMA(x[,k], x[1,7])
    SMA_m = SMA(x[,k], x[1,8])
    
    #Calculate EMA
    EMA_n = EMA(x[,k], x[1,7])
    EMA_m = EMA(x[,k], x[1,8]) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (high){
    k = 2
    #Calculate SMA
    SMA_n = SMA(x[,k], x[1,7])
    SMA_m = SMA(x[,k], x[1,8])
    
    #Calculate EMA
    EMA_n = EMA(x[,k], x[1,7])
    EMA_m = EMA(x[,k], x[1,8]) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (low){
    k = 3
    #Calculate SMA
    SMA_n = SMA(x[,k], x[1,7])
    SMA_m = SMA(x[,k], x[1,8])
    
    #Calculate EMA
    EMA_n = EMA(x[,k], x[1,7])
    EMA_m = EMA(x[,k], x[1,8]) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (close){
    k = 4
    #Calculate SMA
    SMA_n = SMA(x[,k], x[1,7])
    SMA_m = SMA(x[,k], x[1,8])
    
    #Calculate EMA
    EMA_n = EMA(x[,k], x[1,7])
    EMA_m = EMA(x[,k], x[1,8]) 
    
    #find distance between averages and data
    dist_SMA_n = x[,k] - SMA_n
    dist_SMA_m = x[,k] - SMA_m
    dist_EMA_n = x[,k] - EMA_n
    dist_EMA_m = x[,k] - EMA_m
    
  } else if (adjusted){
    k = 6
    #Calculate SMA
    SMA_n = SMA(x[,k], x[1,7])
    SMA_m = SMA(x[,k], x[1,8])
    
    #Calculate EMA
    EMA_n = EMA(x[,k], x[1,7])
    EMA_m = EMA(x[,k], x[1,8]) 
    
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
  for(i in x[1,7]:length(x[,1])){
    if(abs(dist_SMA_n[i]) > abs(dist_EMA_n[i])){GMA_n[i] = EMA_n[i]}
    else{GMA_n[i] = SMA_n[i]}
  }  
  
  for(j in x[1,8]:length(x[,1])){
    if(abs(dist_SMA_m[j]) > abs(dist_EMA_m[j])){GMA_m[j] = EMA_m[j]}
    else{GMA_m[j] = SMA_m[j]}
  }  
    
  # configure plots
  par(bg='black', mfrow=c(2,1),
      col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  plot(x[,k],type='l',col='red',fg='white',
       main=paste(x[1,7],'-day General Moving Average'),
       xlab='Day',ylab='Price')
  lines(GMA_n,col='green')
  
  plot(x[,k],type='l',col='red',fg='white',
       main= paste(x[1,8],'-day General Moving Average'),
       xlab='Day',ylab='Price')
  lines(GMA_m,col='green')
  
  #set up fillable matrix
  mAvgsAllData <- matrix(cbind(x[,k], GMA_n, GMA_m), ncol=3)
  return(mAvgsAllData)
}

mAvgsAll(X)

# test to make sure averages are working, OPEN
MAVGS <- mAvgsAll(X)
MAVGS[,9] -SMA(X[,1],20)
MAVGS[,10]-SMA(X[,1],100)
MAVGS[,11]-EMA(X[,1],20)
MAVGS[,12]-EMA(X[,1],100)

# test HIGH
MAVGS <- mAvgsAll(X,FALSE,TRUE,FALSE,FALSE,FALSE)
MAVGS[,9] -SMA(X[,2],20)
MAVGS[,10]-SMA(X[,2],100)
MAVGS[,11]-EMA(X[,2],20)
MAVGS[,12]-EMA(X[,2],100)

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
gPred(mAvgsAll(X))
