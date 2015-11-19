#####################
# PrimaryModelMark1 #
#####################

#load quantmod and other packages
library(quantmod)

# import and set data
getSymbols('GS', src='yahoo', from='2014-01-01', to=Sys.Date()) #data gives Open, High, Low, Close, Volume, Adjusted

#check x input
X <- matrix(cbind(GS), ncol = 6)

##########################################################
# Function: Weighted Averages (WMA)                      #
# Input: SMAweight, EMAweight, dataRaw , timeperiod      #
# Return: WMA or GMA                                     #
##########################################################
WMA <- function(SMAweight=0, EMAweight=0, dataRaw, timePeriod){
  # calculate SMA and EMA
  SMA = SMA(dataRaw, timePeriod)
  EMA = EMA(dataRaw, timePeriod)
  
  #find distance between averages and data
  dist_SMA = dataRaw - SMA
  dist_EMA = dataRaw - EMA
  
  # set up WMA fillable
  # if weights are both zero, function will find GMA
  # if weights do not sum to one, function will print a note and will defalut to GMA
  WMA = c()
  
  if (SMAweight==0 && EMAweight==0){
    for(i in timePeriod:length(dataRaw)){
      if(abs(dist_SMA[i]) > abs(dist_EMA[i])){
        WMA[i] = EMA[i]
      }else{WMA[i] = SMA[i]}
    } 
  } else if ((SMAweight + EMAweight) != 1){
      print("Weighted values do not sum to one.Program has defaulted to finding GMA instead of WMA.")
  } else {
    for(i in timePeriod:length(dataRaw)){
      WMA[i] = SMAweight*SMA[i] + EMAweight*EMA[i]
    } 
  } 
   return (WMA)
}

###########################################################
# Function: Plot Moving Averages (plotMAs)                #
# Input: dataPlot, times and WMAs for both short and long #
# Return: plots and null                                  #
###########################################################
plotMAs <- function(dataPlot, shortTerm, shortWMA, longTerm, longWMA){
  # configure plots
  par(bg='black', mfrow=c(2,1),
      col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  plot(dataPlot,type='l',col='red',fg='white',
       main=paste(shortTerm,'- day Weighted Moving Average'),
       xlab='Day',ylab='Price')
  lines(shortWMA,col='green')
  
  plot(dataPlot,type='l',col='red',fg='white',
       main= paste(longTerm,'- day Weighted Moving Average'),
       xlab='Day',ylab='Price')
  lines(longWMA,col='green')
  return()
}

##########################################################
# Function: Moving Averages All (mAvgsAll)               #
# Input: x <- getSymbol matrix + nday + mday + "rest"    #
# Return: mAvgsData matrix x + SMAn + SMAm + EMAn + EMAm #
##########################################################
mAvgsAll <- function(x,n_day, m_day, weightSMA, weightEMA, open = TRUE, high = FALSE, low = FALSE, close = FALSE, adjusted = FALSE){ #x needs to be the data and time indicators
  if (open){
    k = 1
    
    #Calculate WMA (or GMA)
    WMA_n = WMA(weightSMA, weightEMA, x[,k], n_day)
    WMA_m = WMA(weightSMA, weightEMA, x[,k], m_day)
    
  } else if (high){
    k = 2
    
    #Calculate WMA (or GMA)
    WMA_n = WMA(weightSMA, weightEMA, x[,k], n_day)
    WMA_m = WMA(weightSMA, weightEMA, x[,k], m_day)
    
  } else if (low){
    k = 3
    
    #Calculate WMA (or GMA)
    WMA_n = WMA(weightSMA, weightEMA, x[,k], n_day)
    WMA_m = WMA(weightSMA, weightEMA, x[,k], m_day)
    
  } else if (close){
    k = 4
    
    #Calculate WMA (or GMA)
    WMA_n = WMA(weightSMA, weightEMA, x[,k], n_day)
    WMA_m = WMA(weightSMA, weightEMA, x[,k], m_day)
    
  } else if (adjusted){
    k = 6
    
    #Calculate WMA (or GMA)
    WMA_n = WMA(weightSMA, weightEMA, x[,k], n_day)
    WMA_m = WMA(weightSMA, weightEMA, x[,k], m_day)
    
  } else {
    print ("check your true/false entrys")
  }

  # plot the data and Moving Averages
  plotMAs <- function(x[,k], n_data, WMA_n, m_data, WMA_m)
  
  #set up fillable matrix
  mAvgsAllData <- matrix(cbind(x[,k], WMA_n, WMA_m), ncol=3)
  return(mAvgsAllData)
}

####################################################
# Function: Give Prediction Data/MAs (gPredDataMA) #
# Input: mAvgsAll(x) -- we'll use the data and GMA #
# Return: "sell" "buy" "wait"                      #
####################################################

# we might want to change the argument for this function
# since X is already defined as the original OHLC data matrix,
# using lowercase x could be confusing.
#
# It wants data of the form [price,WMA_n,WMA_m]; maybe use something like:
# gPredDataMA = function(WMAdata){...}
# the same can be said of gPredDataMABuy, gPredDataMASell, gPredCross, and fProfit
# -Shane

gPredDataMA <- function(x){
  len <- length(x[,1])
  # case of short term, check if data above or below GMA
  if(x[(len-1),1]>x[(len-1),2]){
    if(x[len,1]>x[(len-1),2]){
      print("Prediction from Short: wait")
    } else {
      print("Prediction from Short: sell")
    }
  } else {
    if(x[len,1]<x[(len-1),2]){
      print("Prediction from short: wait")
    } else {
      print("Prediction from short: buy")
    }
  }
  # case of long term, check if data above or below GMA
  if(x[(len-1),1]>x[(len-1),3]){
    if(x[len,1]>x[(len-1),3]){
      print("Prediction from long: wait")
    } else {
      print("Prediction from long: sell")
    }
  } else {
    if(x[len,1]<x[(len-1),3]){
      print("Prediction from long: wait")
    } else {
      print("Prediction from long: buy")
    }
  }
}

####################################################
# Function: Give Prediction Data/MAs (gPredDataMABuy) #
# Input: mAvgsAll(x) -- we'll use the data and GMA #
# Return: true value for buy only                  #
####################################################
gPredDataMABuy <- function(x){
  len <- length(x[,1])
  # case of short term, check if data above or below GMA
  if(x[(len-1),1]>x[(len-1),2]){
    if(x[len,1]>x[(len-1),2]){
      return (FALSE)
    } else {
      return (FALSE)
    }
  } else {
    if(x[len,1]<x[(len-1),2]){
      return (FALSE)
    } else {
      return (TRUE)
    }
  }
}

####################################################
# Function: Give Prediction Data/MAs (gPredDataMASell) #
# Input: mAvgsAll(x) -- we'll use the data and GMA #
# Return: true value for sell only                  #
####################################################
gPredDataMASell <- function(x){
  len <- length(x[,1])
  # case of short term, check if data above or below GMA
  if(x[(len-1),1]>x[(len-1),2]){
    if(x[len,1]>x[(len-1),2]){
      return (FALSE)
    } else {
      return (TRUE)
    }
  } else {
    if(x[len,1]<x[(len-1),2]){
      return (FALSE)
    } else {
      return (FALSE)
    }
  }
}

####################################################
# Function: Give Prediction Crossover (gPredCross) #
# Input: mAvgsAll(x) -- we'll use the data and GMA #
# Return: "sell" "buy" "wait"                      #
####################################################
#gPredCross <- function(x){
#  len <- length(x[,1])
#  # case of short term, check if data above or below GMA
#  if(x[(len-1),2]>x[(len-1),3]){
#    
#  }
#}

####################################################
# Function: find profit (fProfit)                  #
# Input: mAvgsAll(x) -- we'll use the data and WMA #
#         will need "buy" "sell" points            #
# Return: "sell" "buy" "wait"                      #
####################################################
fProfit <- function(x, timeShort, timeLong){
   buyAmt      = 0 # amount a unit of stock was purchased at
   sellAmt     = 0 # amount a unit of stock sold for
   profit      = 0 # money made or lost
   totalProfit = 0 # tracks total profit
   totalSpent  = 0 # tracks toatl buy amounts
   
   for (i in (timeShort+1):length(x[,1])){
     xShort <- x[-(i+1):-length(x[,1]),]
     print("...")
     print(gPredDataMABuy(xShort))
     print(gPredDataMASell(xShort))
     print(i==length(x[,1]))
     print("...")
     if(gPredDataMABuy(xShort)){
       buyAmt  = x[i,1]
       # keep track of total amount spent, only changes with a buy
       totalSpent  = buyAmt + totalSpent
     } else if(gPredDataMASell(xShort)){
       sellAmt = x[i,1]
       profit  = sellAmt-buyAmt
       # keep track of total profit, only changes with a sell
       totalProfit = profit + totalProfit 
     } else if(i == length(x[,1])){
       # if we are still waiting to sell when period ends, we sell for current amount
       if (x[i,2]<buyAmt){ ###SPECIAL CONDITION FOR SHORT###
         totalProfit = totalProfit + (x[i,1]-buyAmt)
       } else{
         # nothing is needed as amount has been sold
       }
       message("NOTE: final balance sold to calculate ROI")
     } else{
       # nothing else needs to happen because we are waiting for a sell prediction
     }
   }
   message(timeShort,"-day TOTALS:")
   message("Total Spent: ", totalSpent)
   message("Total Profit: ", totalProfit)
   message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
   print(x[-1:-(length(x[,1])-1),])
   print(length(x[,1]))
}


fProfit(mAvgsAll(X,100,100,0,0),100,100)
gPredDataMa(mAvgsAll(X,30,50,0,0))

for (i in 2:5){
  print(i)
  if (i==5){
    print("bingo")
  }
}
