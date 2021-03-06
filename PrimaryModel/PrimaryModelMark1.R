#####################
# PrimaryModelMark1 #
#####################

#load quantmod and other packages
library(quantmod)

# import and set data
<<<<<<< HEAD
getSymbols('GS', src='yahoo', from='2014-01-01', to=Sys.Date()) #data gives Open, High, Low, Close, Volume, Adjusted
=======
getSymbols('GS', src='yahoo', from='2014-1-01', to=Sys.Date()) #data gives Open, High, Low, Close, Volume, Adjusted
>>>>>>> refs/remotes/keithwoj/master

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
<<<<<<< HEAD
  # configure plots
  par(bg='black', mfrow=c(2,1),
      col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
=======
  # configure plots  
  par(bg='black', mfrow=c(2,1),
      col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  # set up and plot
>>>>>>> refs/remotes/keithwoj/master
  plot(dataPlot,type='l',col='red',fg='white',
       main=paste(shortTerm,'- day Weighted Moving Average'),
       xlab='Day',ylab='Price')
  lines(shortWMA,col='green')
  
  plot(dataPlot,type='l',col='red',fg='white',
       main= paste(longTerm,'- day Weighted Moving Average'),
       xlab='Day',ylab='Price')
  lines(longWMA,col='green')
<<<<<<< HEAD
=======
  
>>>>>>> refs/remotes/keithwoj/master
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

<<<<<<< HEAD
  # plot the data and Moving Averages
  
  # it wasn't happy about this function
  # so it was removed
  # -Shane
  
  #plotMAs <- function(x[,k], n_data, WMA_n, m_data, WMA_m)
=======
  # plot the data and Moving Averages  
  plotMAs(x[,k], n_day, WMA_n, m_day, WMA_m)
>>>>>>> refs/remotes/keithwoj/master
  
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
<<<<<<< HEAD
=======
#
# Shane, this sounds like a good idea. This will also help us later on when we
# are looking back at the code and trying to figure out what we did and why
# we did. Let's plan on changing this the next time we meet. Otherwise, I may
# get to it over Thanksgiving break.
# -Russ
>>>>>>> refs/remotes/keithwoj/master

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

<<<<<<< HEAD
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
=======
#############################################################
# Function: Give Prediction Crossover of MAs (gPredCrossMA) #
# Input: mAvgsAll(x) -- we'll use the two WMA cols          #
# Return: "sell" "buy" "wait"                               #
# Special: Will only return a single prediction since it    #
#    compares two MAs there is no way to do more than one   #
#    prediction without providing another input matrix with #
#    different MAs                                          #
#############################################################
gPredCrossMA <- function(x){
  len <- length(x[,1])
  # check if short or long peroid is greater
  if(x[(len-1),2]>x[(len-1),3]){ #condition for short period being greater
    if(x[len,2]>x[len,3]){ #nothing has changed, so wait
      print("Prediction from cross: wait")
    } else { #shortMA has dipped below longMA, so sell
      print("Prediction from cross: sell")
    }
  } else{
    if(x[len,2]<x[len,3]){ #nothing has changed, so wait
      print("Prediction from cross: wait")
    } else { #shortMA has gone above longMA, so buy
      print("Prediction from cross: buy")
    }
  }
}

######################################################
# Function: Give Prediction MA/MA (gPredCrossMASell) #
# Input: mAvgsAll(x) -- we'll use the data and WMA   #
# Return: true value for sell only                   #
######################################################
gPredCrossMASell <- function(x){
  len <- length(x[,1])
  if(x[(len-1),2]>x[(len-1),3]){ #condition for short period being greater
    if(x[len,2]>x[len,3]){ #nothing has changed, so wait
      return(FALSE)
    } else { #shortMA has dipped below longMA, so sell
      return(TRUE)
    }
  } else{ #if shortMA starts below longMA we would never sell
    return(FALSE)
  }
}

#####################################################
# Function: Give Prediction MA/MA (gPredCrossMABuy) #
# Input: mAvgsAll(x) -- we'll use the data and WMA  #
# Return: true value for buy only                   #
#####################################################
gPredCrossMABuy <- function(x){
  len <- length(x[,1])
  if(x[(len-1),2]<x[(len-1),3]){ #condition for short period being greater
    if(x[len,2]<x[len,3]){ #nothing has changed, so wait
      return(FALSE)
    } else { #shortMA has gone above longMA, so buy
      return(TRUE)
    }
  } else{ # if shortMA starts below longMA we would never buy
    return(FALSE)
  } 
}

####################################################
# Function: linear regression (lreg)               #
# Input: a matrix (Will likely use mAvgsAll)       #
# Return: slope and intercept                      #
# Special: slope seems more significant            #
####################################################
lreg <- function(x, timeConsidered){ #timeConsidered is the nth most recent data points
  # set up fillable matrix
  dataMatrix <- matrix(c(c(seq(timeConsidered)),c(seq(timeConsidered))), ncol=2)
  for(i in 1:timeConsidered){
    jumpValue <- (length(x)-timeConsidered + i)
    dataMatrix[i,1] <- jumpValue
    dataMatrix[i,2] <- x[jumpValue]
  }
  # calculate correlation
  r <- cor(dataMatrix[,1], dataMatrix[,2])
  # calculate standard deviation
  sx <- sd(dataMatrix[,1])
  sy <- sd(dataMatrix[,2])
  # calculate means
  xbar <- mean(dataMatrix[,1])
  ybar <- mean(dataMatrix[,2])
  # calculate slope of least squares regression lin
  slope <- r*(sy/sx)
  # calcuclate y-intercept of least squares regression line
  return(slope)
}

####################################################
# Function: find profit of Data|MA (fProfitDataMA) #
# Input: mAvgsAll(x) -- we'll use the data and WMA #
#         will need "buy" "sell" points            #
# Return: ROI and Profit Summary                   #
####################################################
fProfitDataMA <- function(x, timeShort, timeLong){
>>>>>>> refs/remotes/keithwoj/master
   buyAmt      = 0 # amount a unit of stock was purchased at
   sellAmt     = 0 # amount a unit of stock sold for
   profit      = 0 # money made or lost
   totalProfit = 0 # tracks total profit
   totalSpent  = 0 # tracks toatl buy amounts
<<<<<<< HEAD
   
   for (i in (timeShort+1):length(x[,1])){
     xShort <- x[-(i+1):-length(x[,1]),]
     
     # we don't need to print these for every data point
     # -Shane
    
     #print("...")
     #print(gPredDataMABuy(xShort))
     #print(gPredDataMASell(xShort))
     #print(i==length(x[,1]))
     #print("...")
     
     if(gPredDataMABuy(xShort)){
       buyAmt  = x[i,1]
       # keep track of total amount spent, only changes with a buy
       totalSpent  = buyAmt + totalSpent
     } else if(gPredDataMASell(xShort)){
=======
   stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
   
   for (i in (timeShort+1):length(x[,1])){
     xShort <- x[-(i+1):-length(x[,1]),]
     #print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
     #print(i)
     #print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
     #print('buy Amt')
     #print(buyAmt)
     #print('sell Amt')
     #print(sellAmt)
     #print('total profit')
     #print(totalProfit)
     #print('total spent')
     #print(totalSpent)
     #print('stock on')
     #print(stockOn)
     #print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
     
     if(gPredDataMABuy(xShort) && stockOn==0){
       #print("BUY!!!") # for debugging
       buyAmt  = x[i,1]
       # keep track of total amount spent, only changes with a buy
       totalSpent  = buyAmt + totalSpent
       stockOn = 1 # 1 indicates stock
     } else if(gPredDataMASell(xShort) && stockOn==1 && buyAmt<x[i,1]){
       #print("SELL!!!") # for debugging
>>>>>>> refs/remotes/keithwoj/master
       sellAmt = x[i,1]
       profit  = sellAmt-buyAmt
       # keep track of total profit, only changes with a sell
       totalProfit = profit + totalProfit 
<<<<<<< HEAD
=======
       stockOn = 0 # restart stock count
>>>>>>> refs/remotes/keithwoj/master
     } else if(i == length(x[,1])){
       # if we are still waiting to sell when period ends, we sell for current amount
       if (x[i,2]<buyAmt){ ###SPECIAL CONDITION FOR SHORT###
         totalProfit = totalProfit + (x[i,1]-buyAmt)
       } else{
         # nothing is needed as amount has been sold
       }
       message("NOTE: final balance sold to calculate ROI")
     } else{
<<<<<<< HEAD
=======
       #print("DO NOTHING!!!") # for debugging
>>>>>>> refs/remotes/keithwoj/master
       # nothing else needs to happen because we are waiting for a sell prediction
     }
   }
   message(timeShort,"-day TOTALS:")
   message("Total Spent: ", totalSpent)
   message("Total Profit: ", totalProfit)
   message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
<<<<<<< HEAD
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
=======
   
   # clear variables and start again, we may want to consider changing to a single MA function style
   # insted of the two MA function style we started with
   # -Russ
   
   buyAmt      = 0 # amount a unit of stock was purchased at
   sellAmt     = 0 # amount a unit of stock sold for
   profit      = 0 # money made or lost
   totalProfit = 0 # tracks total profit
   totalSpent  = 0 # tracks toatl buy amounts
   stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
   
   for (i in (timeLong+1):length(x[,1])){
     xLong <- x[-(i+1):-length(x[,1]),]
     
     if(gPredDataMABuy(xLong) && stockOn==0){
       buyAmt  = x[i,1]
       # keep track of total amount spent, only changes with a buy
       totalSpent  = buyAmt + totalSpent
       stockOn = 1 # 1 indicates stock
     } else if(gPredDataMASell(xLong) && stockOn==1 && buyAmt<x[i,1]){
       sellAmt = x[i,1]
       profit  = sellAmt-buyAmt
       # keep track of total profit, only changes with a sell
       totalProfit = profit + totalProfit 
       stockOn = 0 # restart stock count
     } else if(i == length(x[,1])){
       # if we are still waiting to sell when period ends, we sell for current amount
       if (x[i,3]<buyAmt){ ###SPECIAL CONDITION FOR LONG###
         totalProfit = totalProfit + (x[i,1]-buyAmt)
       } else{
         # nothing is needed as amount has been sold
       }
       message("NOTE: final balance sold to calculate ROI")
     } else{
       # nothing else needs to happen because we are waiting for a sell prediction
     }
   }
   message(timeLong,"-day TOTALS:")
   message("Total Spent: ", totalSpent)
   message("Total Profit: ", totalProfit)
   message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
   
   # for testing the end of matrix
   #print(x[-1:-(length(x[,1])-1),])
   #print(length(x[,1]))
}

####################################################
# Function: find profit of MA|MA (fProfitCrossMA)  #
# Input: mAvgsAll(x) -- we'll use the data and WMA #
#         will need "buy" "sell" points            #
# Return: ROI and Profit Summary                   #
####################################################
fProfitCrossMA <- function(x, timeShort, timeLong){
  buyAmt      = 0 # amount a unit of stock was purchased at
  sellAmt     = 0 # amount a unit of stock sold for
  profit      = 0 # money made or lost
  totalProfit = 0 # tracks total profit
  totalSpent  = 0 # tracks toatl buy amounts
  stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
  
  for (i in (timeLong+1):length(x[,1])){# must use time long since it does not exist when time short does!
    xShort <- x[-(i+1):-length(x[,1]),]
    if(gPredCrossMABuy(xShort) && stockOn == 0){
      buyAmt  = x[i,1]
      # keep track of total amount spent, only changes with a buy
      sellAmt = buyAmt #we set the sellAmt=buyAmt so we do not buy again until after sell
      totalSpent  = buyAmt + totalSpent
      stockOn = 1
    } else if(gPredCrossMASell(xShort) && stockOn == 1){
      sellAmt = x[i,1]
      profit  = sellAmt-buyAmt
      # keep track of total profit, only changes with a sell
      totalProfit = profit + totalProfit 
      stockOn = 0
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
  message("MA SUMMARY & TOTALS:")
  message("Total Spent: ", totalSpent)
  message("Total Profit: ", totalProfit)
  message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
}

fProfitDataMA(mAvgsAll(X,45,80,.1,.9),45,80)
fProfitCrossMA(mAvgsAll(X,20,40,.1,.9),20,40)
#gPredDataMA(mAvgsAll(X,45,100,0,0))
#gPredCrossMA(mAvgsAll(X,45,100,0,0))
#gPredCrossMABuy(mAvgsAll(X,45,100,0,0))
#gPredCrossMASell(mAvgsAll(X,45,100,0,0))

#TESTING STUFF
#matrixTest <- matrix(c(1,1,1,1,1,1,1,1,1,1), ncol=1)
#for(i in (length(matrixTest)-3):length(matrixTest)){
#  matrixTest[i]=i
#  print(matrixTest)
#}
#MatrixTest <- matrix(c(2,3,2,8,9,5,6,0,1,11),ncol=1)
#shortTest <- MatrixTest[-1:(-5+1),]
#
#lreg(MatrixTest,3)

#DONE!
>>>>>>> refs/remotes/keithwoj/master
