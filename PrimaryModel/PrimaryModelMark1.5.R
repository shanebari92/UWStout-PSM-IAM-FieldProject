#####################
# PrimaryModelMark1 #
#####################

#load quantmod and other packages
library(quantmod)

# import and set data
#getSymbols('GS', src='yahoo', from='2014-1-01', to=Sys.Date()) #data gives Open, High, Low, Close, Volume, Adjusted
getSymbols('GS', src='yahoo', from='2014-1-01', to='2016-2-1')
#getSymbols('SPYV', src='yahoo', from='2014-1-01', to='2016-2-01')

#check x input
X <- matrix(cbind(GS), ncol = 6)

##########################################################
# Function: Weighted Averages (WMA)                      #
# Input: SMAweight, EMAweight, dataRaw , timeperiod      #
# Return: WMA or GMA                                     #
##########################################################
WMA <- function(SMAweight=0, EMAweight=0, dataRaw, timePeriod)
{
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
plotMAs <- function(dataPlot, shortTerm, shortWMA, longTerm, longWMA)
{
  # configure plots  
  par(bg='black', mfrow=c(2,1),
      col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  # set up and plot
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
mAvgsAll <- function(x,n_day, m_day, weightSMA, weightEMA, open = TRUE, high = FALSE, low = FALSE, close = FALSE, adjusted = FALSE)
{ #x needs to be the data and time indicators
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
  plotMAs(x[,k], n_day, WMA_n, m_day, WMA_m)
  
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
#
# Shane, this sounds like a good idea. This will also help us later on when we
# are looking back at the code and trying to figure out what we did and why
# we did. Let's plan on changing this the next time we meet. Otherwise, I may
# get to it over Thanksgiving break.
# -Russ

gPredDataMA <- function(x)
{
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
gPredDataMABuy <- function(x)
{
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
gPredDataMASell <- function(x)
{
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

#############################################################
# Function: Give Prediction Crossover of MAs (gPredCrossMA) #
# Input: mAvgsAll(x) -- we'll use the two WMA cols          #
# Return: "sell" "buy" "wait"                               #
# Special: Will only return a single prediction since it    #
#    compares two MAs there is no way to do more than one   #
#    prediction without providing another input matrix with #
#    different MAs                                          #
#############################################################
gPredCrossMA <- function(x)
{
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
gPredCrossMASell <- function(x)
{
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
gPredCrossMABuy <- function(x)
{
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
lreg <- function(x, timeConsidered)
{ #timeConsidered is the nth most recent data points
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
fProfitDataMA <- function(x, timeShort, timeLong)
{
  buyAmt      = 0 # amount a unit of stock was purchased at
  sellAmt     = 0 # amount a unit of stock sold for
  profit      = 0 # money made or lost
  totalProfit = 0 # tracks total profit
  totalSpent  = 0 # tracks total buy amounts
  stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
  heldAmt     = 0 # amount held, left over
  
  # used for plotting
  
  currentProfitShort = c()
  overallProfitShort = c()
  
  
  for (i in (timeShort+1):length(x[,1])){
    
    # used for plotting
    
    currentProfitShort[i] = profit
    overallProfitShort[i] = totalProfit
    
    xShort <- x[-(i+1):-length(x[,1]),]
    
    if(gPredDataMABuy(xShort) && stockOn==0){
      buyAmt  = x[i,1]
      # keep track of total amount spent, only changes with a buy
      totalSpent  = buyAmt + totalSpent
      stockOn = 1 # 1 indicates stock
      
    } else if(gPredDataMASell(xShort) && stockOn==1 && buyAmt<x[i,1]){
      sellAmt = x[i,1]
      profit  = sellAmt-buyAmt
      # keep track of total profit, only changes with a sell
      totalProfit = profit + totalProfit 
      stockOn = 0 # restart stock count
      
      
    } else if(i == length(x[,1])){
      # if we are still waiting to sell when period ends, we sell for current amount
      if (x[i,2]<buyAmt){ ###SPECIAL CONDITION FOR SHORT###
        totalProfit = totalProfit
        heldAmt = buyAmt
        
      } else{
        # nothing is needed as amount has been sold
      }
      #message("NOTE: final balance sold to calculate ROI")# line is no longer necessary as we say held amount
    } else{
      # nothing else needs to happen because we are waiting for a sell prediction
    }
  }
  message(timeShort,"-day TOTALS:")
  message("Total Spent: ", totalSpent)
  message("Amount Held: ", heldAmt)
  message("Total Profit: ", totalProfit)
  message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
  message()
  
  
  
  buyAmt      = 0 # amount a unit of stock was purchased at
  sellAmt     = 0 # amount a unit of stock sold for
  profit      = 0 # money made or lost
  totalProfit = 0 # tracks total profit
  totalSpent  = 0 # tracks toatl buy amounts
  stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
  heldAmt     = 0 # amount held, left over
  
  # used for plotting
  
  currentProfitLong = c()
  overallProfitLong = c()
  
  for (i in (timeLong+1):length(x[,1])){
    
    # used for plotting
    currentProfitLong[i] = profit
    overallProfitLong[i] = totalProfit
    
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
        totalProfit = totalProfit 
        heldAmt = buyAmt
        
      } else{
        # nothing is needed as amount has been sold
      }
      #message("NOTE: final balance sold to calculate ROI")# line is no longer necessary as we say held amount
    } else{
      # nothing else needs to happen because we are waiting for a sell prediction
    }
  }
  message(timeLong,"-day TOTALS:")
  message("Total Spent: ", totalSpent)
  message("Amount Held: ", heldAmt)
  message("Total Profit: ", totalProfit)
  message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
  message()
  
  
  
  # set up plots
  par(bg='black', mfrow=c(2,2),col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  # plot stuff
  plot(currentProfitShort,type='l',col='blue',fg='white',main=paste(timeShort, '- day WMA Current Profit'))
  
  plot(overallProfitShort,type='l',col='green',fg='white',main=paste(timeShort, '- day WMA Total Profit'))
  
  plot(currentProfitLong,type='l',col='blue',fg='white',main=paste(timeLong, '- day WMA Current Profit'))
  
  plot(overallProfitLong,type='l',col='green',fg='white',main=paste(timeLong, '- day WMA Total Profit'))  
  
  
}

####################################################
# Function: find profit of MA|MA (fProfitCrossMA)  #
# Input: mAvgsAll(x) -- we'll use the data and WMA #
#         will need "buy" "sell" points            #
# Return: ROI and Profit Summary                   #
####################################################
fProfitCrossMA <- function(x, timeShort, timeLong)
{
  buyAmt      = 0 # amount a unit of stock was purchased at
  sellAmt     = 0 # amount a unit of stock sold for
  profit      = 0 # money made or lost
  totalProfit = 0 # tracks total profit
  totalSpent  = 0 # tracks toatl buy amounts
  stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
  
  # for plotting
  currentProfit = c()
  overallProfit = c()  

  for (i in (timeLong+1):length(x[,1])){
    
    # for plotting
    currentProfit[i] = profit
    overallProfit[i] = totalProfit
    
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
  
  # set up plots
  par(bg='black', mfrow=c(2,1),col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  plot(currentProfit,type='l',col='blue',fg='white',main=paste('Current Profit'))
  
  plot(overallProfit,type='l',col='green',fg='white',main=paste('Total Profit'))
  
}

#############################################
# Function: multiple buy profit of data|MA  #
# Input: mAvgsAll(x) & percent for sell     #
# Return: ROI and Profit Summary            #
# NOTE: ONLY returns info for short         #
#############################################
fProfitDataMAMult <- function(x, timePeriod, percentBuy = 0, buyCap = 100)
{
  buyAmt      = 0 # amount a unit of stock was purchased at
  buyAmtPool  = 0 # pooled buy amounts
  sellAmt     = 0 # amount a unit of stock sold for
  profit      = 0 # money made or lost
  totalProfit = 0 # tracks total profit
  totalSpent  = 0 # tracks toatl buy amounts
  stockOn     = 0 # 0 if no stock, 1 if stock purchased INDICATIVE VARIABLE
  heldAmt     = 0 # amount held, left over
  
  # used for plotting
  currentProfitMult = c()
  overallProfitMult = c()
  
  for (i in (timePeriod+1):length(x[,1])){
    
    # used for plotting
    currentProfitMult[i] = profit
    overallProfitMult[i] = totalProfit
    
    # print pool FOR DEBUG
    #print(buyAmtPool)
    
    xStock <- x[-(i+1):-length(x[,1]),]
    
    if(gPredDataMABuy(xStock) && stockOn==0){
      buyAmt  = x[i,1]
      buyAmtPool = buyAmt
      # keep track of total amount spent, only changes with a buy
      totalSpent  = buyAmt + totalSpent
      stockOn = 1 # 1 indicates stock
      
    } else if(gPredDataMABuy(xStock) && stockOn>0 && stockOn<=buyCap){
      buyAmtPool = buyAmtPool + x[i,1]
      # keep track of total amount spent, only changes with a buy
      totalSpent  = x[i,1] + totalSpent
      stockOn = stockOn + 1
    
    } else if(gPredDataMASell(xStock) && stockOn>0 && buyAmt*(1+percentBuy)<=x[i,1]){
      sellAmt = x[i,1]
      profit  = sellAmt*stockOn-buyAmtPool
      # keep track of total profit, only changes with a sell
      totalProfit = profit + totalProfit 
      
      # print out amounts before reset (DEBUG)
      print(stockOn)
      print(buyAmt)
      print(buyAmtPool)
      print(sellAmt)
      
      # reset variables
      stockOn     = 0 # restart stock count
      buyAmt      = 0 # restart buyAmt since we finally sold
      buyAmtPool  = 0 # restart buyAmtPool as all stocks were sold
      sellAmt     = 0 # restart to
      
    } else if(i == length(x[,1])){
      # if we are still waiting to sell when period ends, we will list held amount
      if (x[i,2]<buyAmt){ ###SPECIAL CONDITION FOR SHORT###
        totalProfit = totalProfit 
        heldAmt = buyAmtPool
        
      } else{
        # nothing is needed as amount has been sold
      }
      #message("NOTE: final balance sold to calculate ROI")# line is no longer necessary as we say held amount
      
    } else{
      # nothing else needs to happen because we are waiting for a sell prediction or to run out of data
    }
  }
  message(timePeriod,"-day TOTALS:")
  message("Total Spent: ", totalSpent)
  message("Amount Held: ", heldAmt)
  message("Total Profit: ", totalProfit)
  message("Relative Profit (ROI): ", round(((totalProfit/totalSpent)*100),2), " %")
  message()
  
  # set up plots
  par(bg='black', mfrow=c(2,1),col.axis='white',col.lab='white',col.main='white',col.sub='white')
  
  # plot stuff
  plot(currentProfitMult,type='l',col='blue',fg='white',main=paste(timePeriod, '- day WMA Current Profit'))
  plot(overallProfitMult,type='l',col='green',fg='white',main=paste(timePeriod, '- day WMA Total Profit'))
  
}

# TEST PROFITS FOR PRESENTATION
  # RECALL THE MULT BUY PROFIT FUNCTION ASSUMES GROWTH OVER A TIME PERIOD!!!!
fProfitDataMAMult(mAvgsAll(X,25,95,0,0),25,0.15)     # 7.68% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0.2,0.8),25,0.15) #13.91% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0.1,0.9),25,0.15) #13.91% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0.5,0.5),25,0.15) # 8.51% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0,1),25,0.15)     #14.38% ROI with GS!!! from='2014-1-01', to='2016-2-1'

fProfitDataMAMult(mAvgsAll(X,25,95,0,0),25,0.1)      # 8.02% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0.2,0.8),25,0.1)  # 5.61% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0.1,0.9),25,0.1)  # 5.61% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0.5,0.5),25,0.1)  # 8.34% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMAMult(mAvgsAll(X,25,95,0,1),25,0.1)      # 8.00% ROI with GS!!! from='2014-1-01', to='2016-2-1'

  #compare to regular profit function, lookingn only at the short term of 25 (JAHU: 95 has a better ROI)
fProfitDataMA(mAvgsAll(X,25,95,0,0),25,95)     # 1.42% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMA(mAvgsAll(X,25,95,0.2,0.8),25,95) # 3.21% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMA(mAvgsAll(X,25,95,0.1,0.9),25,95) # 3.21% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMA(mAvgsAll(X,25,95,0.5,0.5),25,95) # 2.24% ROI with GS!!! from='2014-1-01', to='2016-2-1'
fProfitDataMA(mAvgsAll(X,25,95,0,1),25,95)     # 2.64% ROI with GS!!! from='2014-1-01', to='2016-2-1'

# FINDING TEST DATA
#fProfitDataMA(mAvgsAll(X,5,95,0,1),5,95)
#fProfitDataMA(mAvgsAll(X,10,90,0,1),10,90)
#fProfitDataMA(mAvgsAll(X,15,85,0,1),15,85)
#fProfitDataMA(mAvgsAll(X,20,80,0,1),20,80)
#fProfitDataMA(mAvgsAll(X,25,75,0,1),25,75)
#fProfitDataMA(mAvgsAll(X,30,70,0,1),30,70)
#fProfitDataMA(mAvgsAll(X,35,65,0,1),35,65)
#fProfitDataMA(mAvgsAll(X,40,60,0,1),40,60)
#fProfitDataMA(mAvgsAll(X,45,55,0,1),45,55)
#fProfitDataMA(mAvgsAll(X,50,100,0,1),50,100)

#fProfitCrossMA(mAvgsAll(X,20,40,.1,.9),20,40)


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