library(TTR)
library(quantmod)

# 
symbol = 'GS'
start_date = '2015-11-01'
#end_date = 

# import data and put into matrix for easy reference
X = matrix(cbind(GS),ncol=6)
getSymbols(Symbols = symbol, src='yahoo', from=start_date, to=Sys.Date())


# High, Low, Close data
OHLC = matrix(cbind(X[,1],X[,2],X[,3],X[,4]),ncol=4)
HLC = matrix(cbind(X[,2],X[,3],X[,4]),ncol=3)

open = X[,1]
high = X[,2]
low = X[,3]
close = X[,4]
volume = X[,5]
adjusted = X[,6]



##########################################
# Split and Dividend Adjustment Ratios
# requires split and dividend data

# adjRatios = adjRatios(splits, dividends, close)
##########################################


##########################################
# Welles Wilder's Directional Movement Index
# requires High, Low, Close prices
# returns [positive direction index, negative direction index, direction index, average direction index]


ADX = ADX(HLC)
##########################################


##########################################
# Aroon indicator
# requires either High, Low prices or Close prices
# returns [aroonUp, aroonDn, oscillator]

aroon = aroon(c(high,low))
# or
# aroon = aroon(close)
##########################################



##########################################
# True Range/Average True Range
# requires High, Low, close
# returns [tr, atr, trueHigh, trueLow]

ATR = ATR(HLC)
##########################################


##########################################
# Bollinger Bands
# requires High, Low, Close
# returns [dn, mavg, up, pctB]

BBands = BBands(HLC)
##########################################



##########################################
# Commodity Channel Index
# requires High, Low, Close
# returns 

CCI = CCI(HLC)
##########################################


##########################################
# Chaikin Accumulation/Distribution
# requires High, Low, Close, Volume

chaikinAD = chaikinAD(HLC,volume=volume)
##########################################



##########################################
# Chaikin Volatility
# requires High, Low

chaikinVolatility = chaikinVolatility(HLC)
##########################################


##########################################
# Close Location Value
# requires High, Low, Close

CLV = CLV(HLC)
##########################################


##########################################
# Chaikin Money Flow
# requires High, Low, Close

CMF = CMF(HLC,volume=volume)
##########################################


##########################################
# Chande Momentum Oscillator
# requires any price vector

# open
CMO = CMO(open)
##########################################


##########################################
# Donchian Channel
# requires High, Low
# returns [high, mid, low]

DonchianChannel = DonchianChannel(c(X[,2],X[,3]))
##########################################


##########################################
# De-Trended Price Oscillator
# requires any price vector

DPO = DPO(open)
##########################################


##########################################
# DV Intermediate Oscillator
# takes any price vector

DVI = DVI(open)
# getting error messages on this one
##########################################


##########################################
# Arms' Ease of Movement Value
# requires High, Low
# returns [emv, emvMA]

EMV = EMV(HLC,volume=volume)

##########################################


##########################################
# Guppy Multiple Moving Averages
# takes any price vector
# returns MA values for 3,5,8,10,12,15 - day short term periods
# and 30,35,40,45,50,60 - day long term periods
GMMA = GMMA(open)

##########################################


##########################################
# Know Sure Thing
# takes any price vector
# returns [kst, signal]

KST = KST(open)

##########################################


##########################################
# Miscellaneous

lags = lags(open)

growth = growth(open,signals=0)
##########################################


##########################################
# MACD Oscillator
# takes any price vector
# returns [macd,signal]

MACD = MACD(open)
##########################################


##########################################
# Money Flow Index
# requires High, Low, Close, Volume

MFI = MFI(HLC,volume=volume)
##########################################


##########################################
# On Balance Volume
# takes any price vector as well as volume

OBV = OBV(open, volume=volume)
##########################################


##########################################
# Construct volatility bands around prices
# takes any price vector as well as volume
# returns [dn, center, up]

PBands = PBands(open, volume=volume)
##########################################


##########################################
# Rate of Change/Momentum
# takes any price vector

ROC = ROC(open)
momentum = momentum(open)
##########################################



##########################################
# Analysis of Running/Rolling/Moving Windows
# requires excess return and market return


rollSFM = rollSFM(Ra,Rb)
##########################################



##########################################
# Relative Strength Index
# takes any price vector

RSI = RSI(open)

##########################################


##########################################
# Percent Rank over a Moving Window
# takes any price vector
# requires a value for n

runPercentRank = runPercentRank(open)

# getting error messages
##########################################


##########################################
# Analysis of Running/Rolling/Moving Windows
# takes any price vector

runSum = runSum(open)
runMin = runMin(open)
runMax = runMax(open)
runMean = runMean(open)

runSD = runSD(open)
runMAD = runMAD(open)
wilderSum = wilderSum(open)

##########################################


##########################################
# Parabolic Stop-and-Reverse
# requires High, Low

SAR = SAR(HLC)
##########################################


##########################################
# Moving Averages

SMA = SMA(open)
EMA = EMA(open)
DEMA = DEMA(open)

EVWMA = EVWMA(open,volume=volume)
ZLEMA = ZLEMA(open)
VWAP = VWAP(open,volume=volume)

HMA = HMA(open)
ALMA = ALMA(open)
##########################################

##########################################
# Stochastic Oscillator/Stochastic Momentum Index
# requires High,Low,Close

# returns [fastk, fastd, slowd]
stoch = stoch(HLC)

# returns [SMI, signal]
SMI = SMI(HLC)
##########################################


##########################################
# Trend Detection Index
# takes a price vector
# returns [tdi, di]

TDI = TDI(open)
##########################################


##########################################
# Triple Smoothed Exponential Oscillator
# takes price vector

TRIX = TRIX(open)
# getting error messages
##########################################


##########################################
# Ultimate Oscillator
# requires HLC data

ultimateOscillator = ultimateOscillator(HLC)
##########################################


##########################################
# Vertical Horizontal Filter
# takes price vector

VHF = VHF(open)
##########################################


##########################################
# Volatility

volatility = volatility(OHLC)
##########################################


##########################################
# Williams Accumulation/Distribution
# requires HLC data

williamsAD = williamsAD(HLC)
##########################################


##########################################
# Williams %R
# requires HLC data

WPR = WPR(HLC)
##########################################


##########################################
# Zig Zag
# requires High, Low

ZigZag = ZigZag(HLC)
##########################################








