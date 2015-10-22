
#download data from yahoo finance
getSymbols.yahoo('GS',from='2015-01-01',to=Sys.Date())

#check to see if data contains Open,High,Low,Close,Volume,Adjusted
print('OHLC?')
is.OHLC(GS)

print('Volume?')
has.Vo(GS)

print('Adjusted?')
has.Ad(GS)

#where and what was the high point?
seriesHi(GS)

# KJW has observed this file.
<<<<<<< HEAD
# CMD has also observed this file.
=======
# RMC has made an attempt to comment
>>>>>>> origin/master

#Shane's comment
