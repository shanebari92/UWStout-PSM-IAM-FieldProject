# load Quantmod package
library(quantmod)

# import data
getSymbols('GS', src='yahoo', from='2014-01-01', to=Sys.Date())

# create data matrix: open, high, low, close
data = matrix(cbind(GS[,1],GS[,2],GS[,3],GS[,4]),ncol=4)

# length of data
size = length(GS[,1])

#
# calculate moving averages based on opening price
#

# specify number of days lag
n_day = 50
m_day = 150

# exponential moving average
EMA_n = EMA(data[,1],n=n_day)
EMA_m = EMA(data[,1],n=m_day)

# create list of zero's
EMA_class_n = cbind(rep(0,size))
EMA_class_m = cbind(rep(0,size))

# create matrix of pairs
class_matrix_n = matrix(cbind(c(data[,1]),c(EMA_n),c(EMA_class_n)),ncol=3)
class_matrix_m = matrix(cbind(c(data[,1]),c(EMA_m),c(EMA_class_m)),ncol=3)

# fill in class_matrix with data values if data > SMA

count_n = 0

# n_day
j = n_day
while(j <= size)
{
  if(data[j,1] >= EMA_n[j]) {class_matrix_n[j,3] = data[j,1]; count_n = count_n + 1}
  j = j + 1
}

count_m = 0

# m_day
i = m_day
while(i <= size)
{
  if(data[i,1] >= EMA_m[i]) {class_matrix_m[i,3] = data[i,1]; count_m = count_m + 1}
  i = i + 1
}


#
# plots
#

# configure plots
par(bg='black', mfrow=c(2,1),
    col.axis='white',col.lab='white',col.main='white',col.sub='white')

# n_day SMA plot
plot(data[,1],type='l',col='red',fg='white',
     main='3-day Exponential Moving Average vs Data',
     xlab='Day',ylab='Price')
lines(EMA_n,col='green')
lines(class_matrix_n[,3],type='p',pch=8,col='pink')

# m_day SMA plot
plot(data[,1],type='l',col='red',fg='white',
     main='15-day Exponential Moving Average vs Data',
     xlab='Day',ylab='Price')
lines(EMA_m,col='blue')
lines(class_matrix_m[,3],type='p',pch=8,col='yellow')




