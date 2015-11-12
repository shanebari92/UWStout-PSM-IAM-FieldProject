# load Quantmod package
library(quantmod)

# import data
getSymbols('SPY', src='yahoo', from='2014-01-01', to=Sys.Date())

# create data matrix: open, high, low, close
data = matrix(cbind(GS[,1],GS[,2],GS[,3],GS[,4]),ncol=4)

# length of data
size = length(GS[,1])

#
# calculate moving averages based on opening price
#

# specify number of days lag
n_day = 3
m_day = 15

# simple moving average
SMA_n = SMA(data[,1],n=n_day)
SMA_m = SMA(data[,1],n=m_day)

# create list of zero's
SMA_class_n = cbind(rep(0,size))
SMA_class_m = cbind(rep(0,size))

# create matrix of pairs
class_matrix_n = matrix(cbind(c(data[,1]),c(SMA_n),c(SMA_class_n)),ncol=3)
class_matrix_m = matrix(cbind(c(data[,1]),c(SMA_m),c(SMA_class_m)),ncol=3)

# fill in class_matrix with data values if data > SMA
count_n = 0

# n_day
j = n_day
while(j <= size)
{
	if(data[j,1] >= SMA_n[j]) {class_matrix_n[j,3] = data[j,1]; count_n = count_n + 1}
	j = j + 1
}

count_m = 0

# m_day
i = m_day
while(i <= size)
{
	if(data[i,1] >= SMA_m[i]) {class_matrix_m[i,3] = data[i,1]; count_m = count_m + 1}
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
	main='3-day Simple Moving Average vs Data',
	xlab='Day',ylab='Price')
lines(SMA_n,col='green')
lines(class_matrix_n[,3],type='p',pch=8,col='pink')

# m_day SMA plot
plot(data[,1],type='l',col='red',fg='white',
	main='15-day Simple Moving Average vs Data',
	xlab='Day',ylab='Price')
lines(SMA_m,col='blue')
lines(class_matrix_m[,3],type='p',pch=8,col='yellow')




