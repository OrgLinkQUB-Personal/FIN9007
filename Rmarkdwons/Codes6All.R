## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----6.1 spx load data---------------------------------

library(bizdays) 

spotUndelrying=3265.35
interestRate=0.01881165
dividendYield=0.01832855 

spxOptionCross=read.csv("./sampleData/spxOptionData20200110.csv", header=T,as.is = T,check.names=FALSE, colClasses=c("numeric","character","character",rep("numeric",3)))
spxOptionCross$TradeDate=as.Date(spxOptionCross$TradeDate, format = "%Y%m%d")
spxOptionCross$expiryDate=as.Date(spxOptionCross$expiryDate, format = "%Y%m%d")

sort(unique(spxOptionCross$expiryDate))

selectedExpiry="2020-06-19"
oneExpiryOptionData=spxOptionCross[spxOptionCross$expiryDate==selectedExpiry,]

# calculate business days
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
ttm=bizdays(oneExpiryOptionData$TradeDate[1], oneExpiryOptionData$expiryDate[1], cal = business_calendar)/252

callOptionData=oneExpiryOptionData[oneExpiryOptionData$c_p==1,]
callOptionData=callOptionData[order(callOptionData$strike),]


putOptionData=oneExpiryOptionData[oneExpiryOptionData$c_p==0,]
putOptionData=putOptionData[order(putOptionData$strike),]


## ----6.1 spx run---------------------------------------
marketPrice=1825.65


strike=1425


objective_Call_IV <- 
  function(volatility) {
    
  d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
  
  d2=d1-volatility*sqrt(ttm)
  
  callPrice=spotUndelrying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)
  
  return((callPrice-marketPrice)^2)
  
  }

result <- optimize(objective_Call_IV, interval = c(0, 30), tol = 0.0000001)


## ----6.1 Call------------------------------------------
callOptionData$IV=NA

for (i in 1:length(callOptionData$IV)){
  marketPrice=callOptionData[i,'price']
  strike=callOptionData[i,'strike']
  
  objective_Call_IV <- 
    function(volatility) {
      
    d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
    d2=d1-volatility*sqrt(ttm)
    
    callPrice=spotUndelrying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)
    
    return((callPrice-marketPrice)^2)
    
    }
  
  
  result <- optimize(objective_Call_IV, interval = c(0, 50), tol = 0.00001)
  
  callOptionData[i,'IV']=result$minimum
  
}

plot(callOptionData$strike,callOptionData$IV)

write.csv(x = callOptionData,file = "SPXcallIV.csv")



## ----6.1 Put-------------------------------------------

putOptionData$IV=NA

for (i in 1:length(putOptionData$IV)){
  
  marketPrice=putOptionData[i,'price']
  strike=putOptionData[i,'strike']
  objective_Put_IV <- function(volatility) {
    d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
    d2=d1-volatility*sqrt(ttm)
    
    putPrice=strike*exp(-interestRate*ttm)*pnorm(-d2)-spotUndelrying*exp(-dividendYield*ttm)*pnorm(-d1)
    
    return((putPrice-marketPrice)^2)
  }
  result <- optimize(objective_Put_IV, interval = c(0, 50), tol = 0.00001)
  
  putOptionData[i,'IV']=result$minimum
}

plot(putOptionData$strike,putOptionData$IV,type="l",col="red")
lines(callOptionData$strike,callOptionData$IV,col="green")

write.csv(x = putOptionData,file = "SPXputIV.csv")


## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----6.2 spy load--------------------------------------

library(bizdays) 

spotUndelrying=325.71

interestRate=0.000378369

dividendYield=-0.003014659
 

spyOptionCross=read.csv("./sampleData/spyOptionData20200110.csv", header=T,as.is = T,check.names=FALSE, colClasses=c("numeric","character","character",rep("numeric",3)))
spyOptionCross$TradeDate=as.Date(spyOptionCross$TradeDate, format = "%Y%m%d")
spyOptionCross$expiryDate=as.Date(spyOptionCross$expiryDate, format = "%Y%m%d")

sort(unique(spyOptionCross$expiryDate))

selectedExpiry="2020-06-19"
oneExpiryOptionData=spyOptionCross[spyOptionCross$expiryDate==selectedExpiry,]

# calculate business days
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
ttm=bizdays(oneExpiryOptionData$TradeDate[1], oneExpiryOptionData$expiryDate[1], cal = business_calendar)/252

callOptionData=oneExpiryOptionData[oneExpiryOptionData$c_p==1,]
callOptionData=callOptionData[order(callOptionData$strike),]


putOptionData=oneExpiryOptionData[oneExpiryOptionData$c_p==0,]
putOptionData=putOptionData[order(putOptionData$strike),]



## ----6.2 spy run---------------------------------------
marketPrice=82.245


strike=245


objective_Call_IV <- 
  function(volatility) {
    
  d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
  
  d2=d1-volatility*sqrt(ttm)
  
  callPrice=spotUndelrying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)
  
  return((callPrice-marketPrice)^2)
  
  }

result <- optimize(objective_Call_IV, interval = c(0, 30), tol = 0.0000001)


## ----6.2 Call------------------------------------------
callOptionData$IV=NA

for (i in 1:length(callOptionData$IV)){
  marketPrice=callOptionData[i,'price']
  strike=callOptionData[i,'strike']
  
  objective_Call_IV <- 
    function(volatility) {
      
    d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
    d2=d1-volatility*sqrt(ttm)
    
    callPrice=spotUndelrying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)
    
    return((callPrice-marketPrice)^2)
    
    }
  
  
  result <- optimize(objective_Call_IV, interval = c(0, 50), tol = 0.00001)
  
  callOptionData[i,'IV']=result$minimum
  
}

plot(callOptionData$strike,callOptionData$IV)

write.csv(x = callOptionData,file = "SPYcallIV.csv")



## ----6.2 Put-------------------------------------------

putOptionData$IV=NA

for (i in 1:length(putOptionData$IV)){
  
  marketPrice=putOptionData[i,'price']
  strike=putOptionData[i,'strike']
  objective_Put_IV <- function(volatility) {
    d1=(log(spotUndelrying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))
    d2=d1-volatility*sqrt(ttm)
    
    putPrice=strike*exp(-interestRate*ttm)*pnorm(-d2)-spotUndelrying*exp(-dividendYield*ttm)*pnorm(-d1)
    
    return((putPrice-marketPrice)^2)
  }
  result <- optimize(objective_Put_IV, interval = c(0, 50), tol = 0.00001)
  
  putOptionData[i,'IV']=result$minimum
}

plot(putOptionData$strike,putOptionData$IV,type="l",col="red")
lines(callOptionData$strike,callOptionData$IV,col="green")

write.csv(x = putOptionData,file = "SPYputIV.csv")


