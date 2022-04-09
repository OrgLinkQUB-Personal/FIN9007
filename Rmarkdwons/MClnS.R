## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----5 lnS SPX Call------------------------------------

timestart<-Sys.time()

spotUnderlying=spotUnderlyingSPX

call_put=1


m=selectedExpiry

  cnt <- 0
  
#cal ttm and R and dividend
MClnS <- 
  TESTsampleImpliedR %>% 
  filter(expiryDate==m)

ttm=TTM <- unique(MClnS$ttm)

deltaT=ttm/nSteps

resIR <- lm(strike ~ putDcall,data = MClnS)

impliedSPXF <- resIR$coefficients[1]

impliedR <- log(resIR$coefficients[2])/ttm

impliedDividend <- impliedR-log(impliedSPXF/spotUnderlying)/ttm

MClnS <- 
  MClnS %>% 
  mutate(impliedSPXF=resIR$coefficients[1],
         impliedR=log(resIR$coefficients[2])/ttm) %>% 
  mutate(impliedDividend=impliedR-log(impliedSPXF/spotUnderlying)/ttm) 


#d <- as.Date.numeric(m,origin = "1970-01-01")

print(paste('for SPX expiry in',m,'we have R=',impliedR,'and Dividend=  ',impliedDividend))

interestRate=as.numeric(impliedR <- log(resIR$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedDividend <- (impliedR-log(impliedSPXF/spotUnderlying)/ttm))

##########
###load codes into para
ttm=TTM <- unique(MClnS$ttm)



#### load as his format
volatility=volatilitySPX 

deltaT=ttm/nSteps

spotUnderlying=spotUnderlyingSPX

nSteps=1*252

strike=Strikes <- unique(MClnS$strike)

interestRate=as.numeric(impliedR <- log(resIR$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedR-log(impliedSPXF/spotUnderlying)/ttm)

muMClnS=interestRate-dividendYield

nTimes=100000

MClnScall=rep(NA,length(strike))


###############

####
for (n in strike){
  
    strike <- n
    
    
simulatedEndSample=rep(NA,nTimes)

    for (i in 1:nTimes){
      
  simulatedEndSample[i]=spotUnderlying*exp((muMClnS-volatility^2/2)*T+volatility*rnorm(1)*sqrt(T))
}


optionSample=rep(NA,length(simulatedEndSample))

#################################################################
# based on the distribution of asset price at expiry, calculate option price at expiry

  for (i in 1:length(simulatedEndSample)){
  optionSample[i]=max((simulatedEndSample[i]-strike)*call_put,0)
  }


#################################################################
# discount option price by risk-free rate
optionPrice=mean(optionSample)*exp(-interestRate*deltaT*nSteps)
cat(paste("this is the",cnt,"times, option Price based on S is:",optionPrice, "\n")) 


  
  cnt <- cnt+1


  
if(call_put==1)
  {MClnScall[cnt] <- optionPrice}
  else
    {MClnSput[cnt] <- optionPrice}
  

  
  
  }


MClnS <- 
  MClnS %>% 
  mutate(MClnScall=MClnScall)


MClnSResult <- MClnS

timeend<-Sys.time()

runningtime<-timeend-timestart

print(runningtime)



## ----5 lnS compare-------------------------------------

MClnSResult <- 
  MClnSResult %>% 
  select(MClnScall,callPrice,strike) %>% 
  mutate(difference=callPrice-MClnScall)

MClnSResult %>% 
  ggplot(aes(x=strike,y=difference))+
  geom_line()+
  ggtitle('call price difference via stochastic process lnS with expriy date ',m)

write.csv(x = MClnSResult,file = "MClnSResult.csv")
  

