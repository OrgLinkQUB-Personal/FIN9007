## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----5 GBM SPX CALL------------------------------------
timestart<-Sys.time()

spotUnderlying=spotUnderlyingSPX

call_put=1

#for (m in Dates){

m=selectedExpiry

  cnt <- 0
  
#cal ttm and R and dividend
GBM <- 
  TESTsampleImpliedR %>% 
  filter(expiryDate==m)

ttm=TTM <- unique(GBM$ttm)

deltaT=ttm/nSteps

resIR <- lm(strike ~ putDcall,data = GBM)

impliedSPXF <- resIR$coefficients[1]

impliedR <- log(resIR$coefficients[2])/ttm

impliedDividend <- impliedR-log(impliedSPXF/spotUnderlying)/ttm

GBM <- 
  GBM %>% 
  mutate(impliedSPXF=resIR$coefficients[1],
         impliedR=log(resIR$coefficients[2])/ttm) %>% 
  mutate(impliedDividend=impliedR-log(impliedSPXF/spotUnderlying)/ttm) 


#d <- as.Date.numeric(m,origin = "1970-01-01")

print(paste('for SPX expiry in',m,'we have R=',impliedR,'and Dividend=  ',impliedDividend))

interestRate=as.numeric(impliedR <- log(resIR$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedDividend <- (impliedR-log(impliedSPXF/spotUnderlying)/ttm))

##########
###load codes into para
ttm=TTM <- unique(GBM$ttm)



#### load as his format
volatility=volatilitySPX 

deltaT=ttm/nSteps

spotUnderlying=spotUnderlyingSPX

nSteps=1*252

strike=Strikes <- unique(GBM$strike)

interestRate=as.numeric(impliedR <- log(resIR$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedR-log(impliedSPXF/spotUnderlying)/ttm)

muGBM=interestRate-dividendYield

nTimes=100000

GBMcall=rep(NA,length(strike))
GBMput=rep(NA,length(strike))


###############

####
  for (n in strike){
    strike <- n

spotUnderlying_t=rep(NA,nSteps)

spotUnderlying_t[1]=spotUnderlying

for (i in 2:nSteps){
  
  spotUnderlying_t[i]=spotUnderlying_t[i-1]+muGBM*spotUnderlying_t[i-1]*deltaT+volatility*spotUnderlying_t[i-1]*rnorm(1)*sqrt(deltaT)
  
}

#plot(spotUnderlying_t, type="l")

# simulate N times
spotUnderlyingMatrix=matrix(NA, nrow = nSteps, ncol = nTimes)

spotUnderlyingMatrix[1,]=spotUnderlying

trials <- seq(1, nTimes)

for (i in 1:nTimes){
  for (j in 2:nSteps){
    spotUnderlyingMatrix[j,i]=spotUnderlyingMatrix[j-1,i]+muGBM*spotUnderlyingMatrix[j-1,i]*deltaT+volatility*spotUnderlyingMatrix[j-1,i]*rnorm(1)*sqrt(deltaT)
  }
}


simulatedEndSample=spotUnderlyingMatrix[nSteps,]

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
  
  #cat('Loop',paste(cnt),'is for option expiry at',paste(d),'when strike is',n,"Option price is: ",optionTemp,'\n')
  
  
if(call_put==1)
  {GBMcall[cnt] <- optionPrice}
  else
    {GBMput[cnt] <- optionPrice}
  

  
  
  }

GBM <- 
  GBM %>% 
  mutate(GBMcall=GBMcall)


GBMResult <- GBM
#if (m==18278)
  #{GBMResult <- GBM}
  #else
    #{GBMResult <- bind_rows(GBMResult,GBM)}
#}


#output time usage
timeend<-Sys.time()

runningtime<-timeend-timestart

print(runningtime)





## ----5 GBM compare-------------------------------------
GBMResult <- 
  GBMResult %>% 
  select(GBMcall,callPrice,strike) %>% 
  mutate(difference=callPrice-GBMcall)

GBMResult %>% 
  ggplot(aes(x=strike,y=difference))+
  geom_line()+
  ggtitle('the plot of call price difference via stochastic process GBM with expriy date',m)
  

write.csv(x = GBMResult,file = "GBMResult.csv")

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
  

## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----5 BSM input---------------------------------------
selectedTradeDate="2020-01-10"

spotUnderlyingSPX <- 
  underlying %>% 
  filter(TradeDate==selectedTradeDate) %>% 
  select(SPX) %>% 
  as.numeric()

volatilitySPX <- 
  underlying %>% 
  filter(TradeDate==selectedTradeDate) %>% 
  select(stdAnnualSPX) %>% 
  as.numeric()


## ----5 BSM loop----------------------------------------
################
timestart<-Sys.time()

spotUnderlying=spotUnderlyingSPX


for (m in Dates){

  cnt <- 0
  
#cal ttm and R and dividend
BSM <- 
  TESTsampleImpliedR %>% 
  filter(expiryDate==m)

ttm=TTM <- unique(BSM$ttm)

resIR <- lm(strike ~ putDcall,data = BSM)

impliedSPXF <- resIR$coefficients[1]

impliedR <- log(resIR$coefficients[2])/ttm

impliedDividend <- impliedR-log(impliedSPXF/spotUnderlying)/ttm

BSM <- 
  BSM %>% 
  mutate(impliedSPXF=resIR$coefficients[1],
         impliedR=log(resIR$coefficients[2])/ttm) %>% 
  mutate(impliedDividend=impliedR-log(impliedSPXF/spotUnderlying)/ttm) 


d <- as.Date.numeric(m,origin = "1970-01-01")

print(paste('for SPX expiry in',d,'we have R=',impliedR,'and Dividend=  ',impliedDividend))

interestRate=as.numeric(impliedR <- log(resIR$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedDividend <- (impliedR-log(impliedSPXF/spotUnderlying)/ttm))

##########
###load codes into para
ttm=TTM <- unique(BSM$ttm)



#### load as his format
volatility=volatilitySPX 
spotUnderlying=spotUnderlyingSPX

strike=Strikes <- unique(BSM$strike)

interestRate=as.numeric(impliedR <- log(resIR$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedR-log(impliedSPXF/spotUnderlying)/ttm)



BSMcall=rep(NA,length(strike))
BSMput=rep(NA,length(strike))


###############

####
  for (n in strike){
    strike <- n


d1=(log(spotUnderlying/strike)+(interestRate-dividendYield+volatility^2/2))*ttm/(volatility*sqrt(ttm))


d2=d1-volatility*sqrt(ttm)




callPrice=
  spotUnderlying*exp(-dividendYield*ttm)*pnorm(d1)-strike*exp(-interestRate*ttm)*pnorm(d2)


putPrice=
  strike*exp(-interestRate*ttm)*pnorm(-d2)-spotUnderlying*exp(-dividendYield*ttm)*pnorm(-d1)
  
  cnt <- cnt+1
  
  #cat('Loop',paste(cnt),'is for option expiry at',paste(d),'when strike is',n,"Option price is: ",optionTemp,'\n')
  
  
  
  BSMcall[cnt] <- callPrice
  BSMput[cnt] <- putPrice
  
  
  }

BSM <- 
  BSM %>% 
  mutate(BSMcall=BSMcall,
         BSMput=BSMput)



if (m==18278)
  {BSMResult <- BSM}
  else
    {BSMResult <- bind_rows(BSMResult,BSM)}
}


#output time usage
timeend<-Sys.time()

runningtime<-timeend-timestart

print(runningtime)

write.csv(x = BSMResult,file = "BSMResult.csv")




## ----5 BSM compare-------------------------------------
BSMCompare <- 
  BSMResult %>%
  select(callPrice,putPrice,BSMcall,BSMput,strike,expiryDate,X) %>% 
  mutate(callD=callPrice-BSMcall,
         putD=putPrice-BSMput)

# BSMCompare %>% 
#   filter(expiryDate==selectedExpiry) %>%
#   ggplot(aes(x=strike,y=callD))+
#   geom_line()


BSMCompare %>% 
  ggplot(aes(x=X,y=callD))+
  geom_line()+
  ggtitle("call price difference of SPX option of all expiryDate and strike")




## ----5 BSM parity plot---------------------------------

BSMCompare <- 
  BSMCompare %>% 
mutate(leftside=callPrice+strike*exp(-interestRate*ttm),
  rightside=spotUnderlying*exp(-dividendYield*ttm)+putPrice) %>% 
mutate(parity=leftside-rightside) %>% 
  mutate(PdiffParity=parity/leftside)


BSMCompare %>% 
  # filter(expiryDate==selectedExpiry) %>% 
  ggplot(aes(x=strike,y=parity))+
  geom_line()

mean(BSMCompare$parity)
mean(BSMCompare$PdiffParity)


BSMCompare %>% 
  ggplot(aes(x=X,y=PdiffParity))+
  geom_line()


write.csv(x = BSMCompare,file = "BSMCompare.csv")
  


## ----5 BSM plot each-----------------------------------
library(patchwork)

for (m in Dates){
  
d <- as.Date.numeric(m,origin = "1970-01-01")
plott <- BSMCompare %>% 
  filter(expiryDate==m) %>%
  ggplot(aes(x=strike,y=callD))+
  geom_line()+
  geom_vline(xintercept = spotUnderlyingSPX,colour="red")+
  ggtitle(paste(d))+
  xlab(NULL)+
  ylab(NULL)

# ggsave(paste("SPXput",d),device = "png")

if(m==18278)
  {plotAllbsm <-plott}
  else{plotAllbsm <- plotAllbsm+plott}

}

BSMdiff <- plotAllbsm+plot_layout(ncol = 4)+
  plot_annotation(title = 'Diff between SPX optiong market price and BSM price of all ExpiryDates',subtitle =paste("red line is the spotunderlying price=",spotUnderlyingSPX) )

BSMdiff

ggsave("BSMdiff.png",width = 2280, height = 1460, units = "px")





