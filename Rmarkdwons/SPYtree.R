## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----3.2 pre input-------------------------------------
selectedTradeDate="2020-01-10"

spotUnderlyingSPY <- 
  underlying %>% 
  filter(TradeDate==selectedTradeDate) %>% 
  select(SPY) %>% 
  as.numeric()

volatilitySPY <- 
  underlying %>% 
  filter(TradeDate==selectedTradeDate) %>% 
  select(stdAnnualSPY) %>% 
  as.numeric()





## ----3.2 state price at expiry-------------------------
nSteps=1000

statesTempSPY=rep(NA,nSteps+1)

for (j in 1:(nSteps+1)){
  statesTempSPY[j]=spotUnderlyingSPY*uCRR^(j-1)*dCRR^(nSteps-(j-1))
}




## ----3.3 option price at expiry------------------------
# note: nSetps=2
optionTempSPY=rep(NA,nSteps+1)
for (j in 1:(nSteps+1)){
  optionTempSPY[j]=max((statesTempSPY[j]-strike)*call_put,0)
}



#cat("state price at step ",nSteps+1," is: ",statesTemp,'\n') 
#cat("Option price at step ",nSteps+1," is: ",optionTemp,'\n')



## ----3.2 SPY loop for RandD----------------------------
################
timestart<-Sys.time()



#linear regression
nSteps=1000
call_put=-1#1 for call option, -1 for put option

for (m in DatesSPY){
cnt <- 0
  

lmsampleSPY <- 
  TESTsampleImpliedRSPY %>% 
  filter(expiryDate==m)

ttm=TTMSPY <- unique(lmsampleSPY$ttm)

resIRSPY <- lm(strike ~ putDcall,data = lmsampleSPY)


impliedSPYF <- resIRSPY$coefficients[1]

impliedRSPY <- log(resIRSPY$coefficients[2])/ttm

impliedDividendSPY <- impliedR-log(impliedSPYF/spotUnderlyingSPY)/ttm

lmsampleSPY <- 
  lmsampleSPY %>% 
  mutate(impliedSPYF=resIRSPY$coefficients[1],
         impliedRSPY=log(resIRSPY$coefficients[2])/ttm) %>% 
  mutate(impliedDividendSPY=impliedRSPY-log(impliedSPYF/spotUnderlyingSPY)/ttm) 


d=as.Date.numeric(m)
print(paste('for SPY expiry in',d,'we have R=',impliedRSPY,'and Dividend=  ',impliedDividendSPY))



##########

interestRate=as.numeric(impliedRSPY <- log(resIRSPY$coefficients[2])/ttm) 

dividendYield=as.numeric(impliedRSPY-log(impliedSPYF/spotUnderlyingSPY)/ttm)

volatility=volatilitySPY 

spotUnderlying=spotUnderlyingSPY

strike=Strikes <- unique(lmsampleSPY$strike)

americanOption=TRUE

deltaT=ttm/nSteps

uCRR=exp(volatility*sqrt(deltaT))

dCRR=exp(-volatility*sqrt(deltaT))

aCRR=exp((interestRate-dividendYield)*(deltaT))

pCRR=(aCRR-dCRR)/(uCRR-dCRR)

####
optionTempSPY=rep(NA,nSteps+1)
statesTempSPY=rep(NA,nSteps+1)
TreePriceSPY=rep(NA,length(strike))

####
  for (n in strike){
    TESTstrike <- n

    for (j in 1:(nSteps+1)){
      statesTempSPY[j]=spotUnderlying*uCRR^(j-1)*dCRR^(nSteps-(j-1))
                            }
####
    for (o in 1:(nSteps+1)){
        optionTempSPY[o]=max((statesTempSPY[o]-TESTstrike)*call_put,0)
                            }  
  
####    
    for (i in nSteps:1){optionTempSPY_1=rep(NA,i)
                        statesTempSPY=rep(NA,i)
                        s=(nSteps+1)
    
      for (s in 1:i){
        statesTempSPY[s]=spotUnderlying*uCRR^(s-1)*dCRR^(i-1-(s-1))
                    }
    
    if (americanOption==TRUE){
      for (j in 1:i){
        optionTempSPY_1[j]=max(((1-pCRR)*optionTempSPY[j]+pCRR*optionTempSPY[j+1])*exp(-interestRate*deltaT),(statesTempSPY[j]-TESTstrike)*call_put)
                    }        }  
    else{
      for (j in 1:i){
        optionTempSPY_1[j]=((1-pCRR)*optionTempSPY[j]+pCRR*optionTempSPY[j+1])*exp(-interestRate*deltaT)
                    }     
        }
                    
  optionTempSPY=optionTempSPY_1}
  
  cnt <- cnt+1
  
  #cat('Loop',paste(cnt),'is for option expiry at',paste(d),'when strike is',n,"Option price is: ",optionTempSPY,'\n')
  
  
  
  TreePriceSPY[cnt] <- optionTempSPY
                    }
lmsampleSPY <- 
  lmsampleSPY %>% 
  mutate(Tree=TreePriceSPY)

#TESTResultSPY <- lmsampleSPY


if (m==18274)
  {TESTResultSPY <- lmsampleSPY}
  else
    {TESTResultSPY <- bind_rows(TESTResultSPY,lmsampleSPY)}
}





write.csv(x = TESTResultSPY,file = "SPYtreePut.csv")
#output time usage
timeend<-Sys.time()

runningtime<-timeend-timestart

print(runningtime)



## ----3.2 subset and plot-------------------------------



PdiffSPY <- 
  TESTResultSPY %>% 
  select(Tree,putPrice,strike,expiryDate,X) %>% 
  mutate(diffSPYput=putPrice-Tree)

PlotSPYput <- 
  PdiffSPY %>%
  ggplot(aes(x=X,y=diffSPYput))+
  geom_line()+
  ggtitle("SPYput option price diff between market and Tree  all Dates&Strikes")


PlotSPYput

ggsave("PlotSPYput.png",width = 2280, height = 720, units = "px")




## ----3.2 patch plot------------------------------------
# Dates1 <- DatesSPY[1:12]
# Dates2 <- DatesSPY[13:24]
# Dates3 <- DatesSPY[25:36]
library(patchwork)

for(i in c(0,1,2)){
  
  Plotdates <-na.omit(DatesSPY[(1+i*12):(12+i*12)]) 
  
  
  for(m in Plotdates){
    
d <- as.Date.numeric(m,origin = "1970-01-01")

print(d)

plott <- PdiffSPY %>% 
  filter(expiryDate==m) %>%
  ggplot(aes(x=strike,y=diffSPYput))+
  geom_line()+
  geom_vline(xintercept = spotUnderlyingSPY,colour="red")+
  ggtitle(paste(d))+
  xlab(NULL)+
  ylab(NULL)

# ggsave(paste("SPYput",d),device = "png")


if(m==Plotdates[1])
  {plotAllSPYput <-plott}
  else{plotAllSPYput <- plotAllSPYput+plott}

  }
  
SPYdiff <- plotAllSPYput+plot_annotation(title = 'Diff between SPY optiong market price and Tree price of all ExpiryDates',subtitle =paste("red line is the spotunderlying price=",spotUnderlyingSPY))#+plot_layout(ncol = 4)  

assign(paste("SPYdiff",i+1),SPYdiff)
  
}
ggsave("`SPYdiff 1`.png",width = 1920, height = 1080, units = "px")
ggsave("`SPYdiff 2`.png",width = 1920, height = 1080, units = "px")
ggsave("`SPYdiff 3`.png",width = 1920, height = 1080, units = "px")


`SPYdiff 1`
`SPYdiff 2`
`SPYdiff 3`
  
# SPYdiffALL<- `SPYdiff 1`+`SPYdiff 2`+`SPYdiff 3`+plot_annotation(title = 'Diff between SPY optiong market price and Tree price of all ExpiryDates',subtitle =paste("red line is the spotunderlying price=",spotUnderlyingSPY))
# 
# SPYdiffALL
# 
# `SPYdiff 1`/`SPYdiff 2`

# ggsave("SPYdiff.png")


