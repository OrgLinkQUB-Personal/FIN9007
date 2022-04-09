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




