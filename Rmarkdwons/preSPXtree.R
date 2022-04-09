## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----3.1 SPXTTM----------------------------------------
spxOptionCross <- 
  spxOptionCross %>% 
  mutate(diffDate=expiryDate-TradeDate,
         ttm=bizdays(TradeDate,expiryDate,cal = business_calendar)/252) 
  


## ----3.1 SPYTTM----------------------------------------
spyOptionCross <- 
  spyOptionCross %>% 
  mutate(diffDate=expiryDate-TradeDate,
         ttm=bizdays(TradeDate,expiryDate,cal = business_calendar)/252) 
  


## ----3.1 PUT-CALL--------------------------------------

#Call Option

TESTcall <- spxOptionCross %>% 
  filter(c_p==1) %>% 
  rename(callPrice=price) %>% 
  mutate(Kttm=(ttm/10)+strike) %>% 
  arrange(Kttm) %>% 
  data.frame()

#Put Option

TESTput <- spxOptionCross %>% 
  filter(c_p==0) %>% 
  rename(putPrice=price) %>% 
  mutate(Kttm=(ttm/10)+strike) %>% 
  arrange(Kttm) %>% 
  data.frame()



## ----3.1 check match-----------------------------------
callSD <- 
  TESTcall %>% 
  select(expiryDate,strike)


putSD <- 
  TESTput %>% 
  select(expiryDate,strike)

setdiff(callSD,putSD)


## ----3.1 rmUnmatch-------------------------------------
removeCall <- TESTcall %>% 
  filter(expiryDate=='2021-12-17',strike<=200)

xrow=removeCall$X



TESTcall <- 
  TESTcall %>% 
  filter(X!=xrow[1]&X!=xrow[2])



## ----3.1 pair------------------------------------------

TESTpaired <- 
  TESTcall %>% 
  left_join(TESTput[,c('Kttm','putPrice')],by='Kttm') %>% 
  select(-c(c_p))

#worning
#underlying only has data till 2020-12-31



## ----3.1 implied---------------------------------------

TESTsampleImpliedR <- 
  TESTpaired %>% 
  mutate(putDcall=putPrice-callPrice) %>% 
  select(Kttm,expiryDate,ttm,strike,putDcall,callPrice,putPrice,X) %>% 
  arrange(expiryDate) %>% 
  data.frame()

Dates <- unique(TESTsampleImpliedR$expiryDate)


