## ----setup, include=FALSE-------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----2.1 SPXTTM-----------------------------------------
spxOptionCross <- 
  spxOptionCross %>% 
  mutate(diffDate=expiryDate-TradeDate,
         ttm=bizdays(TradeDate,expiryDate,cal = business_calendar)/252) 
  


## ----2.1 SPYTTM-----------------------------------------
spyOptionCross <- 
  spyOptionCross %>% 
  mutate(diffDate=expiryDate-TradeDate,
         ttm=bizdays(TradeDate,expiryDate,cal = business_calendar)/252) 
  


## ----PUT-CALL-------------------------------------------

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



## -------------------------------------------------------
callSD <- 
  TESTcall %>% 
  select(expiryDate,strike)


putSD <- 
  TESTput %>% 
  select(expiryDate,strike)

setdiff(callSD,putSD)


## -------------------------------------------------------
removeCall <- TESTcall %>% 
  filter(expiryDate=='2021-12-17',strike<=200)

xrow=removeCall$X



TESTcall <- 
  TESTcall %>% 
  filter(X!=xrow[1]&X!=xrow[2])



## -------------------------------------------------------

TESTpaired <- 
  TESTcall %>% 
  left_join(TESTput[,c('Kttm','putPrice')],by='Kttm') %>% 
  select(-c(c_p))

#worning
#underlying only has data till 2020-12-31



## -------------------------------------------------------

TESTsampleImpliedR <- 
  TESTpaired %>% 
  mutate(putDcall=putPrice-callPrice) %>% 
  select(Kttm,expiryDate,ttm,strike,putDcall,callPrice,putPrice,X) %>% 
  arrange(expiryDate) %>% 
  data.frame()

Dates <- unique(TESTsampleImpliedR$expiryDate)


