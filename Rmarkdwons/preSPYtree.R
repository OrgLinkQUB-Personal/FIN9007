## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----3.2 SPYTTM----------------------------------------
spyOptionCross <- 
  spyOptionCross %>% 
  mutate(diffDate=expiryDate-TradeDate,
         ttm=bizdays(TradeDate,expiryDate,cal = business_calendar)/252) 
  


## ----3.2 PUT-CALL--------------------------------------

#Call Option

TESTcallSPY <- spyOptionCross %>% 
  filter(c_p==1) %>% 
  rename(callPrice=price) %>% 
  mutate(Kttm=(ttm/10)+strike) %>% 
  arrange(Kttm) %>% 
  data.frame()

#Put Option

TESTputSPY <- spyOptionCross %>% 
  filter(c_p==0) %>% 
  rename(putPrice=price) %>% 
  mutate(Kttm=(ttm/10)+strike) %>% 
  arrange(Kttm) %>% 
  data.frame()



## ----3.2 check match-----------------------------------
callSD <- 
  TESTcallSPY %>% 
  select(expiryDate,strike)


putSD <- 
  TESTputSPY %>% 
  select(expiryDate,strike)

setdiff(callSD,putSD)


## ----eval=FALSE, include=FALSE-------------------------
## removeCall <- TESTcallSPY %>%
##   filter(expiryDate=='2021-12-17',strike<=200)
## 
## xrow=removeCall$X
## 
## 
## 
## TESTcallSPY <-
##   TESTcallSPY %>%
##   filter(X!=xrow[1]&X!=xrow[2])
## 


## ----3.2 pair------------------------------------------

TESTpairedSPY <- 
  TESTcallSPY %>% 
  left_join(TESTputSPY[,c('Kttm','putPrice')],by='Kttm') %>% 
  select(-c(c_p))

#worning
#underlying only has data till 2020-12-31



## ----3.2 implied---------------------------------------

TESTsampleImpliedRSPY <- 
  TESTpairedSPY %>% 
  mutate(putDcall=putPrice-callPrice) %>% 
  select(Kttm,expiryDate,ttm,strike,putDcall,callPrice,putPrice,X) %>% 
  arrange(expiryDate) %>% 
  data.frame()

DatesSPY <- unique(TESTsampleImpliedRSPY$expiryDate)

DatesSPY <- DatesSPY[-1]


