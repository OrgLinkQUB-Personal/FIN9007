## ----setup, include=FALSE-------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------
q3plot <- read.csv("SPXtreePut.csv", header=T,as.is = T)

q3plot %>% 
  mutate(diffSPXput=putPrice-Tree) %>% 
  ggplot(aes(x=X,y=diffSPXput))+
  geom_line()+
  ggtitle("plot of SPXput difference of all Dates and Strikes")

