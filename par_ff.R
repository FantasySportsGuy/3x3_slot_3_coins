library(dplyr)

#this function calculates the 90% confidence factor and the upper and lower 
#limits of a 90% confidence interval for a a certain amount of pulls
#the function returns a data frame containing the amount of pulls, 
#the confidence factor, and the lower and upper limit of the confidence interval
confidence_90<-function(xbar,vi,pulls){
  #the formula for the 90% confidence interval is 
  #xbar +or- vi/sqrt(pulls)
  #xbar is the payback percentage, vi is the Volatility Index, and pulls 
  #is how many pulls 
  lower<-(xbar-(vi/sqrt(pulls)))
  upper<-(xbar +(vi/sqrt(pulls)))
  confidence_factor<-((upper-lower)/2)*100
  conf.table<-c(pulls,confidence_factor,lower,upper)
  conf.table<-as.data.frame(conf.table)
  conf.table<-t(conf.table)#transpose to turn conf.table from a list into a colm
  colnames(conf.table)=c("Pulls","confidence_factor","lower","upper")
  return(conf.table)
}

#this function returns a dataframe whose rows contain the number of pulls,
#confidence factor, lower limit, and upper limit of a confidence interval. 
#This function creates a dataframe for 1000,10000, 100000,1000000, and 10000000
#pulls from a given xbar(percent payback) and vi(volatility index)
Con_table<-function(xbar,vi){
  confidence_table_1000<-confidence_90(xbar=xbar,vi=vi,pulls=1000)
  confidence_table_10000<-confidence_90(xbar=xbar,vi=vi,pulls=10000)
  confidence_table_100000<-confidence_90(xbar=xbar,vi=vi,pulls=100000)
  confidence_table_1000000<-confidence_90(xbar=xbar,vi=vi,pulls=1000000)
  confidence_table_10000000<-confidence_90(xbar=xbar,vi=vi,pulls=10000000)
  
  contable<-rbind(confidence_table_1000,confidence_table_10000)
  contable<-rbind(contable,confidence_table_100000)
  contable<-rbind(contable,confidence_table_1000000)
  contable<-rbind(contable,confidence_table_10000000)
  contable<-as.data.frame(contable)
  contable$Pulls<-as.integer(contable$Pulls)
  return(contable)
}

#this function takes in the variables calc which is a dataframe containing two 
#columns which are hits which is the number of hits of a specific pay outcome, 
#and the corresponding payout, total.in which is the product of the number 
#of stops of each reel, and the amount of coins the slot takes. The output is 
#a dataframe containing the 90% confidence intervals and factor for  1000,10000, 
#100000,1000000, and 10000000 pulls.
confidence_table<-function(calc,total.in,coins){
  #if 0 is not the pay column of calc, we add in this row with the number of 
  #hits being total.in-sum(hits). This is the number of hits for 0 payout
  if(!(0 %in% calc$pay)){
    p_0<-data.frame(
      hits=total.in -sum(calc$hits),
      pay=0
    )
    calc<-rbind(calc,p_0)
  }
  hit.total<-sum(calc$hits)#calculating the number of total hits
  hit.percent=as.data.frame(calc$hits/(hit.total))#calculating the hit % of
  #each payout
  ev=hit.percent*calc$pay#ev is the expected value for each payout
  xbar<-sum(ev)/coins#xbar is the expected value of the machine per coin
  e.v.disadv=1-(sum(ev)/coins)#e.v.disadv is the expected disadvantage of the 
  #player per coin, this is also 1-xbar
  
  netpay<-as.data.frame((coins-calc$pay)/coins)#netpay is the amount of coins
  #minus the payout divided by the amount of coins. This is also equal to 
  #the amount of each payout divided by coins minus 1.
  
  #the games standard deviation is sqrt(sum((netpay-e.v.disadv)^2 * hit.percent))
  e<-netpay-e.v.disadv#netpay-e.v.disadv
  f<-e^2 #(netpay-e.v.disadv)^2
  cf<-f*hit.percent#(netpay-e.v.disadv)^2 * hit.percent)
  sd<-sqrt(sum(cf))#sqrt(sum((netpay-e.v.disadv)^2 * hit.percent))
  #vi is the games volatility index(V.I) which is equal to k*sigma, where 
  #k is the z score for the required confidence interval, and sigma is the 
  #standard deviation
  vi=sd*1.645
  contable<-Con_table(xbar=xbar,vi=vi)
  return(contable)
}

