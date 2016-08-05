library(quantmod)
library("TTR")
company<-scan("~/Hello/companies.txt","")
names(company)<-company

tiList<-c("addSMA(n=10)","addSMA(n=20)","addSMA(n=50)","addSMA(n=100)","addSMA(n=200)","addEMA(n=10)","addEMA(n=20)","addEMA(n=50)","addEMA(n=100)","addEMA(n=200)","addBBands()","CMF",
                 "ADX","ATR","CCI","ROC","Momentum","RSI","addSAR()","SMI","TRIX","WPR")
names(tiList)<-c("SMA10","SMA20","SMA50","SMA100","SMA200","EMA10","EMA20","EMA50","EMA100","EMA200","BBands","CMF","ADX","ATR","CCI","ROC","Momentum","RSI","SAR","Stoch","TRIX","WPR")


getData<-function(){
  for (i in 1:length(company)){
    tryCatch({
      getSymbols(company[i],src="yahoo",from="2014-01-01", to="2015-03-03",env=.GlobalEnv$datae)
      print(i)
    },
    error=function(e){
      assign(company[i],NA,envir = .GlobalEnv$errore)
    })
  }
}
# listc<-{}
# 

finalData<-function(symbol){
  z<-get(symbol,envir=datae)
  x<-c()
  for( i in 1:nrow(z)){
    if(Vo(z[i]) == 0){
      x<-c(x,i)
    }
  }
  if(length(x)!=0) z<-z[-x]
  return(z)
}

buy<-function(symbol){
  z<-get(symbol,envir = datae)
  sma100<-as.numeric(tail(SMA(Cl(z),n=100),n=1))
  sar<-as.numeric(tail(SAR(cbind(Hi(z),Lo(z))),n=1))
  rsi<-as.numeric(tail(RSI(Cl(z)),n=1))
  close<-as.numeric(tail(Cl(z),n=1))
  print(paste(close,sma100,sar,close,rsi,sep=";"))
  condition1<-((close>sma100)&&(sar<close)&&(rsi>50))
  print(condition1)
  sma20<-as.numeric(tail(SMA(Cl(z),n=20),n=1))
  sma150<-as.numeric(tail(SMA(Cl(z),n=150),n=1))
  macd<-as.numeric(tail(MACD(Cl(z),12,26,9)[,1],n=1))
  stoch<-as.numeric(tail(stoch(HLC(z))[,1,n=1]))
  condition2<-((sma20>sma150)&&(macd>0)&&(stoch>20))
  print(condition2)
  adx<-as.numeric(tail(ADX(HLC(z))[,4],n=1))
  dip<-as.numeric(tail(ADX(HLC(z))[,1],n=1))
  din<-as.numeric(tail(ADX(HLC(z))[,2],n=1))
  condition3<-((adx>20)&&(dip>din)&&(sar<close))
  print(condition3)
  if((condition1)&&(condition2)&&(condition3)){
    print("TRUE")
    return(TRUE)
  }
  else{
    print("FALSE")
    return(FALSE)
  }
} 