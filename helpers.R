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

# sell<-function(symbol){
#   z<-get(symbol,envir = datae)
#   sma20<-tail(SMA(Cl(z),n=20),n=1)
#   sma150<-tail(SMA(Cl(z),n=150),n=1)
#   macd<-tail(MACD(Cl(z),12,26,9)[,1],n=1)
#   
# }
  #   listc[[length(listc)+1]]<-z
# }
# 
# listn<-{}
# for(v in ls(datae))
# {
#   listn[[length(listn)+1]]<-v
# }
# names(listc)<-listn
# names(listn)<-listn
# 
# listdata<-{}
# 
# for(i in 1:length(listc)){
#   sma10<-SMA(Cl(listc[[i]]),n=10)
#   sma20<-SMA(Cl(listc[[i]]),n=20)
#   sma50<-SMA(Cl(listc[[i]]),n=50)
#   sma100<-SMA(Cl(listc[[i]]),n=100)
#   sma200<-SMA(Cl(listc[[i]]),n=200)
#   ema10<-EMA(Cl(listc[[i]]),n=10)
#   ema20<-EMA(Cl(listc[[i]]),n=20)
#   ema50<-EMA(Cl(listc[[i]]),n=50)
#   ema100<-EMA(Cl(listc[[i]]),n=100)
#   ema200<-EMA(Cl(listc[[i]]),n=200)
#   bbands<-BBands(HLC(listc[[i]])) # dn mavg up pctB
#   #ad<-chaikinAD(HLC(listc[[i]]),Vo(listc[[i]]))
#   cmf<-CMF(HLC(listc[[i]]),Vo(listc[[i]]))
#   adx<-ADX(HLC(listc[[i]])) #DIp DIn DX ADX
#   #trend<-
#   atr<-ATR(HLC(listc[[i]])) #tr atr trueHigh trueLow
#   cci<-CCI(HLC(listc[[i]]))
#   #dc<-DonchianChannel(cbind(Hi(listc[[i]]),Lo(listc[[i]])),n=50) #hi mid low
#   #emv<-EMV(cbind(Hi(listc[[i]]),Lo(listc[[i]])),Vo(listc[[i]])) #emv maEMV
#   #kst<-KST(Cl(listc[[i]])) #kst signal
#   macd<-MACD(Cl(listc[[i]]),12,26,9,maType = "EMA") #macd signal
#   #obv<-OBV(Cl(listc[[i]]),Vo(listc[[i]]))
#   roc<-ROC(Cl(listc[[i]]))
#   mom<-momentum(Cl(listc[[i]]))
#   rsi<-RSI(Cl(listc[[i]]))
#   sar<-SAR(cbind(Hi(listc[[i]]),Lo(listc[[i]])))
#   stochOSC<-stoch(HLC(listc[[i]])) # fastk fastD slowD
#   trix<-TRIX(Cl(listc[[i]])) # TRIX signal
#   #williamsAD<-williamsAD(HLC(listc[[i]]))
#   wpr<-WPR(HLC(listc[[i]]))
#   #zz<-ZigZag(cbind(Hi(listc[[i]]), Lo(listc[[i]])), change=20)
#   #vwap<-VWAP(Cl(listc[[i]]), Vo(listc[[i]]), n = 10)
#   listdata[[i]]<-cbind(listc[[i]],sma10,sma20,sma50,sma100,sma200,ema10,ema20,ema50,ema100,ema200,bbands,ad,cmf,adx,atr,cci,dc,emv,kst,macd,obv,roc,mom,rsi,sar,stochOSC,trix,williamsAD,wpr,zz,vwap)
#   colnames(listdata[[i]])<-c("O","H","L","C","V","A","sma10","sma20","sma50","sma100","sma200","ema10","ema20","ema50","ema100","ema200","dn","mavg","up","pctB","ad","cmf","DIp","DIn","DX","ADX",
#                           "tr","atr","trueHigh","trueLow","cci","hi","mid","low","emv","maEMV","kst","kst.signal","macd","macd.signal","obv","roc","mom","rsi","sar","fastK","fastD","slowD","TRIX","TRIX.Signal",
#                           "williamsAD","wpr","zz","vwap")
# }
# names(listdata)<-listn
# #####trend######
# # trend<-list()
# # for(i in 1:length(listc))
# # {
# #   temp<-as.data.frame(listc[[i]])[,c(2,3)]
# #   temp1<-aroon(as.xts(temp), n=20)
# #   trend[[i]]<-as.data.frame(temp1)  
# # }
# # names(trend)<-names(listc)
# #########################
# # sma20 <- SMA(data2[c('Close')], n = 20)
# # sma50 <- SMA(data2[c('Close')], n = 50)
# # sma100 <- SMA(data2[c('Close')], n = 100)
# # sma200 <- SMA(data2[c('Close')], n = 200)
# # ema10 <- EMA(data2[c('Close')], n = 10)
# # ema20 <- EMA(data2[c('Close')], n = 20)
# # ema50 <- EMA(data2[c('Close')], n = 50)
# # ema100 <- EMA(data2[c('Close')], n = 100)
# # ema200 <- EMA(data2[c('Close')], n = 200)
# # #bbands <- BBands(data2[,c("High","Low","Close")])
# # ad <- chaikinAD(data2[,c("High","Low","Close")], data2[,"Volume"])
# # cmf <- CMF(data2[,c("High","Low","Close")], data2[,"Volume"])
# # #adx <- ADX(data2[,c("High","Low","Close")])
# # trend <- aroon(data2[,c("High", "Low")], n = 20 )          
# # #atr <- ATR(data2[,c("High","Low","Close")], n = 14)               
# # #cci <- CCI(data2[,c("High","Low","Close")], n = 20)             
# # dc <- DonchianChannel( data2[,c("High","Low")], n = 50 )               
# # emv <- EMV(data2[,c("High","Low")], data2[,"Volume"])
# # kst <- KST(data2[,"Close"])
# # macd <- MACD(data2[,"Close"], 12, 26, 9, maType = "EMA" )
# # obv <- OBV(data2[,"Close"], data2[,"Volume"])
# # roc <- ROC(data2[,"Close"])
# # mom <- momentum(data2[,"Close"])
# # rsi <- RSI(data2[,"Close"])
# # sar <- SAR(data2[,c("High","Low")])
# # #stochOSC <- stoch(data2[,c("High","Low","Close")])
# # trix <- TRIX(data2[,"Close"])
# # #williamsAD <- williamsAD(data2[,c("High","Low","Close")])
# # #WPR <- WPR(data2[,c("High","Low","Close")])
# # #WPR <- WPR(HLC(data5))
# # zz <- ZigZag( data2[,c("High", "Low")], change = 20 )
# # vwap <- VWAP( data2[,c("Close")], data2[,c("Volume")], n = 10 )
