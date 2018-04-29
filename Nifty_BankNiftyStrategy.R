
#NIFTy and BankNIfty startegy analysis and backtesting

#packages
require(Quandl)
require(dplyr)


NIFTY=Quandl("NSE/NIFTY_50",api_key="uYHynQoeChxswuz2Sz6g")
BANKNIFTY=Quandl("NSE/NIFTY_BANK", api_key="uYHynQoeChxswuz2Sz6g")

NIFTY=NIFTY[1:nrow(BANKNIFTY),]

#Find correlation
NIFTY$Returns=log(NIFTY$Close[1:(nrow(NIFTY))]/NIFTY$Close[2:nrow(NIFTY)])*100
BANKNIFTY$Returns=log(BANKNIFTY$Close[1:(nrow(BANKNIFTY))]/BANKNIFTY$Close[2:nrow(BANKNIFTY)])*100

Pair_cor=cor(NIFTY$Returns,BANKNIFTY$Returns)
plot(NIFTY$Returns[1:(nrow(NIFTY)-1)],BANKNIFTY$Returns[1:(nrow(BANKNIFTY)-1)])
BANKNIFTY[which(BANKNIFTY$Returns==min(BANKNIFTY$Returns[1:(nrow(BANKNIFTY)-1)])),]

ml=lm(NIFTY$Returns[1:(nrow(NIFTY)-1)] ~ BANKNIFTY$Returns[1:(nrow(BANKNIFTY)-1)])
ml_rev=lm(BANKNIFTY$Returns[1:(nrow(BANKNIFTY)-1)] ~ NIFTY$Returns[1:(nrow(NIFTY)-1)])
summary(ml_rev)
anova(ml)
abline()
cov(NIFTY$Returns,BANKNIFTY$Returns)
predict.lm(ml_rev,newdata=0.5)

