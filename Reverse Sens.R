library(foreign)
library(MASS)
library(rootSolve)
library(numDeriv)
library(gtools)
reg=rlm(Avg.Pos~poly(Max.CPC,3,raw="TRUE"))
reg_curve=function(x) reg$coefficient[4]*x^3
reg_curve=function(x) reg$coefficient[4]*x^3 +reg$coefficient[3]*x^2 + reg$coefficient[2]*x +reg$coefficient[1]
sortmaxcpc=sort(Max.CPC)
maxcpc_bot=sortmaxcpc[1]
maxcpc_top=sortmaxcpc[length(sortmaxcpc)]
maxcpcrange=seq(maxcpc_bot,maxcpc_top,by=.01)
avgposrange=reg_curve(maxcpcrange)
plot(Max.CPC,Avg.Pos)
plot(maxcpcrange,avgposrange,type="l",col="blue",lwd=2)
reg_slope=grad(reg_curve,maxcpcrange)
reg_regslope=rlm(reg_slope~poly(maxcpcrange,3,raw="TRUE"))
regslope_curve=function(x) reg_regslope$coefficient[4]*x^3 + reg_regslope$coefficient[3]*x^2 + reg_regslope$coefficient[2]*x + reg_regslope$coefficient[1]
regsloperange=-(regslope_curve(maxcpcrange))              
plot(maxcpcrange,regsloperange,type="l",col="blue",lwd=2)
reg_dd=grad(regslope_curve,maxcpcrange)
reg_regdd=rlm(reg_dd~poly(maxcpcrange,3,raw="TRUE"))
regdd_curve=function(x) reg_regdd$coefficient[4]*x^3 + reg_regdd$coefficient[3]*x^2 + reg_regdd$coefficient[2]*x + reg_regdd$coefficient[1]
regdd_range=regdd_curve(maxcpcrange)
plot(maxcpcrange,regdd_range,type="l",col="blue",lwd=2)
pos_solve=function(x) regslope_curve(x)+.2
targetcpc=uniroot.all(function(x) pos_solve(x),c(maxcpc_bot,maxcpc_top))
x=length(targetcpc)
y=targetcpc
if(invalid(targetcpc)==TRUE){targetcpc=maxcpc_top
}else{
  dd_check=regdd_curve(targetcpc)
  correctslopeindex=which(regdd_curve(targetcpc)>0)
  targetcpc=targetcpc[correctslopeindex]
  targetcpc=targetcpc[1]
}
a=targetcpc
b=length(targetcpc)
if(invalid(targetcpc)==TRUE){targetcpc=maxcpc_bot
}else{
  if(targetcpc<=7){
    targetcpc=targetcpc
  }else{targetcpc=7}
}