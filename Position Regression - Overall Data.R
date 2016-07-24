library(foreign)
library(MASS)
library(rootSolve)
library(numDeriv)
library(gtools)
df_list=split(Working_Data,Working_Data$Concat)
len=length(df_list)
colHeads=c("Campaign","Adgroup","Keyword","Target.CPC","Match Type")
campaign_name=""
adgroup_name=""
keyword_name=""
match_type=""
tcpc=0
for(i in 1:len){
  
  if(i==157){next}
     
attach(df_list[[i]])
sortmaxcpc=sort(Max.CPC)
maxcpc_bot=sortmaxcpc[1]
maxcpc_top=sortmaxcpc[length(sortmaxcpc)]
maxcpcrange=seq(maxcpc_bot,maxcpc_top,by=.01)
reg=suppressWarnings(rlm(Avg.Pos~poly(Max.CPC,3,raw="TRUE")))
reg_curve=function(x) reg$coefficient[4]*x^3 +reg$coefficient[3]*x^2 + reg$coefficient[2]*x +reg$coefficient[1]
avgposrange=reg_curve(maxcpcrange)
reg_slope=grad(reg_curve,maxcpcrange)
reg_regslope=suppressWarnings(rlm(reg_slope~poly(maxcpcrange,3,raw="TRUE")))
regslope_curve=function(x) reg_regslope$coefficient[4]*x^3 + reg_regslope$coefficient[3]*x^2 + reg_regslope$coefficient[2]*x + reg_regslope$coefficient[1]
regsloperange=-(regslope_curve(maxcpcrange))              
reg_dd=grad(regslope_curve,maxcpcrange)
reg_regdd=suppressWarnings(rlm(reg_dd~poly(maxcpcrange,3,raw="TRUE")))
regdd_curve=function(x) reg_regdd$coefficient[4]*x^3 + reg_regdd$coefficient[3]*x^2 + reg_regdd$coefficient[2]*x + reg_regdd$coefficient[1]
regdd_range=regdd_curve(maxcpcrange)
pos_solve=function(x) regslope_curve(x)+targetslope
targetcpc=uniroot.all(function(x) pos_solve(x),c(maxcpc_bot,maxcpc_top))
if(invalid(targetcpc)==TRUE){targetcpc=maxcpc_top
}else{
  dd_check=regdd_curve(targetcpc)
  correctslopeindex=which(regdd_curve(targetcpc)>0)
  targetcpc=targetcpc[correctslopeindex]
  targetcpc=targetcpc[1]
}
if(invalid(targetcpc)==TRUE){targetcpc=maxcpc_bot
}else{
  if(targetcpc<=10){
    targetcpc=targetcpc
  }else{targetcpc=10}
}

campaign_name=c(campaign_name,as.character(df_list[[i]]$Campaign[1]))
adgroup_name=c(adgroup_name,as.character(df_list[[i]]$Adgroup[1]))
keyword_name=c(keyword_name,as.character(df_list[[i]]$Entity[1]))
match_type=c(match_type,as.character(df_list[[i]]$Match.Type[1]))
tcpc=c(tcpc,targetcpc)
   
}
output_df=data.frame(campaign_name,adgroup_name,keyword_name,tcpc,match_type)
names(output_df)=colHeads
output_df=output_df[-1,]
write.csv(output_df,"/Users/advaithmohan/Advaith/Adwords/Sensitivity/Dates/June 1/Output/MobileItem_Output.csv",row.names=FALSE)