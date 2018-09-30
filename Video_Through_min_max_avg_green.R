
rm(list=ls())
dir1="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Vid_Scenario/Scenarios"
setwd(dir1)
#********************************************************************************************************
getwd()
#********************************************************************************************************
#3 Loading Libraries
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
#fread by default create data table. 
options(datatable.fread.datatable=TRUE)
list_sen<-fread("VidList_Scenarios.csv")

#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*.lsa"))
file<-files[[1]]
par(mfrow=c(2,2))
Sda<-data.table()
for(file in files){
  #**************************************
  #Extracting scenario number 
  temp=strsplit(file,"_")[[1]][1]
  no=strsplit(temp,"S")[[1]][2]
  no=as.numeric(no)
  run_no=strsplit(file,"_")[[1]][2]
  run_no=strsplit(run_no,"[.]")[[1]][1]
  run_no<-as.numeric(run_no)
  #**************************************
  assign("sig_data",fread(file,header=FALSE,sep=";"))
  #********************************************************************************
  #Getting the data for signal group 6 (EBT)
  #Migth need the data for sg 4 in future
  SGr=6
  sig_data<-subset(sig_data,V4==SGr,select=c(V1,V4,V5,V6))
  colnames(sig_data)<-c("simS","SgG","Indi","dur_prev_st")
  summary(sig_data)
  #Indi tells the current signal state
  #dur_prev_st tells the duration of previous signal state
  #Carry out appropriate corrections 
  sig_data[Indi=="red",Indi_cor:="amber"]
  sig_data[Indi=="amber",Indi_cor:="green"]
  sig_data[Indi=="green",Indi_cor:="red"]
  sig_data[,dur:=as.numeric(dur_prev_st)]
  sig_data$no<-rep(no,length(sig_data$simS))
  sig_data$run_no<-rep(run_no,length(sig_data$simS))
  
  Sda<-rbindlist(list(Sda,sig_data))
  sig_data<-NULL
}



setnames(Sda,"no","Scenario")
Sda<-merge(Sda,list_sen)
Sda$vo<-interaction(as.factor(Sda$Actuated_St),as.factor(Sda$Cross_St))
Sda1<-Sda[Indi_cor=="green"]

SdaNew<-Sda1
SdaNew[,min_gr:=min(dur),by=list(Scenario,run_no)]
SdaNew[,avg_gr:=mean(dur),by=list(Scenario,run_no)]
SdaNew[,max_gr:=max(dur),by=list(Scenario,run_no)]



SdaNew<-unique(SdaNew,by=c("Scenario","run_no"))
SdaNew[,min_gr:=mean(min_gr),by=list(Scenario)]
SdaNew[,avg_gr:=mean(avg_gr),by=list(Scenario)]
SdaNew[,max_gr:=mean(max_gr),by=list(Scenario)]



SdaNew<-unique(SdaNew,by=c("Scenario"))


SdaNew<-SdaNew[,.(Scenario,min_gr,avg_gr,max_gr)]

library(reshape2)
SdaNew<-melt(SdaNew,id.vars=c("Scenario"))
SdaNew<-merge(SdaNew,list_sen)
SdaNew[,Volume:=ifelse(Actuated_St=="400 Act","Low Volume","High Volume")]

SdaNew$det_config<-as.factor(SdaNew$det_config)
SdaNew$value<-round(SdaNew$value)
SdaNew$det_config<-ordered(SdaNew$det_config,c("6ft","40ft","60ft","120ft_6ft","250ft_6ft"))

det_label<-c(`250ft_6ft`="6ft Det -250 ft Stopbar",`40ft`="40ft Det",`60ft`="60ft Det",`6ft`="6ft Det", `120ft_6ft`="6ft Det- 120ft Stopbar")
gap_label<-c(`0.1`="0.1 Sec Passage Time",`2.5`="2.5 Sec Passage Time",`5`="5 Sec Passage Time")
sg_M1<-ggplot(SdaNew[MinRecall==FALSE,],aes(x=as.factor(det_config), y=value,fill=variable))
sg_M1<-sg_M1+geom_bar(stat="identity",position =position_dodge())+facet_grid(vehComp+Volume~Gap+Occlusion,labeller=labeller(Gap=gap_label))+ggtitle("EBT Green Time")+xlab("Detector Configuration")+ylab("EBT Green Time (seconds)")
sg_M1<-sg_M1+theme_bw()
sg_M1<-sg_M1+geom_text(aes(label=value),size=3,position = position_dodge(1),vjust=-0.5)
sg_M1<-sg_M1+scale_fill_discrete("EBT Green Time (seconds)",breaks=c("min_gr","avg_gr","max_gr"),labels=c("Minimum","Average","Maximum"))
sg_M1<-sg_M1+scale_x_discrete(breaks=c("6ft","40ft","60ft","120ft_6ft","250ft_6ft"),labels=c("6ft Det","40ft Det","60ft Det","6ft Det- 120ft Stopbar","6ft Det -250 ft Stopbar"))
sg_M1<-sg_M1+theme(axis.text.x=element_text(angle=45,hjust=1))
sg_M1

fdir="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results"
setwd(fdir)

tiff("vid_sigMinMax.tiff",width=12,height=12,units='in',res = 300)
sg_M1
graphics.off()