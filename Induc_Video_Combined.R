#Purpose; Combine the Reults for Inducion loop and video detectors

rm(list=ls())
#2 Set Directory
dir1="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_Scenario/Scenarios"
dir1="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_Scenario/Scenarios"
dir=dir1

#1 Loading the Libraries
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
#2 Reading the Node Results files   
#(Node results files sometimes cotain file for base network. Delete it manually)
#********************************************************************************************************
#Finidng att files 
#Find and store the files which contains length in their names
setwd(dir)
files<-list.files(pattern="_Node Results_")
#For Tesing the code logic
file<-files[[1]]
#Create empty data tables
#da contains the Average values for delay, Qlen ...
da<-data.table()
#tda contains the second by second value for delay Qlen etc .... This is used to plot
#residual queue at the start of green 
tda<-data.table()
#Loop through all the node evaluation files 
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/NodeFileReader.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/NodeFileReader.R")
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code//GGplots_Function.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/GGplots_Function.R")

directory=dir
buf1<-NodeFileReader(files,dir)
da<-buf1[[1]]
tda<-buf1[[2]]

#The following code is needed if multiple runs are conducted
#This code can be used to average out the values 
da[,Qlen1:=round(mean(Qlen),digits = 2),by=list(no,dir)]
da[,veh_delay1:=round(mean(veh_delay),digits = 2),by=list(no,dir)]
da[,stop_delay1:=round(mean(stop_delay),digits = 2),by=list(no,dir)]
da[,stops1:=round(mean(stops),digits = 2),by=list(no,dir)]
da[,Qlen:=Qlen1]
da[,veh_delay:=veh_delay1]
da[,stop_delay:=stop_delay1]
da[,stops:=stops1]
da<-unique(da,by=c("no","dir"))
da<-da[,.(no,dir,stops,veh_delay,Qlen)]
#3 Read List of scenarios and Merge above data table. Plot Delay and Queue
setnames(da,"no","Scenario")
list_sen<-fread("Scenario_Through_STbar.csv")
da<-merge(da,list_sen)
gap_label<-c(`0.1`="0.1 Sec Passage time",`2.5`="2.5 Sec Passage time",`5`="5 Sec Passage time")
vol_label<-c(`400`="400 veh/hr",`800`="800 veh/hr",`1200`="1200 veh/hr")
da[dir=="all",dir:="Intersection"]
da[dir=="ebt",dir:="EBT"]
da[dir=="nbt",dir:="NBT"]
if(dir1==dir){
  det_type<-"Induction Loop Detectors"
}else{
  det_type<-"Video Detectors"
}
library(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")

#EBT
#4 Read all the Signal Change Files. Read the notes in Detector Project notebook to understand the format
#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*lsa"))
file<-files[[1]]
par(mfrow=c(2,2))
SdaEBT<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScTimingReader.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScTimingReader.R")

SdaEBT<-scTimeRead_seed1(6,files)


#EBT Residual Queue
#6 Combine Signal data with queue data and Plot residual queues
#**********************************************************************************************************************
#Residual Queue at start of red or end of green  
tda1<-tda
tda1[,c("st","et"):=tstrsplit(tint,"-")]
tda1[,et:=as.numeric(et)]
tda1<-tda1[,.(et,no,run_no,dir,Qlen)]
SdaEBT<-SdaEBT[Indi_cor=="amber"]
tda1<-tda1[dir=="ebt",]
EBT_residualQ<-merge(tda1,SdaEBT,by.x =c("no","run_no","et"),by.y=c("no","run_no","simS"))
EBT_residualQ<-EBT_residualQ[,.(et,Qlen,dir,no,run_no)]
EBT_residualQ<-EBT_residualQ[,AvgResQ:=mean(Qlen),by=list(no)]
EBT_residualQ<-unique(EBT_residualQ,by=c("no"))
EBT_residualQ<-merge(EBT_residualQ,list_sen,by.x="no",by.y="Scenario")

# NBT Residual Queue
#7 Read all the Signal Change Files. Read the notes in Detector Project notebook to understand the format
#Reading files for phase 4 (NBT)
#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*lsa"))
par(mfrow=c(2,2))
SdaNBT<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScTimingReader.R")
SdaNBT<-scTimeRead_seed1(4,files)

#NBT
#9 Combine Signal data with queue data and Plot residual queues
#**********************************************************************************************************************
#**********************************************************************************************************************
#Residual Queue at start of red or end of green  
tda2<-tda
tda2[,c("st","et"):=tstrsplit(tint,"-")]
tda2[,et:=as.numeric(et)]
tda2<-tda2[,.(et,no,run_no,dir,Qlen)]
SdaNBT<-SdaNBT[Indi_cor=="amber"]
tda2<-tda2[dir=="nbt",]
NBT_residualQ<-merge(tda2,SdaNBT,by.x =c("no","run_no","et"),by.y=c("no","run_no","simS"))
NBT_residualQ<-NBT_residualQ[,.(et,Qlen,dir,no,run_no)]
NBT_residualQ<-NBT_residualQ[,AvgResQ:=mean(Qlen),by=list(no)]
NBT_residualQ<-unique(NBT_residualQ,by=c("no"))
NBT_residualQ<-merge(NBT_residualQ,list_sen,by.x="no",by.y="Scenario")

#C Reading the SC Detector record
#Finidng .ldp files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("*.ldp"))
file<-files[[1]]
par(mfrow=c(2,2))
listSen<-fread("Scenario_Through_STbar.csv")
SdaMax<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScDetectorRecordFileReader.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScDetectorRecordFileReader.R")

store_SCL<-ScMaxOutFun(files,listSen)
SdaMax<-store_SCL[[1]]
SdaMax<-SdaMax[,lapply(.SD,mean),by=list(no)]

MeanTime<-store_SCL[[2]]
MeanTime<-MeanTime[,lapply(.SD,mean),by=list(no)]


#********************************************************************************************
#********************************************************************************************
dir2="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_Video/Scenarios"
dir2="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_video/Scenarios"
dir=dir2
#2 Reading the Node Results files   
#(Node results files sometimes cotain file for base network. Delete it manually)
#********************************************************************************************************
#Finidng att files 
#Find and store the files which contains length in their names
setwd(dir)

files<-list.files(pattern="_Node Results_")
#For Tesing the code logic
file<-files[[1]]
#Create empty data tables
#da contains the Average values for delay, Qlen ...
da_vid<-data.table()
#tda contains the second by second value for delay Qlen etc .... This is used to plot
#residual queue at the start of green 
tda_vid<-data.table()
#Loop through all the node evaluation files 
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/NodeFileReader.R")
directory=dir
buf1<-NodeFileReader(files,dir)
da_vid<-buf1[[1]]
tda_vid<-buf1[[2]]

#The following code is needed if multiple runs are conducted
#This code can be used to average out the values 
da_vid[,Qlen1:=round(mean(Qlen),digits = 2),by=list(no,dir)]
da_vid[,veh_delay1:=round(mean(veh_delay),digits = 2),by=list(no,dir)]
da_vid[,stop_delay1:=round(mean(stop_delay),digits = 2),by=list(no,dir)]
da_vid[,stops1:=round(mean(stops),digits = 2),by=list(no,dir)]
da_vid[,Qlen:=Qlen1]
da_vid[,veh_delay:=veh_delay1]
da_vid[,stop_delay:=stop_delay1]
da_vid[,stops:=stops1]
da_vid<-unique(da_vid,by=c("no","dir"))
da_vid<-da_vid[,.(no,dir,stops,veh_delay,Qlen)]

#3 Read List of scenarios and Merge above data table. Plot Delay and Queue
setnames(da_vid,"no","Scenario")
list_sen<-fread("Scenario_Through_STbar.csv")
da_vid<-merge(da_vid,list_sen)


gap_label<-c(`0.1`="0.1 Sec Passage time",`2.5`="2.5 Sec Passage time",`5`="5 Sec Passage time")
vol_label<-c(`400`="400 veh/hr",`800`="800 veh/hr",`1200`="1200 veh/hr")
da_vid[dir=="all",dir:="Intersection"]
da_vid[dir=="ebt",dir:="EBT"]
da_vid[dir=="nbt",dir:="NBT"]
if(dir1==dir){
  det_type<-"Induction Loop Detectors"
}else{
  det_type<-"Video Detectors"
}

da[,Det_Type:="Induction Loop"]
da_vid[,Det_Type:="Video"]
da_comb<-rbindlist(list(da,da_vid))
library(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")
Vlab=c(`400`="EBT = 400 veh/hr",`800`="EBT = 800 veh/hr",`1200`="EBT = 1200 veh/hr")

a<-det_plots(data1=da_comb,
                  x1=factor(Passage_time),
                  y1=veh_delay,
                  fill1=factor(dir),
                  facet1=EBT_Vol~Det_Type,
                  ylab1="Average Delay (veh-sec)",
                  title1="Average Delay",vol_label=Vlab
)
a<-a+ylim(0,50)


b<-det_plots(data1=da_comb,
             x1=factor(Passage_time),
             y1=Qlen,
             fill1=factor(dir),
             facet1=EBT_Vol~Det_Type,
             ylab1="Average Queue Length (ft)",
             title1="Average Queue Length",
             vol_label=Vlab
             
)
b<-b+ylim(0,1200)


#EBT
#4 Read all the Signal Change Files. Read the notes in Detector Project notebook to understand the format
#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*lsa"))
file<-files[[1]]
par(mfrow=c(2,2))
SdaEBT_vid<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScTimingReader.R")
SdaEBT_vid<-scTimeRead_seed1(6,files)

#EBT Residual Queue
#6 Combine Signal data with queue data and Plot residual queues
#**********************************************************************************************************************
#Residual Queue at start of red or end of green  
tda1_vid<-tda_vid
tda1_vid[,c("st","et"):=tstrsplit(tint,"-")]
tda1_vid[,et:=as.numeric(et)]
tda1_vid<-tda1_vid[,.(et,no,run_no,dir,Qlen)]
SdaEBT_vid<-SdaEBT_vid[Indi_cor=="amber"]
tda1_vid<-tda1_vid[dir=="ebt",]
EBT_residualQ_vid<-merge(tda1_vid,SdaEBT_vid,by.x =c("no","run_no","et"),by.y=c("no","run_no","simS"))
EBT_residualQ_vid<-EBT_residualQ_vid[,.(et,Qlen,dir,no,run_no)]
EBT_residualQ_vid<-EBT_residualQ_vid[,AvgResQ:=mean(Qlen),by=list(no)]
EBT_residualQ_vid<-unique(EBT_residualQ_vid,by=c("no"))
EBT_residualQ_vid<-merge(EBT_residualQ_vid,list_sen,by.x="no",by.y="Scenario")
EBT_residualQ[,Det_Type:="Induction Loop"]
EBT_residualQ_vid[,Det_Type:="Video"]
EBT_residualQ_comb<-rbindlist(list(EBT_residualQ,EBT_residualQ_vid))

gResQ<-det_plots_2(data1=EBT_residualQ_comb,
                   x1=factor(Passage_time),
                   y1=AvgResQ,
                   fill1=factor(Passage_time),
                   facet1=EBT_Vol~Det_Type,
                   ylab1="Average Residual Queue Length (ft)",
                   title1="EBT Average Residual Queues",
                   vol_label=Vlab
)
gResQ<-gResQ+ylim(0,1200)

# NBT Residual Queue
#7 Read all the Signal Change Files. Read the notes in Detector Project notebook to understand the format
#Reading files for phase 4 (NBT)
#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*lsa"))
par(mfrow=c(2,2))
SdaNBT_vid<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScTimingReader.R")
SdaNBT_vid<-scTimeRead_seed1(4,files)

#NBT
#9 Combine Signal data with queue data and Plot residual queues
#**********************************************************************************************************************
#**********************************************************************************************************************
#Residual Queue at start of red or end of green  
tda2_vid<-tda_vid
tda2_vid[,c("st","et"):=tstrsplit(tint,"-")]
tda2_vid[,et:=as.numeric(et)]
tda2_vid<-tda2_vid[,.(et,no,run_no,dir,Qlen)]
SdaNBT_vid<-SdaNBT_vid[Indi_cor=="amber"]
tda2_vid<-tda2_vid[dir=="nbt",]
NBT_residualQ_vid<-merge(tda2_vid,SdaNBT_vid,by.x =c("no","run_no","et"),by.y=c("no","run_no","simS"))
NBT_residualQ_vid<-NBT_residualQ_vid[,.(et,Qlen,dir,no,run_no)]
NBT_residualQ_vid<-NBT_residualQ_vid[,AvgResQ:=mean(Qlen),by=list(no)]
NBT_residualQ_vid<-unique(NBT_residualQ_vid,by=c("no"))
NBT_residualQ_vid<-merge(NBT_residualQ_vid,list_sen,by.x="no",by.y="Scenario")
NBT_residualQ[,Det_Type:="Induction Loop"]
NBT_residualQ_vid[,Det_Type:="Video"]
NBT_residualQ_comb<-rbindlist(list(NBT_residualQ,NBT_residualQ_vid))



gResQ1<-det_plots_2(data1=NBT_residualQ_comb,
                    x1=factor(Passage_time),
                    y1=AvgResQ,
                    fill1=factor(Passage_time),
                    facet1=EBT_Vol~Det_Type,
                    ylab1="Average Residual Queue Length (ft)",
                    title1="NBT Average Residual Queues",
                    vol_label=Vlab
)
gResQ1<-gResQ1+ylim(0,1200)


#C Reading the SC Detector record
#Finidng .ldp files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("*.ldp"))
file<-files[[1]]
par(mfrow=c(2,2))
listSen<-fread("Scenario_Through_STbar.csv")
SdaMax_vid<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScDetectorRecordFileReader.R")
store_SCL<-ScMaxOutFun(files,listSen)
SdaMax_vid<-store_SCL[[1]]
SdaMax_vid<-SdaMax_vid[,lapply(.SD,mean),by=list(no)]
SdaMax[,Det_Type:="Induction Loop"]
SdaMax_vid[,Det_Type:="Video"]
SdaMax_comb<-rbindlist(list(SdaMax,SdaMax_vid))

gmax1a<-det_plots_2(data1=SdaMax_comb,
                    x1=factor(PassageTime),
                    y1=Per_EBT_MaxOout,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det_Type`,
                    ylab1="Percent Max Outs (%)",
                    title1="EBT Max Outs",
                    vol_label=Vlab
)
gmax1a<-gmax1a+ylim(0,100)

gmax2a<-det_plots_2(data1=SdaMax_comb,
                    x1=factor(PassageTime),
                    y1=Per_NBT_MaxOout,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det_Type`,
                    ylab1="Percent Max Outs (%)",
                    title1="NBT Max Outs",
                    vol_label=Vlab
)
gmax2a<-gmax2a+ylim(0,100)

MeanTime_vid<-store_SCL[[2]]
MeanTime_vid<-MeanTime_vid[,lapply(.SD,mean),by=list(no)]
MeanTime[,Det_Type:="Induction Loop"]
MeanTime_vid[,Det_Type:="Video"]
MeanTime_comb<-rbindlist(list(MeanTime,MeanTime_vid))


AvgGr6<-det_plots_2(data1=MeanTime_comb,
                    x1=factor(PassageTime),
                    y1=MeanGr6,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det_Type`,
                    ylab1="Average EBT Green Time (sec)",
                    title1="Average EBT Green Time",vol_label=Vlab
)
AvgGr6<-AvgGr6+ylim(0,80)

AvgGr4<-det_plots_2(data1=MeanTime_comb,
                    x1=factor(PassageTime),
                    y1=MeanGr4,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det_Type`,
                    ylab1="Average NBT Green Time (sec)",
                    title1="Average NBT Green Time",vol_label=Vlab
)
AvgGr4<-AvgGr4+ylim(0,30)

#pdf("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results/Induc_Video_Comb.pdf")
pdf("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results/Induc_Video_Comb.pdf")
a
b
gResQ
gResQ1
gmax1a
gmax2a
AvgGr6
AvgGr4
graphics.off()


setwd("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results")
tiff("ONeLnDelay.tiff",width=12,height=12,units = "cm",res=500)
a
graphics.off()
tiff("ONeLnQueue.tiff",width=12,height=12,units = "cm",res=500)
b
graphics.off()
tiff("ONeoLnEBMax.tiff",width=12,height=12,units = "cm",res=500)
gmax1a
graphics.off()
tiff("ONeLnNBMax.tiff",width=12,height=12,units = "cm",res=500)
gmax2a
graphics.off()
tiff("ONeLnEBRes.tiff",width=12,height=12,units = "cm",res=500)
gResQ
graphics.off()
tiff("OneLnNBRes.tiff",width=12,height=12,units = "cm",res=500)
gResQ1
graphics.off()
