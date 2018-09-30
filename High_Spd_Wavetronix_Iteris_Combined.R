#Purpose : Combine the results for Induction Loop and Radar Detector at High Speed Approach
rm(list=ls())
#2 Set Directory
dir1="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_IterisStudy/Scenarios"
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
if(dir==dir1){
  list_sen<-fread("List_of_senarios_Iteris_60MPh_2ln.csv")

}else{
  list_sen<-fread("List_of_senarios_Radar_60MPh_2ln.csv")
}
setnames(list_sen,"S.No","Scenario")
setnames(list_sen,"EBT Vol","EBT_Vol")
setnames(list_sen,"Passage Time","Passage_time")
list_sen[,Option:=as.character(Option)]
list_sen[Option=="1",Option:="Stopbar Detector on Separate Channel"]
list_sen[Option=="3",Option:="All Detectors on Same Channel"]
list_sen[Option=="All Detectors on Same Channel",`Det Type`:="Radar 2 -\nAll Detectors on\nSame Channel"]
list_sen[Option=="Stopbar Detector on Separate Channel",`Det Type`:="Radar 2 -\nStopbar Detector on\nSeparate Channel"]
da<-merge(da,list_sen)
gap_label<-c(`1.8`="1.8 Sec Passage time")
vol_label<-c(`400`="800 veh/hr",`800`="1600 veh/hr",`1200`="2400 veh/hr")
da[dir=="all",dir:="Intersection"]
da[dir=="ebt",dir:="EBT"]
da[dir=="nbt",dir:="NBT"]




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
SdaMax<-data.table()
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScDetectorRecordFileReader_HighSpeed.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScDetectorRecordFileReader_HighSpeed.R")

## Debugging
# for(i in 1:length(files)){
#   print(files[[i]])
#   store_SCL<-ScMaxOutFun_HgSpd(files[[i]],list_sen)
# }
store_SCL<-ScMaxOutFun_HgSpd(files,list_sen)
SdaMax<-store_SCL[[1]]

SdaMax<-SdaMax[,lapply(.SD,mean),by=list(no)]
tp<-list_sen[,.(Scenario,Option,`Det Type`)]
SdaMax<-merge(SdaMax,tp,by.x="no",by.y="Scenario")

MeanTime<-store_SCL[[2]]
MeanTime<-MeanTime[,lapply(.SD,mean),by=list(no)]
MeanTime<-merge(MeanTime,tp,by.x="no",by.y="Scenario")

#Create a data set to be used with dilemma zone:
dil_sig<-store_SCL[[1]][,.(no,run_no,NumEBT_Phase)]
# Graphs for no of vehicles trapped in dilemma zone
#file<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/dilemma_zone.csv"
if(dir==dir1){
  #file<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/dilemma_zone.csv"
  file="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_IterisStudy/dilemma_zone.csv"
} else{
  #file<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/dilemma_zone.csv"
  file="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/dilemma_zone.csv"
}

dilemma_dat<-fread(file)
dilemma_dat[,No:=.N,by=list(Scenario,Run)]
dilemma_dat<-unique(dilemma_dat,by=c("Scenario","Run"))
setnames(dilemma_dat,"No","VehDilZn")
dilemma_dat<-merge(dilemma_dat,dil_sig,by.x=c("Scenario","Run"),by.y=c("no","run_no"))
dilemma_dat[,VehDilZnPerCyc:=VehDilZn/NumEBT_Phase]
dilemma_dat[,AvgVehDilZnPerCyc:=mean(VehDilZnPerCyc),by=Scenario]
dilemma_dat<-unique(dilemma_dat,by=c("Scenario"))
dilemma_dat<-dilemma_dat[,.(Scenario,AvgVehDilZnPerCyc)]
dilemma_dat<-merge(list_sen,dilemma_dat,by="Scenario")



#********************************************************************************************
#********************************************************************************************
#********************************************************************************************
#2 Set Directory
#dir2="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/Scenarios"
dir2="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/Scenarios"
dir=dir2
setwd(dir)


#2 Reading the Node Results files   
#(Node results files sometimes cotain file for base network. Delete it manually)
files<-list.files(pattern="_Node Results_")
#For Tesing the code logic
file<-files[[1]]
#Create empty data tables
#da contains the Average values for delay, Qlen ...
da1<-data.table()
#tda contains the second by second value for delay Qlen etc .... This is used to plot
#residual queue at the start of green 
tda_ra<-data.table()
#Loop through all the node evaluation files 
#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/NodeFileReader.R")
directory=dir
buf1<-NodeFileReader(files,dir)
da1<-buf1[[1]]
tda_ra<-buf1[[2]]

#The following code is needed if multiple runs are conducted
#This code can be used to average out the values 
da1[,Qlen1:=round(mean(Qlen),digits = 2),by=list(no,dir)]
da1[,veh_delay1:=round(mean(veh_delay),digits = 2),by=list(no,dir)]
da1[,stop_delay1:=round(mean(stop_delay),digits = 2),by=list(no,dir)]
da1[,stops1:=round(mean(stops),digits = 2),by=list(no,dir)]
da1[,Qlen:=Qlen1]
da1[,veh_delay:=veh_delay1]
da1[,stop_delay:=stop_delay1]
da1[,stops:=stops1]
da1<-unique(da1,by=c("no","dir"))
da1<-da1[,.(no,dir,stops,veh_delay,Qlen)]
#3 Read List of scenarios and Merge above data table. Plot Delay and Queue
setnames(da1,"no","Scenario")
list_sen<-fread("List_of_senarios_Radar_60MPh_2ln.csv")
setnames(list_sen,"S.No","Scenario")
setnames(list_sen,"EBT Vol","EBT_Vol")
setnames(list_sen,"Passage Time","Passage_time")
list_sen[,Option:=as.character(Option)]
list_sen[Option=="1",Option:="Stopbar Detector on Separate Channel"]
list_sen[Option=="3",Option:="All Detectors on Same Channel"]
list_sen[Option=="All Detectors on Same Channel",`Det Type`:="Radar 1 -\nAll Detectors on\nSame Channel"]
list_sen[Option=="Stopbar Detector on Separate Channel",`Det Type`:="Radar 1 -\nStopbar Detector on\nSeparate Channel"]
list_sen[,Passage_time:=Passage_time-0.6]

da1<-merge(da1,list_sen)

gap_label<-c(`0.1`="0.1 Sec Passage time",`2.5`="2.5 Sec Passage time",`5`="5 Sec Passage time")
vol_label<-c(`400`="800 veh/hr",`800`="1600 veh/hr",`1200`="2400 veh/hr")
da1[dir=="all",dir:="Intersection"]
da1[dir=="ebt",dir:="EBT"]
da1[dir=="nbt",dir:="NBT"]

setnames(da1,"no","Scenario")
if(dir==dir1){
  list_sen<-fread("List_of_senarios_Iteris_60MPh_2ln.csv")
  
}else{
  list_sen<-fread("List_of_senarios_Radar_60MPh_2ln.csv")
}

setnames(list_sen,"S.No","Scenario")
setnames(list_sen,"EBT Vol","EBT_Vol")
setnames(list_sen,"Passage Time","Passage_time")
list_sen[,Option:=as.character(Option)]
list_sen[Option=="1",Option:="Stopbar Detector on Separate Channel"]
list_sen[Option=="3",Option:="All Detectors on Same Channel"]
list_sen[Option=="All Detectors on Same Channel",`Det Type`:="Radar 1 -\nAll Detectors on\nSame Channel"]
list_sen[Option=="Stopbar Detector on Separate Channel",`Det Type`:="Radar 1 -\nStopbar Detector on\nSeparate Channel"]
list_sen[,Passage_time:=Passage_time-0.6]

#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code//GGplots_Function.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/GGplots_Function.R")

#Volume Label
Vlab=c(`400`="EBT = 800 veh/hr",`800`="EBT = 1600 veh/hr",`1200`="EBT = 2400 veh/hr")
library(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")
da_comb<-rbindlist(list(da,da1))
a<-det_plots(data1=da_comb,
             x1=factor(Passage_time),
             y1=veh_delay,
             fill1=factor(dir),
             facet1=EBT_Vol~`Det Type`,
             xlab1="Effective EBT Passage Time (sec)",
             ylab1="Average Delay (veh-sec)",
             title1="Average Delay",
             vol_label=Vlab
             
)
a<-a+ylim(0,50)

b<-det_plots(data1=da_comb,
             x1=factor(Passage_time),
             y1=Qlen,
             fill1=factor(dir),
             facet1=EBT_Vol~`Det Type`,
             ylab1="Average Queue Length (ft)",
             title1="Average Queue Length",
             vol_label=Vlab
             
)
b<-b+ylim(0,150)


#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*lsa"))
file<-files[[1]]
par(mfrow=c(2,2))
SdaEBT<-data.table()
SdaEBT_ra<-scTimeRead_seed1(6,files)
#EBT Residual Queue
#6 Combine Signal data with queue data and Plot residual queues
#**********************************************************************************************************************
#Residual Queue at start of red or end of green  
tda1_ra<-tda_ra
tda1_ra[,c("st","et"):=tstrsplit(tint,"-")]
tda1_ra[,et:=as.numeric(et)]
tda1_ra<-tda1_ra[,.(et,no,run_no,dir,Qlen)]
SdaEBT_ra<-SdaEBT_ra[Indi_cor=="amber"]
tda1_ra<-tda1_ra[dir=="ebt",]
EBT_residualQ_ra<-merge(tda1_ra,SdaEBT_ra,by.x =c("no","run_no","et"),by.y=c("no","run_no","simS"))
EBT_residualQ_ra<-EBT_residualQ_ra[,.(et,Qlen,dir,no,run_no)]
EBT_residualQ_ra<-EBT_residualQ_ra[,AvgResQ:=mean(Qlen),by=list(no)]
EBT_residualQ_ra<-unique(EBT_residualQ_ra,by=c("no"))
EBT_residualQ_ra<-merge(EBT_residualQ_ra,list_sen,by.x="no",by.y="Scenario")
EBT_residualQ_comb<-rbindlist(list(EBT_residualQ,EBT_residualQ_ra))

gResQ<-det_plots_2(data1=EBT_residualQ_comb,
                   x1=factor(Passage_time),
                   y1=AvgResQ,
                   fill1=factor(Passage_time),
                   facet1=EBT_Vol~`Det Type`,
                   ylab1="Average Residual Queue Length (ft)",
                   title1="EBT Average Residual Queues",
                   vol_label=Vlab
)
gResQ<-gResQ+ylim(0,300)


# NBT Residual Queue
#7 Read all the Signal Change Files. Read the notes in Detector Project notebook to understand the format
#Finidng .lsa files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("S*lsa"))
par(mfrow=c(2,2))
SdaNBT<-data.table()
SdaNBT_ra<-scTimeRead_seed1(4,files)

#NBT
#9 Combine Signal data with queue data and Plot residual queues
#**********************************************************************************************************************
#**********************************************************************************************************************
#Residual Queue at start of red or end of green  
tda2_ra<-tda_ra
tda2_ra[,c("st","et"):=tstrsplit(tint,"-")]
tda2_ra[,et:=as.numeric(et)]
tda2_ra<-tda2_ra[,.(et,no,run_no,dir,Qlen)]
SdaNBT_ra<-SdaNBT_ra[Indi_cor=="amber"]
tda2_ra<-tda2_ra[dir=="nbt",]
NBT_residualQ_ra<-merge(tda2_ra,SdaNBT_ra,by.x =c("no","run_no","et"),by.y=c("no","run_no","simS"))
NBT_residualQ_ra<-NBT_residualQ_ra[,.(et,Qlen,dir,no,run_no)]
NBT_residualQ_ra<-NBT_residualQ_ra[,AvgResQ:=mean(Qlen),by=list(no)]
NBT_residualQ_ra<-unique(NBT_residualQ_ra,by=c("no"))
NBT_residualQ_ra<-merge(NBT_residualQ_ra,list_sen,by.x="no",by.y="Scenario")
NBT_residualQ_comb<-rbindlist(list(NBT_residualQ,NBT_residualQ_ra))

gResQ1<-det_plots_2(data1=NBT_residualQ_comb,
                    x1=factor(Passage_time),
                    y1=AvgResQ,
                    fill1=factor(Passage_time),
                    facet1=EBT_Vol~`Det Type`,
                    ylab1="Average Residual Queue Length (ft)",
                    title1="NBT Average Residual Queues",
                    vol_label=Vlab
)
gResQ1<-gResQ1+ylim(0,100)


#C Reading the SC Detector record
#Finidng .ldp files 
#Find and store the files which contains length in their names
files<-list.files(pattern=glob2rx("*.ldp"))
file<-files[[1]]
par(mfrow=c(2,2))
SdaMax_ra<-data.table()
store_SCL<-ScMaxOutFun_HgSpd(files,list_sen)
SdaMax_ra<-store_SCL[[1]]
SdaMax_ra<-SdaMax_ra[,lapply(.SD,mean),by=list(no)]
tp_ra<-list_sen[,.(Scenario,Option,`Det Type`)]
SdaMax_ra<-merge(SdaMax_ra,tp_ra,by.x="no",by.y="Scenario")
SdaMax_ra_comb<-rbindlist(list(SdaMax,SdaMax_ra))

gmax1a<-det_plots_2(data1=SdaMax_ra_comb,
                    x1=factor(PassageTime),
                    y1=Per_EBT_MaxOout,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det Type`,
                    ylab1="Percent Max Outs (%)",
                    title1="EBT Max Outs",
                    vol_label=Vlab
)
gmax1a<-gmax1a+ylim(0,100)

gmax2a<-det_plots_2(data1=SdaMax_ra_comb,
                    x1=factor(PassageTime),
                    y1=Per_NBT_MaxOout,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det Type`,
                    ylab1="Percent Max Outs (%)",
                    title1="NBT Max Outs",
                    vol_label=Vlab
)
gmax2a<-gmax2a+ylim(0,100)

MeanTime_ra<-store_SCL[[2]]
MeanTime_ra<-MeanTime_ra[,lapply(.SD,mean),by=list(no)]
MeanTime_ra<-merge(MeanTime_ra,tp_ra,by.x="no",by.y="Scenario")
MeanTime_comb<-rbindlist(list(MeanTime,MeanTime_ra))

AvgGr6<-det_plots_2(data1=MeanTime_comb,
                    x1=factor(PassageTime),
                    y1=MeanGr6,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det Type`,
                    ylab1="Average EBT Green Time (sec)",
                    title1="Average EBT Green Time",vol_label=Vlab
)
AvgGr6<-AvgGr6+ylim(0,80)

AvgGr4<-det_plots_2(data1=MeanTime_comb,
                    x1=factor(PassageTime),
                    y1=MeanGr4,
                    fill1=factor(PassageTime),
                    facet1=EBT_Vol~`Det Type`,
                    ylab1="Average NBT Green Time (sec)",
                    title1="Average NBT Green Time",vol_label=Vlab
)
AvgGr4<-AvgGr4+ylim(0,30)


if(dir==dir1){
  #file<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/dilemma_zone.csv"
  file="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/dilemma_zone.csv"
} else{
  #file<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/dilemma_zone.csv"
  file="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/dilemma_zone.csv"
}


#Create a data set to be used with dilemma zone:
dil_sig_ra<-store_SCL[[1]][,.(no,run_no,NumEBT_Phase)]
# Graphs for no of vehicles trapped in dilemma zone
dilemma_dat_ra<-fread(file)
dilemma_dat_ra[,No:=.N,by=list(Scenario,Run)]
dilemma_dat_ra<-unique(dilemma_dat_ra,by=c("Scenario","Run"))
setnames(dilemma_dat_ra,"No","VehDilZn")
dilemma_dat_ra<-merge(dilemma_dat_ra,dil_sig_ra,by.x=c("Scenario","Run"),by.y=c("no","run_no"))
dilemma_dat_ra[,VehDilZnPerCyc:=VehDilZn/NumEBT_Phase]
dilemma_dat_ra[,AvgVehDilZnPerCyc:=mean(VehDilZnPerCyc),by=Scenario]
dilemma_dat_ra<-unique(dilemma_dat_ra,by=c("Scenario"))
dilemma_dat_ra<-dilemma_dat_ra[,.(Scenario,AvgVehDilZnPerCyc)]
dilemma_dat_ra<-merge(list_sen,dilemma_dat_ra,by="Scenario")

dilemma_dat_comb<-rbindlist(list(dilemma_dat,dilemma_dat_ra))


Grdilemma<-det_plots_2(data1=dilemma_dat_comb,
                       x1=factor(Passage_time),
                       y1=AvgVehDilZnPerCyc,
                       fill1=factor(Passage_time),
                       facet1=EBT_Vol~`Det Type`,
                       ylab1="Average Number of Vehicles Trapped \nin Decision Zone Per Cycle",
                       title1="Average Number of Vehicles Trapped in Decision Zone",vol_label=Vlab
)
Grdilemma<-Grdilemma+ylim(0,2.5)


#D Saving plots in pdf for printing 
#pdf("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results/HighSpd_Comb.pdf")
pdf("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results/HighSpd_Iteris_Wavetronix.pdf")

a
b
Grdilemma
gmax1a
gmax2a
gResQ
gResQ1
AvgGr6
AvgGr4
graphics.off()

setwd("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results")
jpeg("IterisHgSpdDelay.jpeg",width=14,height=12,units = "cm",res=500)
a
graphics.off()
jpeg("IterisHgSpdQueue.jpeg",width=14,height=12,units = "cm",res=500)
b
graphics.off()
jpeg("IterisHgSpdEBMax.jpeg",width=14,height=12,units = "cm",res=500)
gmax1a
graphics.off()
jpeg("IterisHgSpdDilZn.jpeg",width=14,height=12,units = "cm",res=500)
Grdilemma
graphics.off()
jpeg("IterisHgSpdEBRes.jpeg",width=14,height=12,units = "cm",res=500)
gResQ
graphics.off()
