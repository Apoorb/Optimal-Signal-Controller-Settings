#Purpose : Combine the results for Induction Loop and Radar Detector at High Speed Approach
rm(list=ls())
#2 Set Directory
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

#********************************************************************************************
#********************************************************************************************
#********************************************************************************************
#2 Set Directory
#dir2="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/SimultaneiousGapOutSen/Scenarios"
dir2="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/SimultaneiousGapOutSen/Scenarios"
dir=dir2
setwd(dir)

source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/NodeFileReader.R")

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
list_sen<-fread("List_of_Sen_SimultaneousGap.csv")
setnames(list_sen,"S.No","Scenario")
setnames(list_sen,"EBT vol","EBT_Vol")
setnames(list_sen,"NBT vol","NBT_Vol")
setnames(list_sen,"WBT Vol","WBT_Vol")
setnames(list_sen,"Passage Time","Passage_time")
list_sen$Passage_time=list_sen$Passage_time-0.6

list_sen[,Passage_time1:=as.character(Passage_time)]
list_sen[Passage_time=="0.5",Passage_time1:="Passage time\nEBT/WBT = 0.5 sec"]
list_sen[Passage_time=="2.4",Passage_time1:="Passage time\nEBT/WBT = 2.4 sec"]
list_sen[,Option:=as.character(Option)]
list_sen[Option=="1",Option:="Stopbar Detector on Seperate Channel"]
list_sen[Option=="3",Option:="All Detectors on Same Channel"]
list_sen$V13=NULL
da1<-merge(da1,list_sen)

gap_label<-c(`1.1`="1.1 Sec Effective Passage time",`3.0`="2.5 Sec Effective Passage time")
da1[dir=="all",dir:="Intersection"]
da1[dir=="ebt",dir:="EBT"]
da1[dir=="nbt",dir:="NBT"]
da1[dir=="wbt",dir:="WBT"]


#source("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code//GGplots_Function.R")
source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/GGplots_Function.R")

da1[,EBT_Vol:=as.character(EBT_Vol)]
da1[,WBT_Vol:=as.character(WBT_Vol)]

da1[EBT_Vol==400]$EBT_Vol="EBT = 800 veh/hr"
da1[EBT_Vol==800,EBT_Vol:="EBT = 1600 veh/hr"]
da1[EBT_Vol==1200,EBT_Vol:="EBT = 2400 veh/hr"]
da1[WBT_Vol==400,WBT_Vol:="WBT = 800 veh/hr"]
da1[WBT_Vol==800,WBT_Vol:="WBT = 1600 veh/hr"]
da1[WBT_Vol==1200,WBT_Vol:="WBT = 2400 veh/hr"]



library(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")
a<-Simul_plots(data1=da1,
             x1=factor(`Gap Out`),
             y1=veh_delay,
             fill1=factor(dir),
             facet1=EBT_Vol~WBT_Vol+Passage_time1,
             xlab1="Gap",
             ylab1="Average Delay (veh-sec)",
             title1="Average Delay"
)

b<-Simul_plots(data1=da1,
               x1=factor(`Gap Out`),
               y1=Qlen,
               fill1=factor(dir),
               facet1=EBT_Vol~WBT_Vol+Passage_time1,
               xlab1="Gap",
               ylab1="Average Queue Length (ft)",
               title1="Average Queue Length"
)
b

source("/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/ScDetectorRecordFileReader_HighSpeed.R")
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
SdaMax_ra<-SdaMax_ra[,.(Per_EBT_MaxOout,Per_NBT_MaxOout,no)]
SdaMax_ra<-merge(SdaMax_ra,list_sen,by.x="no",by.y="Scenario")
SdaMax_ra[,EBT_Vol:=as.character(EBT_Vol)]
SdaMax_ra[,WBT_Vol:=as.character(WBT_Vol)]
SdaMax_ra[EBT_Vol==400]$EBT_Vol="EBT = 800 veh/hr"
SdaMax_ra[EBT_Vol==800,EBT_Vol:="EBT = 1600 veh/hr"]
SdaMax_ra[WBT_Vol==400,WBT_Vol:="WBT = 800 veh/hr"]
SdaMax_ra[WBT_Vol==800,WBT_Vol:="WBT = 1600 veh/hr"]

gmax1a<-Simul_plots(data1=SdaMax_ra,
                    x1=factor(`Gap Out`),
                    y1=Per_EBT_MaxOout,
                    fill1=factor(`Gap Out`),
                    facet1=EBT_Vol~WBT_Vol+Passage_time1,
                    xlab1="Gap",
                    ylab1="EBT Percent Max Outs (%)",
                    title1="EBT Max Outs"
)
gmax1a

gmax2a<-Simul_plots(data1=SdaMax_ra,
                    x1=factor(`Gap Out`),
                    y1=Per_NBT_MaxOout,
                    fill1=factor(`Gap Out`),
                    facet1=EBT_Vol~WBT_Vol+Passage_time1,
                    xlab1="Gap",
                    ylab1="NBT Percent Max Outs (%)",
                    title1="NBT Max Outs"
)
gmax2a


MeanTime_ra<-store_SCL[[2]]
MeanTime_ra<-MeanTime_ra[,lapply(.SD,mean),by=list(no)]
MeanTime_ra$PassageTime=NULL
MeanTime_ra$EBT_Vol=NULL
MeanTime_ra<-merge(MeanTime_ra,list_sen,by.x="no",by.y="Scenario")
MeanTime_ra[,EBT_Vol:=as.character(EBT_Vol)]
MeanTime_ra[,WBT_Vol:=as.character(WBT_Vol)]
MeanTime_ra[EBT_Vol==400]$EBT_Vol="EBT = 800 veh/hr"
MeanTime_ra[EBT_Vol==800,EBT_Vol:="EBT = 1600 veh/hr"]
MeanTime_ra[WBT_Vol==400,WBT_Vol:="WBT = 800 veh/hr"]
MeanTime_ra[WBT_Vol==800,WBT_Vol:="WBT = 1600 veh/hr"]

AvgGr6<-Simul_plots(data1=MeanTime_ra,
                    x1=factor(`Gap Out`),
                    y1=MeanGr6,
                    fill1=factor(`Gap Out`),
                    facet1=EBT_Vol~WBT_Vol+Passage_time1,
                    xlab1="Gap",
                    ylab1="Average EBT Green Time (sec)",
                    title1="Average EBT Green Time"
)
AvgGr6


AvgGr4<-Simul_plots(data1=MeanTime_ra,
                    x1=factor(`Gap Out`),
                    y1=MeanGr4,
                    fill1=factor(`Gap Out`),
                    facet1=EBT_Vol~WBT_Vol+Passage_time1,
                    xlab1="Gap",
                    ylab1="Average NBT Green Time (sec)",
                    title1="Average EBT Green Time"
)
AvgGr4


#file<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/SimultaneiousGapOutSen/dilemma_zone.csv"
  file="/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/SimultaneiousGapOutSen/dilemma_zone.csv"


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
dilemma_dat_ra[,EBT_Vol:=as.character(EBT_Vol)]
dilemma_dat_ra[,WBT_Vol:=as.character(WBT_Vol)]
dilemma_dat_ra[EBT_Vol==400]$EBT_Vol="EBT = 800 veh/hr"
dilemma_dat_ra[EBT_Vol==800,EBT_Vol:="EBT = 1600 veh/hr"]
dilemma_dat_ra[WBT_Vol==400,WBT_Vol:="WBT = 800 veh/hr"]
dilemma_dat_ra[WBT_Vol==800,WBT_Vol:="WBT = 1600 veh/hr"]

Grdilemma<-Simul_plots(data1=dilemma_dat_ra,
                    x1=factor(`Gap Out`),
                    y1=AvgVehDilZnPerCyc,
                    fill1=factor(`Gap Out`),
                    facet1=EBT_Vol~WBT_Vol+Passage_time1,
                    xlab1="Gap",
                    ylab1="Average Number of Vehicles Trapped\nin Decision Zone Per Cycle (EBT)",
                    title1="Average Number of Vehicles Trapped in Decision Zone"
)


#D Saving plots in pdf for printing 
#pdf("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results/HighSpd_Comb.pdf")
mainDir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/results"
subDir<-"SimultRes"
dir.create(file.path(mainDir, subDir))
tem<-file.path(mainDir, subDir)

pdf(file.path(tem,"SimRes.pdf"))
a
b
Grdilemma+theme(legend.position="none")
gmax1a+theme(legend.position="none")
gmax2a+theme(legend.position="none")
AvgGr6+theme(legend.position="none")
AvgGr4+theme(legend.position="none")
graphics.off()

setwd(tem)
jpeg("RAdarHgSpdDelay.jpeg",width=18,height=12,units = "cm",res=500)
a
graphics.off()
jpeg("RAdarSimulQueue.jpeg",width=18,height=12,units = "cm",res=500)
b
graphics.off()
jpeg("RAdarSimulEBMax.jpeg",width=16,height=12,units = "cm",res=500)
gmax1a+theme(legend.position="none")
graphics.off()
jpeg("RAdarSimulDilZn.jpeg",width=16,height=12,units = "cm",res=500)
Grdilemma+theme(legend.position="none")
graphics.off()
jpeg("RAdarSimulAvgGr4.jpeg",width=16,height=12,units = "cm",res=500)
AvgGr4+theme(legend.position="none")
graphics.off()
jpeg("RAdarSimulAvgGr6.jpeg",width=18,height=12,units = "cm",res=500)
AvgGr6+theme(legend.position="none")
graphics.off()
