# Script for processing the headways
#File Name: headway_summary
#Created by: Apoorba Bibeka
#Creation date:7 Feb 2016
#Date modified : Oct 2017
#Purpose:To get the flow rate at a section 
#Last executed:
Sys.time()

#1 Housekeeping
ls()
rm(list=ls())
ls()


#Set the current directory to the directory where all the data files are kept
#setwd("/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/PerfMeasures")

setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_Scenario_2ln/Scenarios")

#Load data.table package for fread to work
library(data.table)

#fread by default create data table. Change the data table to data frame
options(datatable.fread.datatable=FALSE)

#Find and store the files which contains length in their names
files<-list.files(pattern=".mer")


#Skip the first row   fread_  skip=1
#File has header 		fread header=true

#i index of list li. Stores the number of the senario
i<-1

#li

library(plyr)

#Iterate over all the files 

i<-1
final_data<-data.table()
fdat<-data.table()
file<-files[1]
for(file in files){
  f=strsplit(file,"_")[[1]][1]
  no=strsplit(f,"Scenario")[[1]][2]
  f2=strsplit(file,"_")[[1]][2]
  run=strsplit(f2,"[.]")[[1]][1]
  run=as.numeric(run)
  if(no %in% c(1,9,17,25,33,41) & run==1){
		data_f<-fread(file)
		
		colnames(data_f)[2]<-"time_stamp"
		
		#Calculation for all lane 
		time_stamp<-data_f[,"time_stamp"]
		time_stamp<-time_stamp[(time_stamp!=-1)]
		time_stamp <-sort(time_stamp)
		headway<-diff(time_stamp)
		dat<-data.table(headway)
		scenario<-as.numeric(no)
  	run=as.numeric(run)
		dat$Scenario<-scenario
		dat$Run<-run
		# dat$Lane<-12
    dat[,avg_head_all:=round(mean(headway),digits=2)]
    # dat<-unique(dat,by="Scenario")
    setnames(dat,"headway","Head_all")
		
    n_rows<-nrow(dat)
    bu<-data.table(n_rows,no)
    fdat<-rbindlist(list(fdat,bu))
    
		#Calculation for right lane
		time_stamp<-data_f[data_f$Measurem.==1,"time_stamp"]
		time_stamp<-time_stamp[(time_stamp!=-1)]
		time_stamp <-sort(time_stamp)
		headway<-diff(time_stamp)
		dat1<-data.table(headway)
		scenario<-as.numeric(no)
		run=as.numeric(run)
		dat1$Scenario<-scenario
		dat1$Run<-run
		dat1$Lane<-1
		dat1[,avg_head_right:=round(mean(headway),digits=2)]
		# dat1<-unique(dat1,by="Scenario")
		setnames(dat1,"headway","Head_right")
		
		
		
		
		#Calculation for left lane
		time_stamp<-data_f[data_f$Measurem.==2,"time_stamp"]
		time_stamp<-time_stamp[(time_stamp!=-1)]
		time_stamp <-sort(time_stamp)
		headway<-diff(time_stamp)
		dat2<-data.table(headway)
		scenario<-as.numeric(no)
		run=as.numeric(run)
		dat2$Scenario<-scenario
		dat2$Run<-run
		dat2$Lane<-2
		dat2[,avg_head_left:=round(mean(headway),digits=2)]
		# dat2<-unique(dat2,by="Scenario")
		setnames(dat2,"headway","Head_left")
		
		temp<-as.data.table(cbind(dat,dat1[,.(Head_right,avg_head_right)],dat2[,.(Head_left,avg_head_left)]))
		final_data<-rbindlist(list(final_data,temp))
		
		time_stamp<-NULL
		dat<-NULL
		bi<-NULL
		bin<-NULL
		headway<-NULL
  }
		
	}
	
		#input the list of sen
	  list_sen<-fread("List_of_senarios_2ln.csv",header=T)
	   final_data<-merge(final_data,list_sen, by.x="Scenario",by.y="S.No")
	   # final_data[,avg_head_all:=mean(avg_head_all),by=list(Scenario)]
	   # final_data[,avg_head_right:=mean(avg_head_right),by=list(Scenario)]
	   # final_data[,avg_head_left:=mean(avg_head_left),by=list(Scenario)]
	   
	   final_data[,EBT:= `EBT Vol Ln1`+`EBT Vol Ln2`]
      # unique(final_data,by=c("EBT","Ln Dist"))
      
      
      require(ggplot2)
      g1<-ggplot(data=final_data,aes(x=Head_right))+geom_histogram(breaks=seq(0,60,by=0.5),col="blue",fill="blue")+facet_grid(EBT~`Ln Dist`)+theme_bw()+labs(x="Headway (0.5 sec bins)",title="Headway Right Lane (50, 75%)")
      g2<-ggplot(data=final_data,aes(x=Head_left))+geom_histogram(breaks=seq(0,60,by=0.5),col="blue",fill="blue")+facet_grid(EBT~`Ln Dist`)+theme_bw()+labs(x="Headway (0.5 sec bins)",title="Headway Left Lane (50, 25%)")
      g3<-ggplot(data=final_data,aes(x=Head_all))+geom_histogram(breaks=seq(0,25,by=0.5),col="blue",fill="blue")+facet_grid(EBT~`Ln Dist`)+theme_bw()+labs(x="Headway (0.5 sec bins)",title="Headway All Lanes")
      pdf("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/headway.pdf")
      g1
      g2
      g3
      graphics.off()
		x= final_data
		#file="/Users/Apoorb/Dropbox/ATLAS Project/Simulation/CACC Truck/8 Mile Network/Processed_Results/Flow_rate_summary.csv"
	  file="C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/headway_Sum.csv"
		write.table(x,file,append=F,row.names=F,sep=",")




