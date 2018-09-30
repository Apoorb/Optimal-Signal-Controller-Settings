# Read the SC detector record file and find the number of max outss
ScMaxOutFun_HgSpd<-function(files,list_sen){
    file<-files[1]
    require(data.table)
    Fda<-data.table()
    Mean_da<-data.table()
    for(file in files){
      #**************************************
      #Extracting scenario number 
      temp=strsplit(file,"_")[[1]][1]
      t2<-strsplit(file,"_")[[1]][3]
      run_no= strsplit(t2,"[.]")[[1]][1]
      run_no<-as.numeric(run_no)
      #adv_Dir<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/Scenarios"
      #adv_Dir1<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/Scenarios"
     
       adv_Dir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/Scenarios"
      adv_Dir1<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/Scenarios"
      adv_Dir2<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_IterisStudy/Scenarios"  
      
       #Simultaneous gap
      SimuDir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/SimultaneiousGapOutSen/Scenarios"
      
      if(getwd()==adv_Dir |getwd()==adv_Dir1|getwd()==adv_Dir2| getwd()==SimuDir){
        no<-strsplit(temp,"Scenario")[[1]][2]
      }else{
        temp=strsplit(file,"_")[[1]][1]
        no=strsplit(temp," ")[[1]][4]
      }
      no=as.numeric(no)
      #**************************************
      assign("tempDa",fread(file,header=FALSE,sep=" ",skip=14))
      #********************************************************************************
      relSen<-list_sen[Scenario==no,]

      if(getwd()==adv_Dir |getwd()==adv_Dir1|getwd()==adv_Dir2|getwd()==SimuDir){
        EBT_MaxGreen<-relSen$MaxGr6
        NBT_MaxGreen<-relSen$MaxGr4
        EBT_Vol<- relSen$EBT_Vol
        PassageTime<-relSen$Passage_time
      }
      
      #Getting the data for signal group 6 (EBT)
      #Migth need the data for sg 4 in future
      #Rename the columns
      if(ncol(tempDa)==9){
        colnames(tempDa)<-c("simS","Det4","Det6","Sg4","Sg6","Det62","Det2","Sg2","Det22")
      }else{colnames(tempDa)<-c("simS","Det4","Det6","Sg4","Sg6","Det62")
}
      #Change the data format: VISSIM uses "." and "|"
      tempDa[,Sg4:= ifelse(Sg4==".",'R',ifelse(Sg4=="I",'G',"Y"))]
      tempDa[,Sg6:= ifelse(Sg6==".",'R',ifelse(Sg6=="I",'G',"Y"))]
      tempDa[,Det4:= ifelse(Det4==".",FALSE,ifelse(Det4=="|",TRUE,TRUE))]
      tempDa[,Det6:= ifelse(Det6==".",FALSE,ifelse(Det6=="|",TRUE,TRUE))]
      tempDa[,Det62:= ifelse(Det62==".",FALSE,ifelse(Det62=="|",TRUE,TRUE))]
      tempDa[(Det6==FALSE& Det62==TRUE),Det6:=TRUE]
      
      #First 300 sec are warmup period
      tempDa<-tempDa[simS>=300.1,]
      #Find the state change for detector and signal 
      tempDa[, c("ChDet4","ChDet6","ChSg4","ChSg6") := lapply(.SD, function(x) c(0, head(x, -1L) != tail(x, -1L))),.SDcols=c("Det4","Det6","Sg4","Sg6")]
      #Keep the data pertaining to when signal is green
      tempDa<-tempDa[Sg4=='G'| Sg6=='G']
      #Cumulative sum of ChSgX would assign unique IDs to Green phases in different cycle
      tempDa[,UpChSg4:=cumsum(ChSg4)]   
      tempDa[,UpChSg6:=cumsum(ChSg6)]
      #Cumulative sum of ChSgX would assign unique IDs to Detector status. 
      tempDa[,UpChDet4:=cumsum(ChDet4)]   
      tempDa[,UpChDet6:=cumsum(ChDet6)]
      
      
      #Remove first and last phase. If 1st phase in 4 than the remove rows based on SG6
      #If last phase is 4 than remove rows based on Sg 4
      if(tempDa[1,UpChSg4]==0 &tempDa[1,UpChSg6]==0){
        if(tempDa[1,Sg4]=='G'){
          tempDa<-tempDa[UpChSg6!=min(UpChSg6),]
        }else{
          
          tempDa<-tempDa[UpChSg4!=min(UpChSg4),]
        }
      }
     
      if(tempDa[nrow(tempDa),Sg4]=='G'){
        tempDa<-tempDa[UpChSg4!=max(UpChSg4),]
      }else{
        tempDa<-tempDa[UpChSg6!=max(UpChSg6),]
      }
      
      #Consider EBT
      #Subset data pertaining to EBT
      EBT_max_out<-tempDa[Sg6=='G',.(Sg6,UpChSg6,Det4,Det6,UpChDet4,UpChDet6)]
      #Find the green duration
      EBT_max_out[,GDur6:=0.1*(.N),by=UpChSg6]
      #Find the duration for which a detector was on and off during Green
      EBT_max_out[,DetDur6:=0.1*(.N),by=list(UpChSg6,UpChDet6)]
      #Find when the detector was On and Off. Average when detector is off is 0 and the average 
      #when detector is on is 1
      EBT_max_out[,DetStat6:=mean(Det6),by=list(UpChSg6,UpChDet6)]
      #Get the data for each cycle and unique detector state
      EBT_max_out<-unique(EBT_max_out,by=c("UpChSg6","UpChDet6"))
      EBT_max_out<-EBT_max_out[,.(UpChSg6,UpChDet6,DetStat6,DetDur6,GDur6)]
      #EBT_rev contains the data for last detector state for every cycle
      EBT_rev<-EBT_max_out[,lapply(.SD,max),,by=UpChSg6,.SDcols="UpChDet6"]
      EBT_rev<-merge(EBT_rev,EBT_max_out[,.(UpChSg6,UpChDet6,GDur6,DetStat6,DetDur6)])
      NumEBT_Phase<-length(EBT_rev$UpChSg6)
      #For a max out to happen the green time should be atleast greater than max green
      EBT_rev<-EBT_rev[GDur6>=EBT_MaxGreen]
      #For max out to occur either the detector is on and still the signal changers 
      #OR the detector is off but the off duration is less than the passage time
      EBT_rev<-EBT_rev[DetStat6==1 | (DetStat6==0 &DetDur6<PassageTime),]
      
      Freq_EBT_MaxOut<-length(EBT_rev$UpChSg6)
      Per_EBT_MaxOout<-100*Freq_EBT_MaxOut/NumEBT_Phase
      
      NBT_max_out<-tempDa[Sg4=='G',.(Sg4,UpChSg4,Det4,Det6,UpChDet4,UpChDet6)]
      NBT_max_out[,GDur4:=0.1*(.N),by=UpChSg4]
      NBT_max_out[,DetDur4:=0.1*(.N),by=list(UpChSg4,UpChDet4)]
      NBT_max_out[,DetStat4:=mean(Det4),by=list(UpChSg4,UpChDet4)]
      NBT_max_out<-unique(NBT_max_out,by=c("UpChSg4","UpChDet4"))
      NBT_max_out<-NBT_max_out[,.(UpChSg4,UpChDet4,DetStat4,DetDur4,GDur4)]
      NBT_rev<-NBT_max_out[,lapply(.SD,max),,by=UpChSg4,.SDcols="UpChDet4"]
      NBT_rev<-merge(NBT_rev,NBT_max_out[,.(UpChSg4,UpChDet4,GDur4,DetStat4,DetDur4)])
      NumNBT_Phase<-length(NBT_rev$UpChSg4)
      NBT_rev<-NBT_rev[GDur4>=NBT_MaxGreen]
      
      Freq_NBT_MaxOut<-length(NBT_rev$UpChSg4)
      Per_NBT_MaxOout<-100*Freq_NBT_MaxOut/NumNBT_Phase
      da<-data.table(cbind(no,run_no,Freq_EBT_MaxOut,NumEBT_Phase,Per_EBT_MaxOout,Freq_NBT_MaxOut,NumNBT_Phase,Per_NBT_MaxOout,PassageTime,EBT_Vol))
      Fda<-rbindlist(list(Fda,da))
      
      #EBT_max_out contains repeated values. Get unique
      EBT_avgGr<-unique(EBT_max_out,by="UpChSg6")
      NBT_avgGr<-unique(NBT_max_out,by="UpChSg4")
      MeanGr6<-round(mean(EBT_avgGr$GDur6),digits=2)
      #Red for signal 4 = Green for signal 6 + yellow
      MeanRe4<-round(mean(EBT_avgGr$GDur6+5),digits=2)
      MeanGr4<-round(mean(NBT_avgGr$GDur4),digits=2)
      #Red for signal 6 = Green for signal 4 + yellow
      MeanRe6<-round(mean(NBT_avgGr$GDur4+4),digits=2)
      da_nw<-data.table(cbind(no,PassageTime,EBT_Vol,MeanGr6,MeanGr4,MeanRe6,MeanRe4))
      Mean_da<-rbindlist(list(Mean_da,da_nw))
    }
    list(Fda,Mean_da)
}