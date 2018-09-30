


#1 Fuction for reading signal controller file with seed 1
scTimeRead_seed1<-function(phaseNo,files){
  require(data.table)
  da<-data.table()
  for(file in files){
              #**************************************
              #Extracting scenario number 
              temp=strsplit(file,"_")[[1]][1]
              t2=strsplit(file,"_")[[1]][2]
              run_no=strsplit(t2,"[.]")[[1]][1]
              run_no=as.numeric(run_no)
              if(grepl("Scenario",temp)){
                no=strsplit(temp,"Scenario")[[1]][2]
              }else{
                no=strsplit(temp,"S")[[1]][2]
              }
              no=as.numeric(no)
              #**************************************
              assign("sig_data",fread(file,header=FALSE,sep=";"))
              #********************************************************************************
              #Getting the data for signal group 6 (EBT)
              #Migth need the data for sg 4 in future
              SGr=phaseNo
              sig_data<-subset(sig_data,V4==SGr,select=c(V1,V4,V5,V6))
              colnames(sig_data)<-c("simS","SgG","Indi","dur_prev_st")
              summary(sig_data)
              #Indi tells the current signal state
              #dur_prev_st tells the duration of previous signal state
              #Carry out appropriate corrections 
              sig_data<-sig_data[simS>=300,]
              sig_data[Indi=="red",Indi_cor:="amber"]
              sig_data[Indi=="amber",Indi_cor:="green"]
              sig_data[Indi=="green",Indi_cor:="red"]
              sig_data[,dur:=as.numeric(dur_prev_st)]
              sig_data$no<-rep(no,length(sig_data$simS))
              sig_data$run_no<-rep(run_no,length(sig_data$simS))
              da<-rbindlist(list(da,sig_data))
              sig_data<-NULL
  }
  da
}


#1 Fuction for reading signal controller file for all runs



scTimeRead_Allseed<-function(phaseNo,files){
  require(data.table)
  da<-data.table()
  for(file in files){
    #**************************************
    #Extracting scenario number 
    temp=strsplit(file,"_")[[1]][1]
    if(grepl("Scenario",temp)){
      no=strsplit(temp,"Scenario")[[1]][2]
    }else{
      no=strsplit(temp,"S")[[1]][2]
    }
    no=as.numeric(no)
    run_no<-strsplit(file,"_")[[1]][2]
    run_no<-strsplit(run_no,"[.]")[[1]][1]
    run_no<-as.numeric(run_no)
    
    #**************************************
    assign("sig_data",fread(file,header=FALSE,sep=";"))
    #********************************************************************************
    #Getting the data for signal group 6 (EBT)
    #Migth need the data for sg 4 in future
    SGr=phaseNo
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
    da<-rbindlist(list(da,sig_data))
    sig_data<-NULL
  }
  da
}

