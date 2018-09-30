


NodeFileReader<-function(files,directory=dir){
  setwd(directory)
  require(data.table)
  daSum<-data.table()
  da1<-data.table()
  file<-files[2]
  for(file in files){
    #***********************************************************************
    #Extract Scenario number 
 
   # twoLnDir<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_Scenario_2ln/Scenarios"
    #Mac 
    twoLnDir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_STbar_Scenario_2ln/Scenarios"
     #adv_Dir<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/Scenarios"
    adv_Dir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_Scenario_2ln/Scenarios"
    #adv_Dir1<-"C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/Scenarios"
    adv_Dir1<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrmRadar_Scenario_2ln/Scenarios"
    adv_Dir2<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Through_UpStrm_IterisStudy/Scenarios"  
    #MaxGrDir 
    MaxGrDir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Scenario_MaxGr/Scenarios"
    #Simultaneous gap
    SimuDir<-"/Users/Apoorb/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/SimultaneiousGapOutSen/Scenarios"
    
      if(directory==SimuDir|directory==twoLnDir | directory==adv_Dir| directory==adv_Dir1|directory==adv_Dir2 |directory==MaxGrDir)  {
    f<-strsplit(file,"Scenario")[[1]][2]
    no =strsplit(f,"[_]")[[1]][1]
    no=as.numeric(no)
    run_no<-nchar(file)-nchar(".att")
    run_no<-substr(file,run_no,run_no)
    run_no<-as.numeric(run_no)} else{
      pos<-nchar(file)-nchar("_Node Results_001.att")-1
      no =substr(file,pos,pos+1)
      no=as.numeric(no)
      run_no<-nchar(file)-nchar(".att")
      run_no<-substr(file,run_no,run_no)
      run_no<-as.numeric(run_no)
    }
    
    #***********************************************************************
    #Read the file and assign it to a variable scenarioX
    #This way I am wasting a lot of memory but it is easier to debug. 
    #Delete these files it the execution speed reduces 
    assign("node_data",fread(file,header=TRUE,sep=";",skip=20))
    #The following col contains the simulation run (1,2,3)- not needed
    node_data$`$MOVEMENTEVALUATION:SIMRUN`<-NULL
    #Change variable names 
    #Update this part of code if you change the order of node evaluation columns in VISSIM 
    oldnames<-colnames(node_data)
    newnames<-c("tint","from_link","to_link","Qlen","MaxQlen","veh_delay","stop_delay","stops")
    setnames(node_data,oldnames,newnames)
    #Links are number based on NEMA signal groups
    #6 is EBT,  4 is NBT  etc
    node_data[to_link==6 &from_link==6,dir:="ebt"]
    node_data[to_link==2 &from_link==2,dir:="wbt"]
    node_data[to_link==4 &from_link==4,dir:="nbt"]
    node_data[from_link==1& to_link==4,dir:="eblt"]
    #The NAs in the from_link and to_link columns contain the entire intersection values
    node_data[is.na(from_link)& is.na(to_link),dir:="all"]    #AAA
    node_data$to_link<-NULL
    node_data$from_link<-NULL
    #node_tda would contain second by second results for a scenario and all runs 
 
      node_tda<-node_data
      #Because node_tda is second by second there are lot of NAs in the data.table. Pay attentoin to not running this line before running code AAA
      node_tda[is.na(node_tda)]<-0
      node_tda<-node_tda[!tint %in% c("Average","Maximum","Minimum","Standard deviation","Total")]
      node_tda$no<-rep(no,length(node_tda$tint))
      node_tda$run_no<-rep(run_no,length(node_tda$tint))
      da1<-rbindlist(list(da1,node_tda))
    
    #This code cannot go up as we need to copy node_data to node_tda first
    node_data=node_data[tint=="Average",]
    node_data[is.na(node_data)]<-0
    node_data$no<-rep(no,length(node_data$tint))
    daSum<-rbindlist(list(daSum,node_data))
    node_data<-NULL
    node_tda<-NULL
    
  }
  list(daSum,da1)
}


