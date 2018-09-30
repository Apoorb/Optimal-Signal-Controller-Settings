

NodeFileReader<-function(files,directory){
  require(data.table)
  daSum<-data.table()
  da1<-data.table()
  file<-files[10]
  for(file in files){
    #***********************************************************************
    #Extract Scenario number 

    f<-strsplit(file,"Scenario")[[1]][2]
    no =strsplit(f,"[_]")[[1]][1]
    no=as.numeric(no)
    run_no<-nchar(file)-nchar(".att")
    run_no<-substr(file,run_no,run_no)
    run_no<-as.numeric(run_no)
    #***********************************************************************
    #Read the file and assign it to a variable scenarioX
    #This way I am wasting a lot of memory but it is easier to debug. 
    #Delete these files it the execution speed reduces 
    assign("node_data",fread(file,header=TRUE,sep=";"))
    #The following col contains the simulation run (1,2,3)- not needed
    node_data$`$MOVEMENTEVALUATION:SIMRUN`<-NULL
    #Change variable names 
    #Update this part of code if you change the order of node evaluation columns in VISSIM 
    oldnames<-colnames(node_data)
    newnames<-c("tint","from_link","to_link","Qlen","veh_delay")
    setnames(node_data,oldnames,newnames)
    
    #Links are number based on NEMA signal groups
    #6 is EBT,  4 is NBT  etc
    node_data[from_link==6 &to_link==6,dir:="ebt"]
    node_data[from_link==2 &to_link==2,dir:="wbt"]
    node_data[from_link==1& to_link==41,dir:="eblt"]
    
    node_data[from_link==4 &to_link==41,dir:="nbt"]
    node_data[from_link==8 &to_link==8,dir:="sbt"]
    node_data[from_link==8& to_link==6,dir:="sblt"]
    #The NAs in the from_link and to_link columns contain the entire intersection values
    node_data[is.na(from_link)& is.na(to_link),dir:="all"]    #AAA
    node_data$to_link<-NULL
    node_data$from_link<-NULL
    node_data<-node_data[!is.na(dir),]
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


