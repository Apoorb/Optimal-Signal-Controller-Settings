#New_study_plan_scenario.R
#Purpose: Create list of scenarios 
rm(list=ls())



#***********************************************************************************************************************************
#***************************************Simultaneous Gap Out**************************************************************************
#***********************************************************************************************************************************

require(data.table)
detector<-"Radar"
i=1
Vol_nbt<-400
vol_ebt<-c(400,800)
vol_wbt<-c(400,800)
passage_time<-c(0.5,2.4)
gap_out<-c("Simultaneous","Non Simultaneous")
data1<-data.table()
for(ga in gap_out){
  for(wb in vol_wbt){
    for(a in vol_ebt){
      for(b in passage_time){
        if(a==400){
          EBT_MaxGreen<-40
          NBT_MaxGreen<-30
        }
        else if(a==800){
          EBT_MaxGreen<-40
          NBT_MaxGreen<-30
        }
        else if(a==1200){
          EBT_MaxGreen<-70
          NBT_MaxGreen<-30
        }
        
        temp<-cbind(i,"Radar",2,a,1,Vol_nbt,5,b,EBT_MaxGreen,NBT_MaxGreen,wb,ga)              
        data1<-rbind(data1,temp)
        i=i+1
      }
    }
  }
}
colnames(data1)
colnames(data1)<-c("S.No","Det Type","Num Ln","EBT Vol","Option","NBT vol","Per Trucks","Passage Time","MaxGr6","MaxGr4","WBT Vol","Gap Out")
# data1<-data1[!(`S.No` %in% c(11,12,15,16))]
# data1[`S.No`==13,`S.No`:=11]
# data1[`S.No`==14,`S.No`:=12]
# data1[`Gap Out`=="Non Simultaneous",`WBT Vol`:=NA]
data1<-data1[,.(`S.No`,`Gap Out`,`WBT Vol`,`EBT Vol`,`Passage Time`,MaxGr6,MaxGr4,Option,`Det Type`,`Num Ln`,`Per Trucks`,`NBT vol`)]

setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/")
fwrite(data1,"List_of_Sen_SimultaneousGap.csv")






#***********************************************************************************************************************************
#***************************************Stopbar Detector***************************************************************************
#***********************************************************************************************************************************


detector_config<-c("60ft")
i=1
Vol_nbt<-400
vol_ebt<-c(400,800,1200)
passage_time<-c(0,0.5,1,1.5,2,3,4,5)
data1<-data.table()
for(a in vol_ebt){
  for(b in passage_time){
    if(a==400){
      green_split="13-13"
      EBT_MaxGreen<-30
      NBT_MaxGreen<-30
    }
    else if(a==800){
      green_split="30-15"
      EBT_MaxGreen<-30
      NBT_MaxGreen<-30
    }
    else if(a==1200){
      green_split="68-22"
      EBT_MaxGreen<-70
      NBT_MaxGreen<-30
    }
    
    temp<-cbind(i,a,green_split,EBT_MaxGreen,NBT_MaxGreen,b,"Non-Locking","No_recall","60ft",Vol_nbt)              
    data1<-rbind(data1,temp)
    i=i+1
  }
}
colnames(data1)<-colnames(data)


setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/")
colnames(data1)<-c("Scenario","EBT_Vol","Green_Splits","EBT_MaxGreen","NBT_MaxGreen","Passage_time","Memory","recall","detector_config","NBT_vol")
fwrite(data1,"Scenario_Through_STbar.csv")




#***********************************************************************************************************************************
#***************************************Upstream Detector***************************************************************************
#***********************************************************************************************************************************
Vol_nbt<-400
vol_ebt<-c(400,800,1200)
passage_time<-c(2.5,3,4,5)
cycle_len<-c(60,120)
Green_splits<-c("optimal")
Memory<-c("Locking","Non-Locking")
recall<-c("MinRecall","No_recall")
detector_config<-c("250ft_6ft")
library(data.table)
data<-data.table()
i=1
for(a in vol_ebt){
      for(b in Green_splits){
       for(c in passage_time){
        for(d in Memory){
          for(e in recall){
            for(f in detector_config){
              if(a==400){
                green_split="13-13"
              }
              else if(a==800){
                green_split="30-15"
              }
              else if(a==1200){
                green_split="68-22"
              }
                if(d=="Locking" & e=="MinRecall"){}
              else{   temp<-cbind(i,a,b,c,d,e,f,Vol_nbt)              
              data<-rbind(data,temp)
              i=i+1}
          }
        }
      }
    }
  }
}

colnames(data1)<-colnames(data)


setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/")
colnames(data)<-c("Scenario","EBT_Vol","Green_Splits","Passage_time","Memory","recall","detector_config","NBT_vol")
fwrite(data,"Scenario_Through_Upstream.csv")


#**********************************************************************************************************************************
#**************************Study plan for max out**********************************************************************************
#**********************************************************************************************************************************

detector_config<-c("60ft")
i=1
Vol_nbt<-400
vol_ebt<-c(800,1200)
passage_time<-c(1,1.5,2,3,4,5)
maxOut<-c("TSOH","plus5","plus10")
lanedist<-c("50-50","75-25")
data1<-data.table()
for(ln in lanedist){
for(a0 in maxOut){  
  for(a in vol_ebt){
    for(b in passage_time){
      if(a0=="TSOH" & a==800){
        green_split="13-13"
        EBT_MaxGreen<-30
        NBT_MaxGreen<-30
      }
      else if(a0=="TSOH" & a==1200){
        green_split="68-22"
        EBT_MaxGreen<-70
        NBT_MaxGreen<-30
      }
      else if(a0=="plus5" & a==800){
        green_split="13-13"
        EBT_MaxGreen<-30+5
        NBT_MaxGreen<-30+5
      }
      else if(a0=="plus5" & a==1200){
        green_split="68-22"
        EBT_MaxGreen<-70+5
        NBT_MaxGreen<-30+5
      }
      else if(a0=="plus10" & a==800){
        green_split="13-13"
        EBT_MaxGreen<-30+10
        NBT_MaxGreen<-30+10
      }
      else if(a0=="plus10" & a==1200){
        green_split="68-22"
        EBT_MaxGreen<-70+10
        NBT_MaxGreen<-30+10
      }
    temp<-cbind(i,ln,a0,a,green_split,EBT_MaxGreen,NBT_MaxGreen,b,"Non-Locking","No_recall","60ft",Vol_nbt)              
    data1<-rbind(data1,temp)
    i=i+1
     }
  }
}
}
colnames(data1)<-colnames(data)


setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code/")
colnames(data1)<-c("Scenario","Lane Dist","MaxGr_Mt","EBT_Vol","Green_Splits","EBT_MaxGreen","NBT_MaxGreen","Passage_time","Memory","recall","detector_config","NBT_vol")
fwrite(data1,"Scenario_MaxGr.csv")






