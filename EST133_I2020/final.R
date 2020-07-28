rm(list=ls())
library(readxl)
library(dplyr)
#Recuperatorio
p3<-read.csv("C:\\Users\\ALVARO\\Box\\2020\\umsa\\est133\\final_google.csv",sep=",")
p3<-p3[,-c(5,6,8,9,10,11,12)]
names(p3)[1:5]<-c("time","mail","nota","ci","ru")

aux<-seq(8,32,3)
p2<-p3[,-aux]

nn<-c(paste0(rep(c("preg","preg_pt"),3),sort(rep(seq(1:9),2))))
names(p2)[6:23]<-nn
############CONFIRMACION DE LA EVALUACIÓN
#pregunta 1:3
p2$preg_1<-as.numeric(substr(p2$preg_pt1,1,2))
p2$preg_2<-as.numeric(substr(p2$preg_pt2,1,2))
p2$preg_3<-as.numeric(substr(p2$preg_pt3,1,2))
p2$preg_4<-as.numeric(substr(p2$preg_pt4,1,2))
p2$preg_5<-as.numeric(substr(p2$preg_pt5,1,2))
p2$preg_6<-as.numeric(substr(p2$preg_pt6,1,2))
p2$preg_7<-as.numeric(substr(p2$preg_pt7,1,2))
p2$preg_8<-as.numeric(substr(p2$preg_pt8,1,2))
p2$preg_9<-as.numeric(substr(p2$preg_pt9,1,2))
#NA
for(i in 24:32){
p2[is.na(p2[,i]),i]<-0
}
#nota final
p2$total<-apply(p2[,24:32],1,sum)
p2$nota<-as.numeric(substr(p2$nota,1,3))
#p2$total-p2$nota
###
final<-p2[,c(4,24:33)]
library(writexl)
setwd("C:\\Users\\ALVARO\\Documents\\GitHub\\notas\\EST133_I2020\\")
final<-final %>% arrange(ci)
write_xlsx(final,"final_EST133.xlsx")
final<-final[,c("ci","total")]
save(final,file="final.RData")
