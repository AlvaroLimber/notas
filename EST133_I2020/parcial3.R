rm(list=ls())
library(readxl)
library(dplyr)
#parcial 2
#p3<-read.csv("C:\\Users\\ALVARO\\Box\\2020\\umsa\\est133\\parcial3_google.csv",sep=",")
#p3<-read.csv("C:\\Users\\ALVARO\\Box\\2020\\umsa\\est133\\parcial3_google2.csv",sep=",")
p3<-read.csv("C:\\Users\\ALVARO\\Box\\2020\\umsa\\est133\\parcial3_google3.csv",sep=",")
p3<-p3[,-c(5,6,8,9,11,12,14,15)]
names(p3)[1:7]<-c("time","mail","nota","ci","ru","nacimiento","tipo")

p2p<-p3[p3$tipo=="PAR",c(1:7,8:31)]
p2i<-p3[p3$tipo=="IMPAR",c(1:7,32:55)]
aux<-c(10,13,16,19,22,25,28,31)
p2p<-p2p[,-aux]
p2i<-p2i[,-aux]
nn<-c(paste0(rep(c("preg","preg_pt"),3),sort(rep(seq(1:8),2))))
names(p2p)[8:23]<-nn
names(p2i)[8:23]<-nn
###########
p2<-rbind(p2p,p2i)
rm(p2p,p2i)
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
#NA
for(i in 24:31){
p2[is.na(p2[,i]),i]<-0
}
#nota final
p2$total<-apply(p2[,24:31],1,sum)
p2$nota<-as.numeric(substr(p2$nota,1,3))
#p2$total-p2$nota
###
p3<-p2[,c(7,4,24:32)]
library(writexl)
setwd("C:\\Users\\ALVARO\\Documents\\GitHub\\notas\\EST133_I2020\\")
p3<-p3 %>% arrange(ci)
write_xlsx(p3,"parcial3_EST133.xlsx")
p3<-p3[,c("ci","total")]
save(p3,file="p3.RData")
