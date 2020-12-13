rm(list=ls())
library(readxl)
library(dplyr)
#dir
setwd("C:/Users/ALVARO/Box/2020/umsa/est133/r-exams")
#Recuperatorio
p3<-read.csv("Recuperatorio, EST-133. Paralelo C. II-2020.csv",sep=",")
p3<-p3[,-c(5,6,8,9,10,11,12,14,15)]
names(p3)[1:6]<-c("time","mail","nota","ci","ru","tipo")

p21<-p3[p3$tipo=="Primer parcial",c(1:6,7:18)]
p22<-p3[p3$tipo=="Segundo Parcial",c(1:6,19:30)]

aux<-seq(9,18,3)
p21<-p21[,-aux]
p22<-p22[,-aux]

nn<-c(paste0(rep(c("preg","preg_pt"),3),sort(rep(seq(1:4),2))))
names(p21)[7:14]<-nn
names(p22)[7:14]<-nn

###########
p2<-rbind(p21,p22)
rm(p21,p22)
############CONFIRMACION DE LA EVALUACIÓN
#pregunta 1:3
p2$preg_1<-as.numeric(substr(p2$preg_pt1,1,2))
p2$preg_2<-as.numeric(substr(p2$preg_pt2,1,2))
p2$preg_3<-as.numeric(substr(p2$preg_pt3,1,2))
p2$preg_4<-as.numeric(substr(p2$preg_pt4,1,2))
#NA
for(i in 15:18){
p2[is.na(p2[,i]),i]<-0
}
#nota final
p2$total<-apply(p2[,15:18],1,sum)
p2$nota<-as.numeric(substr(p2$nota,1,3))
#p2$total-p2$nota
###
recup<-p2[,c(6,4,15:19)]
library(writexl)

recup<-recup %>% arrange(tipo,ci)
write_xlsx(recup,"recup_EST133.xlsx")
recup<-recup[,c("tipo","ci","total")]
save(recup,file="recup.RData")
