library(readxl)
#parcial 1 y base de datos
bd<-read_excel("C:\\Users\\ALVARO\\Box\\2020\\umsa\\est133\\NOT_EST-133-I2020.xls")
#parical 2
p2<-read.csv("C:\\Users\\ALVARO\\Box\\2020\\umsa\\est133\\parcial2_google.csv",sep=",")
p2<-p2[,-c(5,6,8,9,11,12,14,15)]
names(p2)[1:7]<-c("time","mail","nota","ci","ru","nacimiento","tipo")
p2p<-p2[p2$tipo=="PAR",c(1:7,8:47)]
p2i<-p2[p2$tipo=="IMPAR",c(1:7,48:87)]
aux<-c(10,13,16,27,38,41,44,47)
p2p<-p2p[,-aux]
p2i<-p2i[,-aux]
nn<-c(paste0(rep(c("preg","preg_pt"),3),sort(rep(seq(1:3),2))),
sort(c(paste0("preg4",letters[1:4]),paste0("preg4",letters[1:4],"_pt"))),
c("preg5","preg_pt5"),
sort(c(paste0("preg6",letters[1:4]),paste0("preg6",letters[1:4],"_pt"))),
paste0(rep(c("preg","preg_pt"),3),sort(rep(7:10,2))))
names(p2p)[8:39]<-nn
names(p2i)[8:39]<-nn
p2<-rbind(p2p,p2i)
rm(p2p,p2i)
############CONFIRMACION DE LA EVALUACIÓN
#pregunta 1:3
p2$nota_p1<-as.numeric(substr(p2$preg_pt1,1,2))
p2$nota_p2<-as.numeric(substr(p2$preg_pt2,1,2))
p2$preg_pt3[42]<-"0.00 / 2"
p2$nota_p3<-as.numeric(substr(p2$preg_pt3,1,2))
#pregunta 4
p2$preg4c_pt[54]<-"0.00 / 2"
p2$nota_p4<-as.numeric(substr(p2$preg4a_pt,1,2))+
as.numeric(substr(p2$preg4b_pt,1,2))+
as.numeric(substr(p2$preg4c_pt,1,2))+
as.numeric(substr(p2$preg4d_pt,1,2))
#pregunta 5
p2$nota_p5<-as.numeric(substr(p2$preg_pt5,1,2))
#pregunta 6
p2$preg6b_pt[86]<-"0.00 / 2"
p2$preg6c_pt[86]<-"0.00 / 2"
p2$nota_p6<-as.numeric(substr(p2$preg6a_pt,1,2))+
  as.numeric(substr(p2$preg6b_pt,1,2))+
  as.numeric(substr(p2$preg6c_pt,1,2))+
  as.numeric(substr(p2$preg6d_pt,1,2))
# pregunta 7
#par 2.65, impar=3.31
#par
p2$preg7<-toupper(p2$preg7)
p2$preg7<-gsub(",",".",p2$preg7)
aux<-c("V","(",")","[","]","=","X")
for(i in aux){
  p2$preg7<-gsub(i,"",p2$preg7,fixed=T)
}
p2$preg7<-as.numeric(p2$preg7)

p2$nota_p7<-0
p2$nota_p7[p2$tipo=="PAR"]<-(p2$preg7[p2$tipo=="PAR"]>=2.5 & p2$preg7[p2$tipo=="PAR"]<=2.7)*10
p2$nota_p7[p2$tipo=="IMPAR"]<-(p2$preg7[p2$tipo=="IMPAR"]>=3.2 & p2$preg7[p2$tipo=="IMPAR"]<=3.4)*10
p2$nota_p7[is.na(p2$nota_p7)]<-0

#pregunta 8
#impar -1/8 -0.08, par -13/60, -0.22
p2$preg8<-toupper(p2$preg8)
p2$preg8<-gsub(",",".",p2$preg8)
aux<-c("C","(",")","[","]","=","X")
for(i in aux){
  p2$preg8<-gsub(i,"",p2$preg8,fixed=T)
}
p2$preg8[c(37,62)]<-substr(p2$preg8,1,6)[c(37,62)]

p2$preg8<-gsub("13/60",13/60,p2$preg8)
p2$preg8<-gsub("5/12",5/12,p2$preg8)
p2$preg8<-gsub("1/12",1/12,p2$preg8)
p2$preg8<-gsub("1/2",1/2,p2$preg8)

p2$preg8<-as.numeric(p2$preg8)

p2$nota_p8<-0
p2$nota_p8[p2$tipo=="PAR"]<-(p2$preg8[p2$tipo=="PAR"]>=(-0.24) & p2$preg8[p2$tipo=="PAR"]<=(-0.2))*10
p2$nota_p8[p2$tipo=="IMPAR"]<-(p2$preg8[p2$tipo=="IMPAR"]>=(-0.1) & p2$preg8[p2$tipo=="IMPAR"]<=(-0.06))*10
p2$nota_p8[is.na(p2$nota_p8)]<-0

#pregunta 9
p2$nota_p9<-as.numeric(substr(p2$preg_pt9,1,2))

#pregunta 10

p2$preg10<-toupper(p2$preg10)

p2$preg10<-gsub(",",".",p2$preg10)

p2$preg10<-gsub("E[X]=0",0,p2$preg10,fixed = T)
p2$preg10<-gsub("E[X] = 0",0,p2$preg10,fixed = T)
p2$preg10<-gsub("E(X) = 0",0,p2$preg10,fixed = T)

p2$preg10<-as.numeric(p2$preg10)

p2$nota_p10<-0

p2$nota_p10<-(p2$preg10==0)*10
p2$nota_p10[is.na(p2$nota_p10)]<-0

p2$nota_rev<-apply(p2[,40:49],1,sum)
p2$p2_20<-(apply(p2[,40:49],1,sum)/100)*20
###verificando par impar

lastci <- function(x){
  x<-as.character(x)
  res<-sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
  return(res)
  }

table(as.numeric(substr(strReverse(p2$ci),1,1)) %% 2,p2$tipo)

  p2$p2_20<-(apply(p2[,40:49],1,sum)/100)*20

p2<-p2[,c("ci","p2_20")]
save(p2,file="p2rev.RData")