# EJEMPLOS RANDOMFOREST

# TUNEADO DE MTRY CON CARET

library(caret)
library(randomForest)
load("saheartbis.Rda")


set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8,9,10,11))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

rf<- train(factor(chd)~.,data=saheartbis,
 method="rf",trControl=control,tuneGrid=rfgrid,
 linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
 importance=TRUE)

rf

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla

barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))

# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest

set.seed(12345)

rfbis<-randomForest(factor(chd)~.,
 data=saheartbis,
 mtry=5,ntree=3000,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])


# EL TUNEADO ANTERIOR LO HABÍAMOS REALIZADO CON TODAS LAS VARIABLES
# PERO SABEMOS QUE SOLO 7 SON IMPORTANTES, VAMOS A REALIZAR EL TUNEADO 
# UNA SEGUNDA VEZ CON SOLO LAS VARIABLES DE INTERÉS

listconti<-c("age", "tobacco", "ldl",
             "adiposity",  "typea","sbp","famhist.Absent")
paste(listconti,collapse="+")

set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(chd)~age+tobacco+ldl+adiposity+typea+famhist.Absent,data=saheartbis,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf

# Recomienda mtry=3

# La función cruzadarfbin permite plantear random forest 

load ("saheartbis.Rda")
source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")
source ("cruzada rf binaria.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("age", "tobacco", "ldl",
  "adiposity",  "typea","famhist.Absent"),
 listclass=c(""), grupos=10,sinicio=1234,repe=10)

 medias1$modelo="Logística"

medias2<-cruzadaavnnetbin(data=saheartbis,
vardep="chd",listconti=c("age", "tobacco", "ldl",
"adiposity",  "typea","famhist.Absent"), listclass=c(""),grupos=10,sinicio=1234,repe=10,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

  medias2$modelo="avnnet"


  medias3<-cruzadaarbolbin(data=saheartbis,
                           vardep="chd",listconti=c("age", "tobacco", "ldl",
                                                    "adiposity",  "typea","famhist.Absent"), listclass=c(""),grupos=10,sinicio=1234,repe=10,
  cp=c(0),minbucket =5)

  medias3$modelo="arbol"

  medias4<-cruzadarfbin(data=saheartbis, 
                        vardep="chd",listconti=c("age", "tobacco", "ldl",
                                                 "adiposity",  "typea","famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=10,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=150)

  medias4$modelo="bagging"

medias5<-cruzadarfbin(data=saheartbis, 
                          vardep="chd",listconti=c("age", "tobacco", "ldl",
                                                   "adiposity",  "typea","famhist.Absent"),
 listclass=c(""),
  grupos=10,sinicio=1234,repe=10,nodesize=10,
  mtry=3,ntree=3000,replace=TRUE,sampsize=150)

  medias5$modelo="rf"

union1<-rbind(medias1,medias2,medias3,medias4,medias5)

par(cex.axis=1.2)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")



# EJEMPLO CON VARIABLE CONTINUA

load("compressbien.Rda")

library(randomForest)

rfbis<-randomForest(cstrength~age+water+cement+blast,
 data=compressbien,
 mtry=2,ntree=1000,sampsize=300,nodesize=10,replace=TRUE)

# En continuas No estoy seguro de que 
# este gráfico esté realizado sobre oob
# pero puede valer para decidir tamaño muestral e iteraciones

plot(rfbis$mse)

# El problema es que en validación cruzada no podremos poner 1000 
# observaciones pues cada grupo tiene menos

# TUNEADO CON CARET DEL NÚMERO DE VARIABLES A SORTEAR EN CADA NODO

set.seed(12345)
rfgrid<-expand.grid(mtry=c(2,3,4))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

rf<- train(cstrength~age+water+cement+blast,
 data=compressbien,
 method="rf",trControl=control,tuneGrid=rfgrid,
 ntree=1000,sampsize=600,nodesize=10,replace=TRUE,
 importance=TRUE)

rf

# Recomienda bagging (mtry=4). A pesar de ello, comprobaremos vía validación
# cruzada repetida

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$IncNodePurity),]
tabla

barplot(tabla$IncNodePurity,names.arg=rownames(tabla))

source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
load("compressbien.Rda")

data<-compressbien

medias1<-cruzadaavnnet(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5,
size=c(15),decay=c(0.01),repeticiones=5,itera=100)

medias1$modelo="avnnet"

medias2<-cruzadalin(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),grupos=4,sinicio=1234,repe=5)

medias2$modelo="lineal"

medias3<-cruzadaarbol(data=data,
vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
 grupos=4,sinicio=1234,repe=5,cp=0,minbucket=5)

medias3$modelo="arbol"

medias4<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=10,replace=TRUE,ntree=600,mtry=4)

medias4$modelo="bagging"

medias5<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=10,replace=TRUE,ntree=600,mtry=3)

medias5$modelo="rf"

union1<-rbind(medias1,medias2,medias3,medias4,medias5)

par(cex.axis=1.5)
boxplot(data=union1,error~modelo,col="pink")

union1<-rbind(medias4,medias5)

par(cex.axis=1.5)
boxplot(data=union1,error~modelo,col="pink")



