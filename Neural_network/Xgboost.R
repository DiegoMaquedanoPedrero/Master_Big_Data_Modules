
# EJEMPLOS XGBOOST

# TUNEADO DE XGBOOST CON CARET

# Caret permite tunear estos parámetros básicos:
 
#  
# nrounds (# Boosting Iterations)=número de iteraciones
# max_depth (Max Tree Depth)=profundida máxima de los árboles
# eta (Shrinkage)=parámetro v gradient boosting
# gamma (Minimum Loss Reduction)=gamma
   # cte regularización. Dejar a 0 por defecto
# colsample_bytree (Subsample Ratio of Columns) 
   # % Sorteo variables antes de cada árbol , 
   # al estilo de random forest pero antes del árbol, no en cada nodo. Dejar
   # a 1 por defecto.
# min_child_weight (Minimum Sum of Instance Weight).
    # observaciones mínimas en el nodo final. Similar al minobsinnode del gbm.
# # subsample (Subsample Percentage) 
   # % Sorteo de observaciones antes de cada árbol , al estilo de random forest.
   # Dejar a 1 por defecto.


library(caret)

load("saheartbis.Rda")
set.seed(12345)

xgbmgrid<-expand.grid(
 min_child_weight=c(5,10,20),
 eta=c(0.1,0.05,0.03,0.01,0.001),
 nrounds=c(100,500,1000,5000),
 max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

xgbm<- train(factor(chd)~.,data=saheartbis,
 method="xgbTree",trControl=control,
 tuneGrid=xgbmgrid,verbose=FALSE)

xgbm
 
plot(xgbm)

# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

xgbmgrid<-expand.grid(eta=c(0.01),
 min_child_weight=c(20),
 nrounds=c(50,100,150,200,250,300),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

xgbm<- train(factor(chd)~.,data=saheartbis,
 method="xgbTree",trControl=control,
 tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm,ylim=c(0.65,0.76))

# Probamos con otras semillas para la validación cruzada

set.seed(12367)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

xgbm<- train(factor(chd)~.,data=saheartbis,
 method="xgbTree",trControl=control,
 tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm,ylim=c(0.65,0.76))


# IMPORTANCIA DE VARIABLES

varImp(xgbm)
plot(varImp(xgbm))

# PRUEBO PARÁMETROS CON VARIABLES SELECCIONADAS

xgbmgrid<-expand.grid(
 min_child_weight=c(5,10,20),
 eta=c(0.1,0.05,0.03,0.01,0.001),
 nrounds=c(100,500,1000,5000),
 max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
 classProbs=TRUE) 

xgbm<- train(factor(chd)~age+tobacco+ldl+adiposity+typea+famhist.Absent,
 data=saheartbis,
 method="xgbTree",trControl=control,
 tuneGrid=xgbmgrid,verbose=FALSE)

xgbm
 
plot(xgbm)

# La función cruzadagbmbin permite plantear gradient boosting para binarias 

load ("saheartbis.Rda")
source ("cruzadas avnnet y log binaria.R")
source ("cruzada arbolbin.R")
source ("cruzada rf binaria.R")
source ("cruzada gbm binaria.R")
source ("cruzada xgboost binaria.R")

medias1<-cruzadalogistica(data=saheartbis,
 vardep="chd",listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""), grupos=4,sinicio=1234,repe=10)

 medias1$modelo="Logística"

medias2<-cruzadaavnnetbin(data=saheartbis,
 vardep="chd",listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=10,
  size=c(5),decay=c(0.1),repeticiones=5,itera=200)

  medias2$modelo="avnnet"


  medias3<-cruzadaarbolbin(data=saheartbis,
 vardep="chd",listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),grupos=4,sinicio=1234,repe=10,
  cp=c(0),minbucket =5)

  medias3$modelo="arbol"

  medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=10,nodesize=10,
  mtry=6,ntree=3000,replace=TRUE,sampsize=100)

  medias4$modelo="bagging"

    medias5<-cruzadarfbin(data=saheartbis, vardep="chd",
   listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=10,nodesize=10,
  mtry=5,ntree=3000,replace=TRUE,sampsize=100)

  medias5$modelo="rf"

medias6<-cruzadagbmbin(data=saheartbis, vardep="chd",
   listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
n.minobsinnode=10,shrinkage=0.001,n.trees=2000,interaction.depth=2)

medias6$modelo="gbm"

medias7<-cruzadaxgbmbin(data=saheartbis, vardep="chd",
   listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.10,nrounds=100,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=1,
 alpha=0,lambda=0,lambda_bias=0)

medias7$modelo="xgbm"

# La otra opción recomendada por caret

medias8<-cruzadaxgbmbin(data=saheartbis, vardep="chd",
   listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.001,nrounds=500,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=1,
 alpha=0,lambda=0,lambda_bias=0)

medias8$modelo="xgbm2"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,
 medias7,medias8)

par(cex.axis=0.8,cex=1)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")

# PARA REDUCIR LA VARIANZA SE PUEDE RECURRIR A SORTEAR VARIABLES
# (ESTILO RANDOMFOREST PERO ANTES DEL ARBOL) Y/O
# A SORTEAR OBSERVACIONES (BAGGING)
# EN AMBOS CASOS HAY QUE AUMENTAR EL NÚMERO DE ÁRBOLES

medias9<-cruzadaxgbmbin(data=saheartbis, vardep="chd",
      listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.10,nrounds=200,max_depth=6,
  gamma=0,colsample_bytree=0.8,subsample=1,
 alpha=0,lambda=0,lambda_bias=0)

medias9$modelo="xgbmvar08"


medias10<-cruzadaxgbmbin(data=saheartbis, vardep="chd",
      listconti=c("tobacco", "ldl","age", "typea", "famhist.Absent"),
 listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.10,nrounds=200,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=0.8,
 alpha=0,lambda=0,lambda_bias=0)

medias10$modelo="xgbmobs08"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,
 medias8,medias9,medias10)

par(cex.axis=0.5,cex=1)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC",col="pink")


# EJEMPLO CON VARIABLE CONTINUA

load("compressbien.Rda")

library(caret)

set.seed(12345)

xgbmgrid<-expand.grid(
 min_child_weight=c(5,10,20),
 eta=c(0.1,0.05,0.03,0.01,0.001),
 nrounds=c(100,500,1000,5000),
 max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

comp<-compressbien[,c("cstrength","age","water","cement","blast")]

xgbm<- train(cstrength~.,data=comp,
 method="xgbTree",trControl=control,
 tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

# Tuning parameter 'subsample' was held constant at a value of 1
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were nrounds = 5000, max_depth = 6,
#  eta = 0.03, gamma = 0, colsample_bytree = 1, min_child_weight = 10
#  and subsample = 1.
 
plot(xgbm)

# ESTUDIO DE EARLY STOPPING
# Probamos a fijar algunos parámetros para ver como evoluciona
# en función de las iteraciones

xgbmgrid<-expand.grid(eta=c(0.03,0.05),
 min_child_weight=c(10),
 nrounds=c(1000,2000,5000,6000,7000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all") 

xgbm<- train(cstrength~.,data=comp,
 method="xgbTree",trControl=control,
 tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)

# IMPORTANCIA DE VARIABLES

varImp(xgbm)
plot(varImp(xgbm))

# VALIDACIÓN CRUZADA REPETIDA

source("cruzada arbol continua.R")
source("cruzadas avnnet y lin.R")
source("cruzada rf continua.R")
source("cruzada gbm continua.R")
source("cruzada xgboost continua.R")

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
  nodesize=20,replace=TRUE,ntree=200,mtry=4)

medias4$modelo="bagging"


medias5<-cruzadarf(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
  nodesize=10,replace=TRUE,ntree=600,mtry=3)

medias5$modelo="rf"


medias6<-cruzadagbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
n.minobsinnode=20,shrinkage=0.10,n.trees=500,interaction.depth=2)

medias6$modelo="gbm"


medias7<-cruzadaxgbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.03,nrounds=5000,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=1)

medias7$modelo="xgbm"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)


union1<-rbind(medias1,medias3,medias4,medias5,medias6,medias7)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo,col="pink")


# PRUEBAS SORTEANDO OBSERVACIONES Y TAMBIÉN VARIANDO EL PARÁMETRO ALPHA DE REGULARIZACIÓN


medias8<-cruzadaxgbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.03,nrounds=5000,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=0.8)

medias8$modelo="xgbm2"

medias9<-cruzadaxgbm(data=data,
 vardep="cstrength",listconti=c("age","water","cement","blast"),
listclass=c(""),
  grupos=4,sinicio=1234,repe=5,
   min_child_weight=10,eta=0.03,nrounds=5000,max_depth=6,
  gamma=0,colsample_bytree=1,subsample=1,alpha=0.5)

medias9$modelo="xgbm3"


union1<-rbind(medias6,medias7,medias8,medias9)

par(cex.axis=0.5)
boxplot(data=union1,error~modelo)




