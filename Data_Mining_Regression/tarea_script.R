source("FuncionesMineriaAida.R")

library(questionr)
library(car)
library(Hmisc)
library(readxl)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(caret)
library(OneR)

datos <- read_excel("DatosEleccionesEuropeas2019.xlsx")
str(datos)


datos[,c(12:13,30,41)] <- lapply(datos[,c(12:13,30,41)], as.factor)

sapply(Filter(is.numeric, datos),function(x) length(unique(x)))

summary(datos)

datos$UnemploymentPtge <-replace(datos$UnemploymentPtge,
                       which((datos$UnemploymentPtge< 0)|(datos$UnemploymentPtge >100)), NA)
datos$AutonomosPtge <-replace(datos$AutonomosPtge,
                        which((datos$AutonomosPtge< 0)|(datos$AutonomosPtge >100)), NA)

summary(datos$AutonomosPtge)
summary(datos$UnemploymentPtge)

datos$CCAA<-car::recode(datos$CCAA,
    "c('Galicia','Asturias','Cantabria','PaísVasco','Rioja','CastillaLeón')='CCAA_Noroeste';
     c('Navarra','Aragón','Cataluña','ComValenciana','Baleares')='CCAA_Noreste';
     c('Extremadura','Madrid','CastillaMancha','Canarias')='CCAA_Suroeste';
     c('Andalucía','Murcia','Ceuta','Melilla')='CCAA_Sureste'")

varObjCont<-datos$AbstencionPtge
varObjBin<-datos$Izquierda_Derecha
input<-as.data.frame(datos[,-c(1,40,41)])
row.names(input)<-datos$CodigoINE

for (i in names(which(sapply(input, class)=="numeric"))){
        outliers(paste0("input$",i))
     }

sapply(datos, function(x) sum(is.na(x))/length(x))

input$prop_missings<-rowMeans(is.na(input))
summary(input$prop_missings)

(prop_missingsVars<-colMeans(is.na(input)))

input <-subset(input,prop_missings<0.5,select=names(prop_missingsVars)[prop_missingsVars<0.5])
varObjBin<-varObjBin[input$prop_missings<0.5]
varObjCont<-varObjCont[input$prop_missings<0.5]

input[,as.vector(which(sapply(input, class)=="numeric"))]<-
    sapply(Filter(is.numeric, input),function(x) impute(x,"random"))
input[,as.vector(which(sapply(input, class)=="factor"))]<-
    sapply(Filter(is.factor, input),function(x) impute(x,"random"))

# Se cambia el tipo de factor a character al imputar, así que hay que corregirlo
input[,as.vector(which(sapply(input, class)=="character"))] <-
    lapply(input[,as.vector(which(sapply(input, class)=="character"))] , as.factor)

length(unique(input$prop_missings))

input$prop_missings<-as.factor(input$prop_missings)
freq(input$prop_missings)

input$prop_missings<-car::recode(input$prop_missings,
    "c('0.0263157894736842','0.0526315789473684','0.0789473684210526','0.105263157894737')='0-10%';
c('0.131578947368421','0.157894736842105','0.184210526315789','0.210526315789474','0.263157894736842')='>10%'")
freq(input$prop_missings)

saveRDS(data.frame(varObjBin,varObjCont,input),"datosVotosDep")

rprop(table(input$CCAA,varObjBin))
rprop(table(input$PartidoCCAA,varObjBin))

boxplot_cuantcuali(input$Age_over65_Ptge,varObjBin,"%65 años")
boxplot_cuantcuali(input$UnemploymentPtge,varObjBin,"% desempleados")

boxplot_cuantcuali(varObjCont,input$CCAA,"varObjCont")
boxplot_cuantcuali(varObjCont,input$PartidoCCAA,"varObjCont")

dispersion(Filter(is.numeric, input),varObjCont)

datos<-data.frame(varObjBin,varObjCont,input)

#datos<-datos %>% select(-)

datos<-datos[,-c(1,12,13,15)]

set.seed(123)
trainIndex <- createDataPartition(datos$varObjCont, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]
#################################
modelo1<-lm(varObjCont~.,data=data_train,singular.ok = TRUE)
summary(modelo1)

Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test)

importanciaVariables(modelo1)
#########################
modelo2<-lm(varObjCont~Blancos+Nulos+Cs+PP+PSOE+Podemos+
                CCAA+Age_under19_Ptge+Age_over65_Ptge+
                WomanPopulationPtge+ForeignersPtge+UniversityPtge+
                Empresas+Densidad+PersonasInmueble+Explotaciones+PartidoCCAA+
                UnemploymentPtge+WomenUnemploymentPtge+UnemployLess25_Ptge+
                AgricultureUnemploymentPtge+IndustryUnemploymentPtge+
                ConstructionUnemploymentPtge+prop_missings,data=data_train,singular.ok = TRUE)
summary(modelo2)

Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test)

importanciaVariables(modelo2)
################################
modelo2<-lm(varObjCont~Blancos+Nulos+Cs+PP+PSOE+Podemos+
                CCAA+Age_under19_Ptge+Age_over65_Ptge+
                WomanPopulationPtge+ForeignersPtge+UniversityPtge+
                Empresas+Densidad+PersonasInmueble+Explotaciones+PartidoCCAA+
                UnemploymentPtge+WomenUnemploymentPtge+UnemployLess25_Ptge+
                AgricultureUnemploymentPtge+IndustryUnemploymentPtge+
                ConstructionUnemploymentPtge+prop_missings,data=data_train,singular.ok = TRUE)
summary(modelo2)

Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test)

importanciaVariables(modelo2)
#######################################
modelo3<-lm(varObjCont~Blancos+Nulos+Cs+PP+Podemos+
                CCAA+Age_under19_Ptge+Age_over65_Ptge+
                ForeignersPtge+
                Empresas+Densidad+PersonasInmueble+Explotaciones+
                UnemploymentPtge+
                AgricultureUnemploymentPtge+IndustryUnemploymentPtge+
                ConstructionUnemploymentPtge+prop_missings,data=data_train,singular.ok = TRUE)
summary(modelo3)

Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test)

importanciaVariables(modelo3)
#######################################
modelo4<-lm(varObjCont~Blancos+Nulos+Cs+PP+Podemos+
                CCAA+Age_under19_Ptge+Age_over65_Ptge+
                ForeignersPtge+
                Densidad+PersonasInmueble+Explotaciones+
                UnemploymentPtge+
                AgricultureUnemploymentPtge+
                ConstructionUnemploymentPtge+prop_missings,data=data_train,singular.ok = TRUE)
summary(modelo4)

Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test)

importanciaVariables(modelo4)
#######################################
par(mar=c(9, 12, 2.1, 2.1))
modelo5<-lm(varObjCont~Blancos+Nulos+Cs+PP+Podemos+
                CCAA+Age_under19_Ptge+Age_over65_Ptge+
                ForeignersPtge+
                Densidad+PersonasInmueble+Explotaciones+
                UnemploymentPtge+
                AgricultureUnemploymentPtge+
                ConstructionUnemploymentPtge+prop_missings+CCAA:UnemploymentPtge,
                data=data_train,singular.ok = TRUE)
importanciaVariables(modelo5)

###########################################
modelos<-list(modelo1,modelo2,modelo3,modelo4,modelo5)
sapply(modelos,function(x) x$rank)
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))

coef(modelo3)

datos<-datos2

datos<-datos[,-c(1,2)]
datos$aleatorio<-runif(nrow(datos))
datos$aleatorio2<-runif(nrow(datos))

discCont<-droplevels(optbin(data.frame(Filter(is.numeric, input[,-c(36,37)]),
                                       bin(varObjCont,nbins=5,method = "content"))))[,
                                                                                     -(ncol(Filter(is.numeric, input[,-c(36,37)]))+1)]
names(discCont)<-paste("disc", names(discCont), sep = "_")

aggregate(varObjCont, by=list(discCont$disc_Empresas), mean)
aggregate(varObjCont, by=list(discCont$disc_WomenUnemploymentPtge), mean)
aggregate(varObjCont, by=list(discCont$disc_AgricultureUnemploymentPtge), mean)

discCont$disc_Empresas<-car::recode(discCont$disc_Empresas,
    "c('(56.6,56.9]','(56.9,62.6]')='(56.6,62.6]'")

discCont$disc_WomenUnemploymentPtge<-car::recode(discCont$disc_WomenUnemploymentPtge,
    "c('(46.3,52.8]','(52.8,54.8]')='(46.3,54.8]'")

discCont$disc_AgricultureUnemploymentPtge<-car::recode(discCont$disc_AgricultureUnemploymentPtge,
    "c('(6.73,7.52]','(7.52,7.68]')='(6.73,7.68]'")

discCont$disc_IndustryUnemploymentPtge<-car::recode(discCont$disc_IndustryUnemploymentPtge,
    "c('(6.62,8.89]','(8.89,9.66]')='(6.62,9.66]'")

discCont$disc_ConstructionUnemploymentPtge<-car::recode(discCont$disc_ConstructionUnemploymentPtge,
    "c('(5.1,6.88]','(6.88,8.01]')='(5.1,8.01]'")

freq(discCont$disc_Empresas)

datos_todocont<-data.frame(varObjCont,input,TransfCont,discCont)
datos_todocont<-datos_todocont %>% select(-c("Censo","PartidoMasVotado","Otros"))
names(datos_todocont)

set.seed(123)
trainIndex <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train <- datos_todocont[trainIndex,]
data_test <- datos_todocont[-trainIndex,]

#### Aquí nos traemos el modelo manual ganador del apartado anterior para compararlo
# con los modelos que se van a generar automáticamente

modelo3<-lm(varObjCont~Blancos+Nulos+Cs+PP+Podemos+
                CCAA+Age_under19_Ptge+Age_over65_Ptge+
                ForeignersPtge+
                Empresas+Densidad+PersonasInmueble+Explotaciones+
                UnemploymentPtge+
                AgricultureUnemploymentPtge+IndustryUnemploymentPtge+
                ConstructionUnemploymentPtge+prop_missings,data=data_train,singular.ok = TRUE)

null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:33)])
#Modelo maximo, seleccionamos solo las columnas de las variables originales
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both",trace=F)
Rsq(modeloStepAIC,"varObjCont",data_test)

modeloStepAIC$rank

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    trace=F)
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank


modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC,"varObjCont",data_test)
modeloStepBIC$rank

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloBackBIC,"varObjCont",data_test)
modeloBackBIC$rank

########2ª parte. Se eliminan del data las variables menos significativas
data_train_reduced<-data_train %>% select(c("varObjCont","Nulos","Cs","PP","PSOE",
                                          "Podemos","CCAA","ForeignersPtge","UniversityPtge",
                                          "Densidad","PersonasInmueble","Explotaciones",
                                          "UnemploymentPtge","AgricultureUnemploymentPtge",
                                          "IndustryUnemploymentPtge","ConstructionUnemploymentPtge",
                                          "prop_missings"))

fullInt<-lm(varObjCont~.^2, data=data_train_reduced[,c(1:17)])
# El backward puede ser muy lento (o incluso no funcionar) cuando hay muchos posibles efectos
# No lo realizo por abreviar, pero sería recomendable probarlo
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both", trace = FALSE)
Rsq(modeloStepAIC_int,"varObjCont",data_test)

modeloBackAIC_int<-step(full, scope=list(lower=null, upper=fullInt), direction="backward", trace = FALSE)
Rsq(modeloBackAIC_int,"varObjCont",data_test)


modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",
                        k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_int,"varObjCont",data_test)

modeloBackBIC_int<-step(full, scope=list(lower=null, upper=fullInt), direction="backward",
                        k=log(nrow(data_train)),trace=F)
Rsq(modeloBackBIC_int,"varObjCont",data_test)






#Variables y transformadas
fullT<-lm(varObjCont~., data=data_train[,c(1:62)])
modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both"
                          ,trace=F)
Rsq(modeloStepAIC_trans,"varObjCont",data_test)
modeloStepAIC_trans$rank



modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",
                          k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_trans,"varObjCont",data_test)
modeloStepBIC_trans$rank






fulltodo<-lm(varObjCont~., data=data_train)
modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both"
                         ,trace=F)
Rsq(modeloStepAIC_todo,"varObjCont",data_test)


modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both",
                         k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_todo,"varObjCont",data_test)
modeloStepBIC_todo$rank



fullIntT<-lm(varObjCont~.^2, data=data_train)
modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",
                            k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_todoInt,"varObjCont",data_test)
modeloStepBIC_todoInt$rank

modeloManual<-modelo3
modelos<-list(modeloManual,modeloStepAIC,modeloStepBIC,modeloStepAIC_int,modeloStepBIC_int,
              modeloStepAIC_trans,modeloStepBIC_trans,modeloStepAIC_todo,modeloStepBIC_todo)
sapply(modelos,function(x) x$rank)
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))


vcrTodosModelos<-list()
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
    set.seed(123)
    vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
               method = "lm",
               trControl = trainControl(method="repeatedcv", number=5, repeats=20)
    )
    vcrTodosModelos[[i]]<-vcr
}
names(vcrTodosModelos)<-paste0("Model",1:length(modelos),
                               "_",sapply(modelos,function(x) x$rank))
bwplot(resamples(vcrTodosModelos),metric=c("Rsquared"))


summary(resamples(vcrTodosModelos),metric=c("Rsquared"))


coef(modeloStepBIC_trans)
importanciaVariables(modeloStepBIC_todo)








################ REGRESIÓN LOGISTICA
library(car)
library(pROC)
library(questionr)
library(OneR)
par(mar=c(2, 10, 1.1, 1.1))
par(cex=0.7)


varObjBin<-datos2$varObjBin
datos<-datos2 %>%
    select(-c("Censo","PartidoMasVotado","Otros","varObjBin","varObjCont","VOX","Cs"))

datos$aleatorio<-runif(nrow(datos))
datos$aleatorio2<-runif(nrow(datos))
input<-datos
datos$varObjBin<-varObjBin


Transfbin<-Transf_Auto(Filter(is.numeric, input[,-c(31,32)]),varObjBin)
names(Transfbin)

discbin<-droplevels(optbin(data.frame(Filter(is.numeric,input[,-c(31,32)]),varObjBin)))[,
                                                                                        -(ncol(Filter(is.numeric, input[,-c(31,32)]))+1)]
names(discbin)<-paste("disc", names(discbin), sep = "_")

apply(discbin,2,freq)

datos_todobin<-data.frame(varObjBin,input,Transfbin,discbin)


set.seed(123)
trainIndex <- createDataPartition(datos$varObjBin, p=0.8, list=FALSE)
data_train <- datos_todobin[trainIndex,]
data_test <- datos_todobin[-trainIndex,]
modeloInicial<-glm(varObjBin~.,data=data_train[,1:33],family=binomial)
pseudoR2(modeloInicial,data_train,"varObjBin")
modeloInicial$rank
importanciaVariablesLog(modeloInicial)

modelo2<-glm(varObjBin~VotosEmitidos+Abstenciones+Blancos+
                PP+PSOE+CCAA+Age_under19_Ptge+Age_over65_Ptge+
                WomanPopulationPtge+ForeignersPtge+PersonasInmueble+
                UniversityPtge+Empresas+PobChange_pct+
                Explotaciones+PartidoCCAA+WomenUnemploymentPtge+
                UnemploymentPtge+UnemployLess25_Ptge+
                UnemployMore40_Ptge+IndustryUnemploymentPtge+
                ConstructionUnemploymentPtge+AgricultureUnemploymentPtge+
                ServicesUnemploymentPtge+AutonomosPtge,data=data_train,family=binomial)
summary(modelo2)

pseudoR2(modelo2,data_train,"varObjBin")
modelo2$rank
importanciaVariablesLog(modelo2)

modelo3<-glm(varObjBin~VotosEmitidos+Abstenciones+Blancos+
                 PP+PSOE+CCAA+Age_over65_Ptge+
                 WomanPopulationPtge+ForeignersPtge+
                 Empresas+PobChange_pct+
                 Explotaciones+PartidoCCAA+
                 ConstructionUnemploymentPtge+AgricultureUnemploymentPtge+
                 ServicesUnemploymentPtge+AutonomosPtge,data=data_train,family=binomial)
summary(modelo3)

pseudoR2(modelo3,data_train,"varObjBin")
modelo3$rank
importanciaVariablesLog(modelo3)

exp(modelo3$coefficients)




#### Seleccion de variables regresion logistica
null<-glm(varObjBin~1,data=data_train,family=binomial)
full<-glm(varObjBin~.,data=data_train[,1:33],family=binomial)

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both",trace=F)
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward",trace=F)
modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)),trace=F)
modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    k=log(nrow(data_train)),trace=T)



fullT<-glm(varObjBin~.,data=data_train[,1:57],family=binomial)
modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",trace=F)
modeloBackAIC_trans<-step(fullT, scope=list(lower=null, upper=fullT), direction="backward",trace=F)
modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",
                    k=log(nrow(data_train)),trace=F)
modeloBackBIC_trans<-step(fullT, scope=list(lower=null, upper=fullT), direction="backward",
                    k=log(nrow(data_train)),trace=F)


fullTodo<-glm(varObjBin~.,data=data_train,family=binomial)
fullTodoInt<-glm(varObjBin~.^2,data=data_train,family=binomial)

modelos<-list(modelo3,modeloStepAIC,modeloBackAIC,modeloStepBIC,modeloBackBIC,
              modeloStepAIC_trans,modeloBackAIC_trans,modeloStepBIC_trans,modeloBackBIC_trans)

sapply(modelos,function(x) pseudoR2(x,data_test,"varObjBin"))
sapply(modelos,function(x) x$rank)

modelos<-list(modelo3,modeloStepAIC,modeloStepBIC,modeloBackBIC,
              modeloStepAIC_trans,modeloBackAIC_trans,modeloStepBIC_trans,modeloBackBIC_trans)
library(pROC)
sapply(modelos,function(x) plot.roc(data_test$varObjBin,
                               predict(x,data_test,type = "response"),direction="<")$auc)

summary(resamples(vcrTodosModelos),metric=c("ROC"))

auxVarObj<-data_train$varObjBin
data_train$varObjBin<-make.names(data_train$varObjBin)
#formateo la variable objetivo para que funcione el codigo
vcrTodosModelos<-list()
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
    set.seed(1712)
    vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
               method = "glm", family="binomial",metric = "ROC",
               trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                        summaryFunction=twoClassSummary,
                                        classProbs=TRUE)
    )
    vcrTodosModelos[[i]]<-vcr
}
#recupero la variable objetivo en su formato original
data_train$varObjBin<-auxVarObj
names(vcrTodosModelos)<-paste0("Model",1:length(modelos),
                               "_",sapply(modelos,function(x) x$rank))
bwplot(resamples(vcrTodosModelos),metric=c("ROC"))

freq(data_train$varObjBin)
sensEspCorte(modeloStepBIC_trans,data_test,"varObjBin",0.5,"1")

test_roc<-roc(data_test$varObjBin, predict(modeloStepBIC_trans,data_test,
                                           type = "response"), direction="<")

plot(test_roc,print.thres="best")

test_roc$thresholds[which.min(abs(test_roc$sensitivities-test_roc$specificities))]

sensEspCorte(modeloStepBIC_trans,data_test,"varObjBin",0.5,"1")
sensEspCorte(modeloStepBIC_trans,data_test,"varObjBin",0.417,"1")
sensEspCorte(modeloStepBIC_trans,data_test,"varObjBin",0.455,"1")




importanciaVariablesLog(modeloStepBIC_trans)
exp(coef(modeloStepBIC_trans))
pseudoR2(modeloStepBIC_trans,data_train,"varObjBin")
pseudoR2(modeloStepBIC_trans,data_test,"varObjBin")


roc(data_train$varObjBin, predict(modeloStepBIC_trans,data_train,type = "response"),
    direction="<")$auc

roc(data_test$varObjBin, predict(modeloStepBIC_trans,data_test,type = "response"),
    direction="<")$auc



sensEspCorte(modeloStepBIC_trans,data_train,"varObjBin",0.417,"1")
sensEspCorte(modeloStepBIC_trans,data_test,"varObjBin",0.417,"1")