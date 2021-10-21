
library(readr)
library(dplyr)
library(caret)
library(purrr)
library(tidyr)
library(corrplot)
library(Matrix)
library(magrittr)



rm(list=ls())


data=read_delim("Desktop/SB11_20202.txt",delim="¬")
data %<>% select(starts_with("PUNT"),ESTU_GENERO)

data2=is.na(data)

v=rep(1,times=ncol(data2))%>%matrix(ncol=1)
v=data2 %*% v

data=data[v==0,]
data=data[sample(486627,1000),]

#Para cambiar el directorio
#setwd("~/PracticalMLProject")

set.seed(100798)
trainIndex <- createDataPartition(data$ESTU_GENERO, p = .8, 
                                  list = FALSE, 
                                  times = 1)


training<-data[trainIndex,]
testing<-data[-trainIndex,]



# colsnum<-colnames(training)[sapply(training,class)=="numeric"]
# nonmissings<-map_df(training,~sum(!is.na(.x)))
# nonmissings<-gather(nonmissings,variable,valor)
# difvalues<-map_df(training,~length(unique(.x)))
# difvalues<-gather(difvalues,variable,valor)

# to_remove<-difvalues$variable[difvalues$valor<3]
# 
# training<-training %>% dplyr::select(-one_of(to_remove))

# incross<-createDataPartition(training$classe,p=0.3,list=FALSE) 
# crossv<-training[incross,] training<-training[-incross,]


trainingpre=preProcess(training,c("center","scale"))
training2<-predict(trainingpre,training)

# #the following variables [1] "roll_belt"        "total_accel_belt" "max_roll_belt"    "max_picth_belt"   "min_roll_belt"   
# #[6] "min_pitch_belt"   "avg_roll_belt"    "avg_yaw_belt"     "accel_belt_y"
# # are highly correlated with one another will remove all but avg_roll_belt that seems to be the most correlated
# # with the rest of variables, others are detected as full colineality
# 
# removecols<-c("min_yaw_belt","amplitude_pitch_arm" ,
#         "min_roll_dumbbell","max_yaw_forearm","amplitude_yaw_arm","min_yaw_dumbbell")
# 
# training2<-training2 %>% dplyr::select(-one_of(removecols))

# rankfist<-function(i){
#   rankMatrix(training2[colnames(training2)%in% colsnum][1:i])
# }
# 
# fin<-(training2[colnames(training2)%in% colsnum] %>% dim)[2]
# milista<-map(1:fin,rankfist)
# milista2<-map(milista,`[[`,1)
# unlist(milista2)
# colnames(training2[colnames(training2)%in% colsnum])[c(10,66,95)]
# to_remove2<-colnames(training2[colnames(training2)%in% colsnum])[c(10,66,95)]
# training2<-training2 %>% dplyr::select(-one_of(to_remove2))

train_control<-trainControl(method = "cv",number = 10,classProbs = TRUE)
modelo1<-train(data=training,ESTU_GENERO~.,method="multinom")



data1<-predict(trainingpre,testing)
final<-predict(modelo1,data1[,colnames(data1)%in% colnames(training2)])

#################################


modelo2<-train(data=training,ESTU_GENERO~.,method="rf",metric="Accuracy")

data2<-predict(trainingpre,testing)
final2<-predict(modelo2,data1[,colnames(data2)%in% colnames(training2)])

final2==data2$FAMI_ESTU_GENERO


###########################



modelo3<-train(data=training,ESTU_GENERO~.,method="knn")

data3<-predict(trainingpre,testing)


final3<-predict(modelo3,data1[,colnames(data3)%in% colnames(training2)])
data3$ESTU_GENERO %<>% factor(levels=levels(final3))

postResample(pred=final3,obs=data3$ESTU_GENERO)
confusionMatrix(data3$ESTU_GENERO,final3)


#############################

library(caretEnsemble)

model_list=caretList(ESTU_GENERO~.,data=training,
                     trControl = train_control,
                     methodList = c("multinom","rf","knn"))

caretEnsemble(model_list,metric="Accuracy")

