setwd("~/Desktop/cloud_annual")

## Data process
city_features<-read.csv('city_features.csv',header=T)
city_features$Carbon.dioxide.emissions[which(city_features$year==2000 & city_features$city=='yancheng')]<-
  1055.861643
city_features_s<-read.csv('~/Desktop/interesting_problem/city_features_short.csv')
colnames(city_features_s)[4]<-'CDE'
city_features_s$CDE<-city_features$Carbon.dioxide.emissions
city_features_s<-city_features_s[,2:4]

## Linear Regression
lr<-lm(CDE~.,data=city_features_s)
library(car)
city_features_sm<-city_features_s[-c(220,224,235),]
influencePlot(lr,id=F)
text(0.2570227,-5.9266891,'220')
text(0.2570227,-6.6373274,'235')

pdf("~/Desktop/cloud_annual/draft_paper/influence.pdf")
influencePlot(lr)
dev.off()
lr_m<-lm(CDE~.,data=city_features_sm)

## RF - Determination
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
set.seed(1234)
trControl<-trainControl(method='cv',number=10,search='grid')
tuneGrid<-expand.grid(.mtry=c(90))
store_results<-c()
for(mtry in seq(53,73,10)){
  for(maxnode in seq(10,100,10)){
    for(ntree in seq(200,800,100)){
      tuneGrid<-expand.grid(.mtry=mtry)
      rf_default<-train(Carbon.dioxide.emissions~.,tuneGrid=tuneGrid,
                        maxnodes=maxnode,ntree=ntree,data=city_features,method='rf',metric='Rsquared',trControl=trControl)
      store_results<-rbind(store_results,c(mtry,maxnode,ntree,mean(rf_default$finalModel$rsq),rf_default$results$Rsquared))
    }
  }
}
store_results[which.max(store_results[,4]),]

## RF - 53 70 200 
new_add<-data.frame(matrix(NA,nrow=292,ncol=73))
loc<-1
for(i in levels(city_features_s$city)){
  new_array<-city_features_s$city==i
  new_add[,loc]<-factor(new_array)
  loc<-loc+1
}
colnames(new_add)<-levels(city_features_s$city)
city_features_s<-cbind(city_features_s,(new_add))
city_features_s$city<-NULL

set.seed(1234)
rf_final<-randomForest(CDE~.,data=city_features_s,
                       mtry=53,maxnodes=70,ntree=200,importance=TRUE)
pdf("~/Desktop/cloud_annual/store_plot/ntree_d.pdf")
plot(rf_final)
dev.off()
pdf("~/Desktop/cloud_annual/store_plot/important_city_d.pdf")
varImpPlot(rf_final)
dev.off()
importance(rf_final)

## Prediction for some cities - what matters
city_features<-read.csv('2000-2017.csv',header = T)
city_full<-levels(city_features$City)
city_features$PR[city_features$City=='shanghai'&city_features$Year==2012]<-2396.05
city_inf<-data.frame(city=city_full)

loc<-1
set.seed(1234)
trControl<-trainControl(method='cv',number=10,search='grid')
for(i in city_full){
  data_here<-city_features[city_features$City==i,]
  data_here$City<-NULL
  data_here$Year<-NULL
  store_results<-c()
  for(mtry in seq(2,10,4)){
    for(maxnode in seq(2,16,7)){
      for(ntree in seq(200,800,300)){
        tuneGrid<-expand.grid(.mtry=mtry)
        rf_default<-train(CDE~.,tuneGrid=tuneGrid,
                          maxnodes=maxnode,ntree=ntree,data=data_here,method='rf',metric='Rsquared',trControl=trControl)
        store_results<-rbind(store_results,c(mtry,maxnode,ntree,mean(rf_default$finalModel$rsq),rf_default$results$Rsquared))
        
      }
    }
  }
  final_model<-store_results[which.max(store_results[,4]),]
  city_inf$mtry[loc]<-final_model[1]
  city_inf$maxnodes[loc]<-final_model[2]
  city_inf$ntree[loc]<-final_model[3]
  city_inf$rsq[loc]<-final_model[4]
  rf_final<-randomForest(CDE~.,data=data_here,
                         mtry=final_model[1],maxnodes=final_model[2],ntree=final_model[3],importance=TRUE)
  see<-importance(rf_final)
  save(file=paste0(i,'.RData'),see)
  city_inf$factor1[loc]<-rownames(see)[which.max(see[,2])]
  see<-see[-which.max(see[,2]),]
  city_inf$factor2[loc]<-rownames(see)[which.max(see[,2])]
  loc<-loc+1
}

city_inf[city_inf=='chongqing' | city_inf=='chengdu' | 
           city_inf=='suining' | city_inf=='meishan' | city_inf=='yaan',]
city_inf[city_inf=='changsha' | city_inf=='wuhan' | 
           city_inf=='yichang' | city_inf=='xiangyang' | city_inf=='nanchang',]
city_inf[city_inf=='shanghai' | city_inf=='suzhou' | 
           city_inf=='nanjing' | city_inf=='wuxi' | city_inf=='hangzhou' | city_inf=='hefei',]

## Time changes? 2000 - 2008
city_features<-read.csv('2000-2017.csv',header = T)
city_full<-levels(city_features$City)
city_features$PR[city_features$City=='shanghai'&city_features$Year==2012]<-2396.05
city_inf<-data.frame(city=city_full)

city_features<-city_features[city_features$Year<=2010,]

loc<-1
set.seed(1234)
trControl<-trainControl(method='cv',number=10,search='grid')
for(i in city_full){
  data_here<-city_features[city_features$City==i,]
  data_here$City<-NULL
  data_here$Year<-NULL
  store_results<-c()
  for(mtry in seq(2,10,4)){
    for(maxnode in seq(2,8,3)){
      for(ntree in seq(200,800,300)){
        tuneGrid<-expand.grid(.mtry=mtry)
        rf_default<-train(CDE~.,tuneGrid=tuneGrid,
                          maxnodes=maxnode,ntree=ntree,data=data_here,method='rf',metric='Rsquared',trControl=trControl)
        store_results<-rbind(store_results,c(mtry,maxnode,ntree,mean(rf_default$finalModel$rsq),rf_default$results$Rsquared))
        
      }
    }
  }
  final_model<-store_results[which.max(store_results[,4]),]
  city_inf$mtry[loc]<-final_model[1]
  city_inf$maxnodes[loc]<-final_model[2]
  city_inf$ntree[loc]<-final_model[3]
  city_inf$rsq[loc]<-final_model[4]
  rf_final<-randomForest(CDE~.,data=data_here,
                         mtry=final_model[1],maxnodes=final_model[2],ntree=final_model[3],importance=TRUE)
  see<-importance(rf_final)
  save(file=paste0(i,'2000.RData'),see)
  city_inf$factor1[loc]<-rownames(see)[which.max(see[,2])]
  see<-see[-which.max(see[,2]),]
  city_inf$factor2[loc]<-rownames(see)[which.max(see[,2])]
  loc<-loc+1
}

city_inf[city_inf=='chongqing' | city_inf=='chengdu' | 
           city_inf=='suining' | city_inf=='meishan' | city_inf=='yaan',]
city_inf[city_inf=='changsha' | city_inf=='wuhan' | 
           city_inf=='yichang' | city_inf=='xiangyang' | city_inf=='nanchang',]
city_inf[city_inf=='shanghai' | city_inf=='suzhou' | 
           city_inf=='nanjing' | city_inf=='wuxi' | city_inf=='hangzhou' | city_inf=='hefei',]

## Time changes? 2009 - 20017
city_features<-read.csv('2000-2017.csv',header = T)
city_full<-levels(city_features$City)
city_features$PR[city_features$City=='shanghai'&city_features$Year==2012]<-2396.05
city_inf<-data.frame(city=city_full)

city_features<-city_features[city_features$Year>=2009,]

loc<-1
set.seed(1234)
trControl<-trainControl(method='cv',number=10,search='grid')
for(i in city_full){
  data_here<-city_features[city_features$City==i,]
  data_here$City<-NULL
  data_here$Year<-NULL
  store_results<-c()
  for(mtry in seq(2,10,4)){
    for(maxnode in seq(2,8,3)){
      for(ntree in seq(200,800,300)){
        tuneGrid<-expand.grid(.mtry=mtry)
        rf_default<-train(CDE~.,tuneGrid=tuneGrid,
                          maxnodes=maxnode,ntree=ntree,data=city_features,method='rf',metric='Rsquared',trControl=trControl)
        store_results<-rbind(store_results,c(mtry,maxnode,ntree,mean(rf_default$finalModel$rsq),rf_default$results$Rsquared))
        
      }
    }
  }
  final_model<-store_results[which.max(store_results[,4]),]
  city_inf$mtry[loc]<-final_model[1]
  city_inf$maxnodes[loc]<-final_model[2]
  city_inf$ntree[loc]<-final_model[3]
  city_inf$rsq[loc]<-final_model[4]
  rf_final<-randomForest(CDE~.,data=data_here,
                         mtry=final_model[1],maxnodes=final_model[2],ntree=final_model[3],importance=TRUE)
  see<-importance(rf_final)
  save(file=paste0(i,'2009.RData'),see)
  city_inf$factor1[loc]<-rownames(see)[which.max(see[,2])]
  see<-see[-which.max(see[,2]),]
  city_inf$factor2[loc]<-rownames(see)[which.max(see[,2])]
  loc<-loc+1
}

city_inf[city_inf=='chongqing' | city_inf=='chengdu' | 
           city_inf=='suining' | city_inf=='meishan' | city_inf=='yaan',]
city_inf[city_inf=='changsha' | city_inf=='wuhan' | 
           city_inf=='yichang' | city_inf=='xiangyang' | city_inf=='nanchang',]
city_inf[city_inf=='shanghai' | city_inf=='suzhou' | 
           city_inf=='nanjing' | city_inf=='wuxi' | city_inf=='hangzhou' | city_inf=='hefei',]


## Prediction for real
city_features<-read.csv('2000-2017.csv',header = T)
city_full<-levels(city_features$City)
city_features$PR[city_features$City=='shanghai'&city_features$Year==2012]<-2396.05

year_full<-2000:2017

ep<-matrix(NA,nrow=16,ncol=6)
loc<-1
set.seed(1234)
trControl<-trainControl(method='cv',number=10,search='grid')

for (i in city_full){
  data_here<-city_features[city_features$City==i,]
  data_here$City<-NULL
  
  for (j in 2012:2017){
  
  # psi
  idx_here<-data.frame(Idx=data_here$PSI[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                           mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  psi_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # PTI
  idx_here<-data.frame(Idx=data_here$PTI[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  pti_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # LP
  idx_here<-data.frame(Idx=data_here$LP[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  lp_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # PTAGE
  idx_here<-data.frame(Idx=data_here$PTAGE[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  ptage_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # IUTTH
  idx_here<-data.frame(Idx=data_here$IUTTH[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  iutth_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # PCCEUR
  idx_here<-data.frame(Idx=data_here$PCCEUR[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  pcceur_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # PCCERR
  idx_here<-data.frame(Idx=data_here$PCCERR[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  pccerr_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # PR
  idx_here<-data.frame(Idx=data_here$PR[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  pr_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # PUP
  idx_here<-data.frame(Idx=data_here$PUP[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  pup_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # FD
  idx_here<-data.frame(Idx=data_here$FD[data_here$Year<j],Year=data_here$Year[data_here$Year<j])
  rf_final<-randomForest(Idx~.,data=idx_here,
                         mtry=1,maxnodes=dim(idx_here)[1],ntree=800,importance=TRUE)
  fd_new<-predict(rf_final,newdata=data.frame(Year=j))
  
  # Full
  store_results<-c()
  for(mtry in seq(2,10,4)){
    for(maxnode in round(seq(2,sum(data_here$Year<j)-1,length.out = 3))){
      for(ntree in seq(200,800,300)){
        tuneGrid<-expand.grid(.mtry=mtry)
        rf_default<-train(CDE~.,tuneGrid=tuneGrid,
                          maxnodes=maxnode,ntree=ntree,data=data_here[data_here$Year<j,-1],method='rf',metric='Rsquared',trControl=trControl)
        store_results<-rbind(store_results,c(mtry,maxnode,ntree,mean(rf_default$finalModel$rsq),rf_default$results$Rsquared))
        
      }
    }
  }
  final_model<-store_results[which.max(store_results[,4]),]
  
  rf_final<-randomForest(CDE~.,data=data_here[data_here$Year<j,-1],
                         mtry=final_model[1],maxnodes=final_model[2],ntree=final_model[3],importance=TRUE)
  cde_new<-predict(rf_final,newdata=
                     data.frame(PSI=psi_new,PTI=pti_new,
                                LP=lp_new,PTAGE=ptage_new,IUTTH=iutth_new,
                                PCCEUR=pcceur_new,PCCERR=pccerr_new,
                                PR=pr_new,PUP=pup_new,FD=fd_new))
  ep[loc,j-2011]<-(cde_new-data_here$CDE[data_here$Year==j])/data_here$CDE[data_here$Year==j]
  
  }
  loc<-loc+1
}

## read csv
city_features<-read.csv('2000-2017.csv',header = T)
city_full<-levels(city_features$City)
for(i in city_full){
  file_here<-paste0(i,'.RData')
  load(file_here)
  write.csv(file=paste0(i,'.csv'),see)
}


