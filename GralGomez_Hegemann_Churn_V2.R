#Loading used libraries
library(FSelector)
library(FactoMineR)
library(caret)
library(corrplot)
library("psych")
library("MASS")
library(reshape)
library(ade4)
library(discretization)
library(ada)
##Prehandling
##1. Create subset of numerical data
##(2.delete correlated features by spearman-correlation)
trainingset=data.frame(read.csv("C:\\Users\\Boon\\Dropbox\\BAPM_WiSe14\\Data\\BAPM_Trainingset.csv", header=TRUE))
testset=data.frame(read.csv("C:\\Users\\Boon\\Dropbox\\BAPM_WiSe14\\Data\\BAPM_Testset.csv", header=TRUE))
#write.table(testset,file="testset.csv",append=FALSE, quote=FALSE,sep=",", eol="\r", na="NA",dec=".",row.names=FALSE,col.names=TRUE)
# delete categorical variables as written in the report, also delete customer_id.
testset=data.frame("churn"=trainingset$churn,testset)
testset$churn=replace(testset$churn,2:nrow(testset),NA)

nummcolidx=grep("churn",names(trainingset))
trainingset=trainingset[,c(nummcolidx, (1:ncol(trainingset))[-nummcolidx])]
completeset=rbind(trainingset,testset)


completesub=subset(completeset, select=-c(Customer_ID,csa,last_swap,adults,age1,age2,car_buy,cartype,children,crtcount,div_type,hnd_webcap,dwllsize,dwlltype,educ1,HHstatin,infobase,kid0_2,kid3_5,kid6_10,kid11_15,kid16_17,lor,mailflag,mailordr,mailresp,marital,mtrcycle,numbcars,occu1,ownrent,pcowner,pre_hnd_price,proptype,REF_QTY,solflag,tot_acpt,tot_ret,wrkwoman,new_cell))

#delete continous variable as written in report due to missing and aggregated features
completesub=subset(completesub, select=-c(retdays,rmcalls,rmmou,rmrev,attempt_Mean,attempt_Range,complete_Mean,complete_Range,drop_blk_Mean,drop_blk_Range,ovrrev_Mean,ovrrev_Range))
#Factorization
factors=c("actvsubs","area","asl_flag","churn","crclscod","creditcd","dualband","ethnic","forgntvl","hnd_price","income","models","phones","prizm_social_one","refurb_new","rv","truck","uniqsubs")
nobinfacts=c("churn")


#delete cats for binary-encoding:
#factors without high-level-cats
factorset=completesub[,(names(completesub) %in% factors) & (!names(completesub) %in% nobinfacts)]
factorsinc=c(names(factorset))
completesub[factorsinc]=lapply(completesub[factorsinc],as.factor)
#nominalfacts=subset(factorset,select=c(uniqsubs,actvsubs,crclscod,hnd_price,income,models,phones))


#splitting AVG6 month in second3-month because data correlates with first 3month
completesub$avg6mou=completesub$avg6mou-completesub$avg3mou
#print(nummclass$avg6mou[1])
completesub$avg6qty=completesub$avg6qty-completesub$avg3qty
completesub$avg6rev=completesub$avg6rev-completesub$avg3rev
names(completesub)[names(completesub)=="avg6mou"]="avg2nd3mou"
names(completesub)[names(completesub)=="avg6qty"]="avg2nd3qty"
names(completesub)[names(completesub)=="avg6rev"]="avg2nd3rev"
nummset=completesub[,!(names(completesub) %in% factors)]
numms=c(names(nummset))


#Reduction of continuous features by Correlation-Based Approach
#Select either pearson, spearman or kendall
corrtype="pearson"
correlationmatrix=cor(nummset, y=NULL, use='pairwise', method=c(corrtype))
#Find high correlated feature; change cutoff for better result
cutoff=0.50
highlycorrelated=findCorrelation(correlationmatrix, cutoff=cutoff)
print(highlycorrelated)
corrplot(correlationmatrix,order="hclust")
nummset=nummset[,-highlycorrelated]
corrplot(correlationmatrix[-highlycorrelated,-highlycorrelated], order="hclust")



##Dummy-Encoding for train
binnumset=nummset
for (i in 1:ncol(binnumset)){
  binnumset[,i][binnumset[,i]>median(nummset[,i],na.rm=TRUE)]=1
  binnumset[,i][binnumset[,i]<=median(nummset[,i],na.rm=TRUE)]=0
}




factors2=sapply(completesub,is.factor)
appset=completesub[,factors]
appset=subset(appset,select=-c(churn))
#Combining levels of actvsubs by 0,1,2,3
levels(appset$actvsubs)=list("0"=c(1),"1"=c(2),"2"=c(3),">=3"=c(4:12))
#Combining Levels of Creditcard with F including all others after E
levels(appset$crclscod)=list(A=c(1:3,52),B=c(4:6),C=c(7:12),D=(14:17),E=c(18:24),F=c(25:37),Z=c(38:51,53,54))
#Combining levels of hnd_price by bis 50, 50-100,100-150,150-200,>200
levels(appset$hnd_price)=list("0-50"=c(1:3),"50-100"=c(4:6),"100-150"=c(7:9),"150-200"=c(10:12),">200"=c(13:17))
#Combining levels of models by 1,2,3,4,>4
levels(appset$models)=list("1"=c(1),"2"=c(2),"3"=c(3),"4"=c(4),">4"=c(5:14))
#combining levels of phones by 1,2,3,4,>4
levels(appset$phones)=list("1"=c(1),"2"=c(2),"3"=c(3),"4"=c(4),">4"=c(5:24))
#combining levels of uniqsubs by 1,2,3,4,>4
levels(appset$uniqsubs)=list("1"=c(1),"2"=c(2),"3"=c(3),"4"=c(4),">4"=c(5:15))


#Binary-Encoding for categoricals train
binfacts=acm.disjonctif(appset)
binfactschurn=data.frame("churn"=completesub$churn,binfacts)
binset=data.frame(binfacts,binnumset)
binsetchurn=data.frame("churn"=completesub$churn,binnumset,binfacts)

trainAdjusted=data.frame("churn"=trainingset$churn,binset[1:50000,])
testAdjusted=data.frame("churn"=testset$churn,"Customer_ID"=testset$Customer_ID,binset[50001:100000,])



##Building the Adaboost-Prediction-Model
##fit 8-split trees
adafit=ada(churn~.,,data=trainAdjusted,iter=20,nu=1,type="discrete")
adafit=addtest(adafit,testAdjusted[,-1],testAdjusted[,1])
plot(adafit,TRUE,TRUE)
varplot(adafit)
predictionvector=predict(adafit,newdata=trainAdjusted,type=c("both"))
predictionprob=predict(adafit,newdata=trainAdjusted,type=c("probs"))
predictionvector=data.frame("Customer_ID"=testset$Customer_ID,predictionvector)
predictionvector=data.frame("Customer_ID"=predictionvector[,1],"EstimatedChurnProbability"=predictionvector[,4])

write.table(predictionvector,file="predictionvector.csv",append=FALSE, quote=FALSE,sep=",", eol="\r", na="NA",dec=".",row.names=FALSE,col.names=TRUE)

