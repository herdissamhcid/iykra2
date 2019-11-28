# library used
library(dplyr)
library(lubridate)
library(norm)
library(Hmisc)
library(MASS)
library(InformationValue)
library(ROCR)
library(usdm)
library(stats)
library(Amelia)
library(caret)
#require(gdata)
#library(Perl)
#require(xlsx)
# read data
populasi=read.csv("", header=TRUE, sep=",")
#boostingdata = read.csv("D:/Sam's/Data/boostingdata.csv", header=TRUE, sep=",")
populasi = populasi[,-1]
populasi = subset(populasi, select = -c(newage))
# check missing values
sapply(populasi,function(x) sum(is.na(x)))

#check unique value
unik = lapply(populasi, function(x) length(unique(x)))

# mapping missing value
#missmap(populasi, main = "Missing values vs observed")

# convert categorical variable to factor
panjangunik = as.data.frame(unik)
category = panjangunik[,(panjangunik[1,])<=2]

for(i in 1:ncol(populasi)){
  for(y in 1:ncol(category))
{
  if(colnames(populasi[i])==colnames(category[y])){
  populasi[,i] <- as.factor(populasi[,i])
  }
  }
}
class(populasi$s1ue)
# net income
#populasi = subset(populasi, populasi$totincome<100000000)
#populasi$netincome = populasi$totincome - populasi$totexpenses

# correlation matrix
# checking correlation matrix
df = populasi
  #subset(       , select = c("newage","totexpenses","angsratio","faskredit",'sex',"jngkawkt","totincome","lmtgl1","jmltgn","vintage","lmkrj1","homstat","default") )
matrixcorr = rcorr(as.matrix(df))

# function to format correlation matrix
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# convert correlation matrix to dataframe
cortable = flattenCorrMatrix(matrixcorr$r, matrixcorr$P)
cortable = as.data.frame(cortable)
#cortable = subset(cortable,cortable$column == c("default"))

# select variable
data = populasi[,c('age2','vintage','faskredit','jngkawkt','angsratio','totincome','lmkrj1')]
cls = sapply(data, is.numeric)
dfvif = data[,cls]
multicol = as.data.frame(vif(data))
multicol = multicol[multicol$VIF>=4.5,]
#data = subset(data,select = -c(totincome,hasil1))
default = populasi$default  
data = cbind(data,default)
#kasus = subset(data, select = -c('totincome'))


# modeling
#modelling
#populasi$incomebin = ifelse(populasi$totincome<10000000,1,ifelse(populasi$totincome<20000000,2,3))
#income = populasi[,c('accno','incomebin',"totincome",'jmltgn')]
#income.under10 = income[income$incomebin==1,]
#income.btwn = income[income$incomebin==2,]
#income.over20 = income[income$incomebin==3,]
#income.under10$ratio = ifelse(income.under10$jmltgn==0,0.28,ifelse((income.under10$jmltgn==1 | income.under10$jmltgn==2),0.41,ifelse((income.under10$jmltgn==3 | income.under10$jmltgn==4),0.53,0.7)))
#income.btwn$ratio = ifelse(income.btwn$jmltgn==0,0.32,ifelse((income.btwn$jmltgn==1 | income.btwn$jmltgn==2),0.46,ifelse((income.btwn$jmltgn==3 | income.btwn$jmltgn==4),0.56,0.68)))
#income.over20$ratio = ifelse(income.over20$jmltgn==0,0.39,ifelse((income.over20$jmltgn==1 | income.over20$jmltgn==2),0.49,ifelse((income.over20$jmltgn==3 | income.over20$jmltgn==4),0.61,0.74)))
populasi$expensebaru = populasi$totincome*0.3
populasi$expns.over6new = ifelse(populasi$expensebaru>6000000,1,0)
set.seed(123456)
d = sort(sample(nrow(populasi), 6388))
train <-populasi[d,]
test <- populasi[-d,]
#trainasu=read.csv("D:/Sam's/Data/trainingbaru.csv", header=TRUE, sep=",")
#trainasu$expns.over6 = ifelse(trainasu$expensebaru>6000000,1,0)
m<-glm(default~ lmkrj1 + angsratio + expns.over6new + jngkawkt + faskredit + jmltgn, data = train, family=binomial())
#expns.over6 + dsr.over40 + faskredit + vintage + age2 + totincome + jmltgn         
#vintage + lmkrj1 + jngkawkt + expns.over6 + dsr.over40 + plincrat,data = train,family=binomial())
#step1<-stepAIC(m,direction = "both")
#step1$anova
summary(m)

# accuracy training data
train$score=predict(m,type=c("response"),train)
Concordance(train$default,train$score)

#accuracy testing data
test$score=predict(m,type=c("response"), test)
Concordance(test$default,test$score)

# Calculate ROC curve
pred = prediction(train$score,train$default)
perf = performance(pred,"tpr","fpr")
plot(perf)
# Calculate AUC 
auc = performance(pred, measure = "auc")
auc@y.values[[1]]


urutan=test[,c('default','score')]
urutan=urutan[with(urutan,order(-score)),]
bodat=c()
for (i in 1:nrow(urutan))
{
  bodat[i]=as.integer(i)
}
urutan=cbind(urutan,bodat)
stepz = nrow(urutan)/10
urutan$binning = cut(urutan$bodat, seq(0,max(urutan$bodat),stepz), labels = c(1:as.integer(max(urutan$bodat)/stepz)))

urutan2=train[,c('default','score')]
urutan2=urutan2[with(urutan2,order(-score)),]
bodat=c()
for (i in 1:nrow(urutan2))
{
  bodat[i]=as.integer(i)
}
urutan2=cbind(urutan2,bodat)
stepz = nrow(urutan2)/10
urutan2$binning = cut(urutan2$bodat, seq(0,max(urutan2$bodat),stepz), labels = c(1:as.integer(max(urutan2$bodat)/stepz)))

#custno = populasi$custno
#data = cbind(data, custno)
write.csv(urutan,file="D:/Sam's/Data/testingnoncaptivecutoff1.csv")
#write.csv(data,file="D:/Sam's/Data/fix.csv")
#write.csv(populasi,file="D:/Sam's/Data/populasi.csv")
write.csv(urutan2,file="D:/Sam's/Data/trainingnoncaptivecutoff1.csv")
#write.csv(cortable, file ="D:/Sam's/Data/korelasi.csv" )

#Binning for scorecard
#-----------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(rmarkdown)
library(scorecard)
populasi$expns.over6 = ifelse(populasi$expns.over6==1,'Yes','No')
populasi$dsr.over40 = ifelse(populasi$dsr.over40==1,'Yes','No')

# split to train and test data
# breaking dt into train and test ------
dt_list = split_df(populasi, "default", ratio = 0.7, seed=23)
dt_train = dt_list$train; dt_test = dt_list$test

#woe binning
breaks_list = list(expns.over6 = c('Yes','No'),
                   dsr.over40 = c('Yes','No'),
                   angsratio = NULL,
                   vintage = NULL,
                   lmkrj1 = NULL,
                   jngkawkt = NULL,
                   plincrat = NULL,
                   angsratio = NULL)
                   #age2 = NULL)
bins = woebin(populasi,y= 'default',x = c('angsratio','vintage','lmkrj1','jngkawkt','plincrat','expns.over6','angsratio','dsr.over40'), breaks_list = breaks_list)

#converting train and test to woe values
train = woebin_ply(dt_train, bins)
test = woebin_ply(dt_test, bins)

m<-glm(default~ plincrat_woe + vintage_woe + lmkrj1_woe + jngkawkt_woe + expns.over6_woe + angsratio_woe ,data = train, family='binomial')
summary(m)
# predicted proability
train$score1 = predict(m, type='response', train)
test$score1 = predict(m, type='response', test)

# # ks & roc plot
perf_eva(train$default, train$score1, title = "train")
perf_eva(test$default, test$score1, title = "test")

#' # scorecard
card = scorecard(bins, m, pdo = 100, basepoints_eq0 = T )
score2 = scorecard_ply(dt_train, card, only_total_score = F)
score3 = scorecard_ply(dt_test, card, only_total_score = F)
train$score2 = score2$score
test$score2 = score3$score
# ks for excel
urutan=test[,c('default','score1','score2')]
urutan=urutan[with(urutan,order(-score1)),]
bodat=c()
for (i in 1:nrow(urutan))
{
  bodat[i]=as.integer(i)
}
urutan=cbind(urutan,bodat)
stepz = nrow(urutan)/10
urutan$binning = cut(urutan$bodat, seq(0,max(urutan$bodat),stepz), labels = c(1:as.integer(max(urutan$bodat)/stepz)))
write.csv(urutan,file="D:/Sam's/Data/cobabobotpervartest.csv")
#perf_eva = (dt_woe$default, dt_pred)
