###### 20200911##############
###### Chao Sang  ###########

#~ ~ data analysis ~ ~----
#1. basic characteristic----
#(1) discovery_set----
#~ all----
data.frm<-read.csv("discovery_set.csv")

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ early----
data_discovery_set<-read.csv("discovery_set.csv")

data.frm<-data_discovery_set[which(data_discovery_set$group=="S0_2"),]
#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ late----
data_discovery_set<-read.csv("discovery_set.csv")

data.frm<-data_discovery_set[which(data_discovery_set$group=="S34"),]

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ normality----
data.frm<-read.csv("discovery_set.csv")

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')
num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c("p_normality")
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-shapiro.test(idx)[["p.value"]]
}

#~ Mann-Whitney test----
data.frm<-read.csv("discovery_set.csv")

idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c('P_MW-u-test')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-as.numeric(data.frm[,idx_msr[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  rst_msr[n,1]<-wilcox.test(idx~Y)[["p.value"]]
}

#~ X2 test----
data.frm<-read.csv("discovery_set.csv")

idx_cnt<-c('DM.IFG',	'Sex')

num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 1)
colnames(rst_cnt)<-c('X2-test')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-as.numeric(data.frm[,idx_cnt[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  tab<-table(idx,Y)
  print(tab)
  rst_cnt[n,1]<-chisq.test(tab)[["p.value"]]
}

#(2) validation_set1----
#~ all----
data.frm<- read.csv("validation_set1.csv")

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ early----
data_validation_set1<- read.csv("validation_set1.csv")
data.frm<-data_validation_set1[which(data_validation_set1$group=="S0_2"),]

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ late----
data_validation_set1<- read.csv("validation_set1.csv")

data.frm<-data_validation_set1[which(data_validation_set1$group=="S34"),]

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ normality----
data.frm<- read.csv("validation_set1.csv")

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c("p_normality")
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-shapiro.test(idx)[["p.value"]]
}

#~ t test----
data.frm<- read.csv("validation_set1.csv")

idx_msr<-c('PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c('P_t-test')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-as.numeric(data.frm[,idx_msr[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  rst_msr[n,1]<-t.test(idx~Y)[["p.value"]]
}

#~ Mann-Whitney test----
data.frm<- read.csv("validation_set1.csv")

idx_msr<-c('Age',	'ALB',	'ALT',	'AST', 'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c('P_MW-u-test')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-as.numeric(data.frm[,idx_msr[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  rst_msr[n,1]<-wilcox.test(idx~Y)[["p.value"]]
}

#~ X2 test----
data.frm<- read.csv("validation_set1.csv")

idx_cnt<-c('DM.IFG',	'Sex')

num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 1)
colnames(rst_cnt)<-c('X2-test')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-as.numeric(data.frm[,idx_cnt[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  tab<-table(idx,Y)
  print(tab)
  rst_cnt[n,1]<-chisq.test(tab)[["p.value"]]
}

#~ Fisher exact test----
data.frm<- read.csv("validation_set1.csv")

idx_cnt<-c('DM.IFG')

num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 1)
colnames(rst_cnt)<-c('fisher-test')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-as.numeric(data.frm[,idx_cnt[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  tab<-table(idx,Y)
  print(tab)
  rst_cnt[n,1]<-fisher.test(tab)[["p.value"]]
}

#(3) validation_set2----
#~ all----
data.frm<- read.csv("validation_set2.csv")

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ early----
data_validation_set2<- read.csv("validation_set2.csv")

data.frm<-data_validation_set2[which(data_validation_set2$group=="S0_2"),]

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ late----
data_validation_set2<- read.csv("validation_set2.csv")

data.frm<-data_validation_set2[which(data_validation_set2$group=="S34"),]

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 3)
colnames(rst_msr)<-c('mean','sd','meansd')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-mean(idx)
  rst_msr[n,2]<-sd(idx)
}
rst_msr[,3]<-paste0(round(rst_msr[,1],2),"¡À",round(rst_msr[,2],2))

#count data
idx_cnt<-c('DM.IFG',	'Sex')
num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 3)
colnames(rst_cnt)<-c('N1','N2','N1N2')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-data.frm[,idx_cnt[n]]
  rst_cnt[n,1:2]<-table(idx) 
}
rst_cnt[,3]<-paste0(rst_cnt[,1],":",rst_cnt[,2])

#~ normality----
data.frm<- read.csv("validation_set2.csv")

#measurement data
idx_msr<-c('Age',	'ALB',	'ALT',	'AST',	'BMI', 'FBG',	'GGT',	'HbA1c',
           'HDL',	'LDL',	'PLT',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c("p_normality")
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-data.frm[,idx_msr[n]]
  rst_msr[n,1]<-shapiro.test(idx)[["p.value"]]
}

#~ t test----
data.frm<- read.csv("validation_set2.csv")

idx_msr<-c('ALB','HDL','LDL','PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c('P_t-test')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-as.numeric(data.frm[,idx_msr[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  rst_msr[n,1]<-t.test(idx~Y)[["p.value"]]
}

#~ Mann-Whitney test----
data.frm<- read.csv("validation_set2.csv")

idx_msr<-c('Age',	'ALT',	'AST', 'BMI', 'FBG',	'GGT',	'HbA1c',
           'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

num_msr <- as.numeric(length(idx_msr))
rst_msr <- matrix(nrow = num_msr,ncol = 1)
colnames(rst_msr)<-c('P_MW-u-test')
rownames(rst_msr)<-idx_msr

for (n in 1:num_msr) {
  idx<-as.numeric(data.frm[,idx_msr[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  rst_msr[n,1]<-wilcox.test(idx~Y)[["p.value"]]
}

#~ X2 test----
data.frm<- read.csv("validation_set2.csv")

idx_cnt<-c('DM.IFG',	'Sex')

num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 1)
colnames(rst_cnt)<-c('X2-test')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-as.numeric(data.frm[,idx_cnt[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  tab<-table(idx,Y)
  print(tab)
  rst_cnt[n,1]<-chisq.test(tab)[["p.value"]]
}

#~ Fisher exact test----
data.frm<- read.csv("validation_set2.csv")

idx_cnt<-c('DM.IFG')

num_cnt <- as.numeric(length(idx_cnt))
rst_cnt <- matrix(nrow = num_cnt,ncol = 1)
colnames(rst_cnt)<-c('fisher-test')
rownames(rst_cnt)<-idx_cnt

for (n in 1:num_cnt) {
  idx<-as.numeric(data.frm[,idx_cnt[n]])
  Y<-as.numeric(as.factor(data.frm$group))
  tab<-table(idx,Y)
  print(tab)
  rst_cnt[n,1]<-fisher.test(tab)[["p.value"]]
}

#2. variables selection----
#(1)  ROC----
library(pROC)
data.frm<-read.csv("discovery_set.csv")

Y <- data.frm$group
pm <- data.frm[,idx]
pm.name<-data.matrix(colnames(pm))
lg <- as.numeric(length(pm.name))
auc <- matrix(nrow=lg,ncol=1)

Y<-(Y %>% as.factor %>% as.numeric)-1

for (i in 1:lg) {
  pm1<-pm[,i]
  auc[i,] <-auc(roc(Y,pm1))
}
roc <- cbind(pm.name,auc)

#(2)  stepwise logistic regression----
data.frm<-read.csv("discovery_set.csv")
idx<-c('Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
       'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	'AST.ALT',	'AST.PLT')

g_frm <- data.frm$group
pm_frm <- data.frm[,idx]

Y <- g_frm
dataT <- cbind(Y,pm_frm)
dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")
model_step<-step(object = model,trace = 0)
summary(model_step)

#(3) varibles free combination----
library(stats)
library(pROC)
library(PRROC)
library(magrittr)
data_train<-read.csv("discovery_set.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')
data_train<-data_train[,prmtr]

data_train$AST.ALT<-(data_train$AST)/(data_train$ALT)
data_train$AST.PLT<-(data_train$AST)/(data_train$PLT)

idx<-c('Age', 'ALT', 'AST',	'BMI', 'DM.IFG', 'FBG', 'GGT',	'HbA1c', 
       'LDL', 'PLT', 'Sex', 'TG', 'AST.ALT', 'AST.PLT')

g_train <- data_train$group
pm_train <- data_train[,idx]

lg_idx<-as.numeric(length(idx))

for (n in 2:lg_idx) {
  x <- choose(lg_idx,n)
  a <- t(combn(idx,n))
  b <- matrix(nrow = x,ncol = 12 )
  colnames(b)<-c("accuracy","P_a","P_b","R_a","R_b","F_a","F_b",
                 "auROC","auPR","threshold","specificity","sensitivity")
  rst <- cbind(b,a)
  for (m in 1:x) {

    am <- a[m,]
    Y <- g_train
    dataT <- cbind(Y,pm_train[,c(am)])
    dataV <- dataT
    
    dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
    dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

    model<-glm(Y ~ ., data=dataT,family = "binomial")
   
    pred <- predict(model, dataV, type='response')
    
    lg <- as.numeric(length(pred))
    for (s in 1:lg) {
      if(pred[s]<0.5){dataV$class[s]<-0}else {dataV$class[s]<-1}
    }
    
    tab <- table(dataV$Y,dataV$class)
    accu <- sum(diag(prop.table(tab)))
    P_a <- diag(prop.table(tab,2))[1]
    P_b <- diag(prop.table(tab,2))[2]
    R_a <- diag(prop.table(tab,1))[1]
    R_b <- diag(prop.table(tab,1))[2]
    F1_a <- 2*P_a*R_a/(P_a+R_a)
    F1_b <- 2*P_b*R_b/(P_b+R_b)
    
    roc <- roc(dataV$Y, pred)
    auROC <- auc(roc)
    best_cut <- as.matrix(coords(roc, 'best', transpose = FALSE))
    
    pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
    auPR <- pr$auc.integral
    
    row<-m
    rst[row,1]<-accu
    rst[row,2]<-P_a
    rst[row,3]<-P_b
    rst[row,4]<-R_a
    rst[row,5]<-R_b
    rst[row,6]<-F1_a
    rst[row,7]<-F1_b
    rst[row,8]<-auROC
    rst[row,9]<-auPR
    rst[row,10:12]<-best_cut
  }
  write.csv(rst,paste0("discovery_glm_free_cmbn_idx",n,".csv"))
}

#3. Model construction and validation----
library(gbm)
library(randomForest)
library(stats)
library(pROC)
library(PRROC)
library(magrittr)

#(1) discovery_set----
data_discovery_set<-read.csv("discovery_set.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')
data_train<-data_discovery_set[,prmtr]

data_train$AST.ALT<-(data_train$AST)/(data_train$ALT)
data_train$AST.PLT<-(data_train$AST)/(data_train$PLT)

rst <- matrix(nrow = 3,ncol =12 )
colnames(rst)<-c("accuracy","P_a","P_b","R_a","R_b","F_a","F_b","auROC","auPR","threshold","specificity","sensitivity")
rownames(rst)<-c("discovery_set","validation_set1","validation_set2")

idx<-c('Age',	'ALT',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'TG',	'AST.PLT')

g_train <- data_train$group
pm_train <- data_train[,idx]
FIB4_train<-data_train$FIB4

Y <- g_train
FIB4<-FIB4_train
dataT <- cbind(Y,pm_train)
dataV <- dataT

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")
summary(model)

pred <- predict(model, dataV, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

thrd<-best_cut[1]

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-1
rst[row,1]<-accu
rst[row,2]<-P_a
rst[row,3]<-P_b
rst[row,4]<-R_a
rst[row,5]<-R_b
rst[row,6]<-F1_a
rst[row,7]<-F1_b
rst[row,8]<-auROC
rst[row,9]<-auPR
rst[row,10:12]<-best_cut

#(2) validation_set1----
data_validation_set1 <- read.csv("validation_set1.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')

data_valid1<-data_validation_set1[,prmtr]
data_valid1$AST.ALT<-(data_valid1$AST)/(data_valid1$ALT)
data_valid1$AST.PLT<-(data_valid1$AST)/(data_valid1$PLT)

g_valid1 <- data_valid1$group
pm_valid1 <- data_valid1[,idx]
FIB4_valid1 <- data_valid1$FIB4

Y <- g_train
FIB4<-FIB4_train
dataT <- cbind(Y,pm_train)
Y <- g_valid1
FIB4<-FIB4_valid1
dataV <- cbind(Y,pm_valid1)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")

pred <- predict(model, dataV, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

thrd<-best_cut[1]

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-2
rst[row,1]<-accu
rst[row,2]<-P_a
rst[row,3]<-P_b
rst[row,4]<-R_a
rst[row,5]<-R_b
rst[row,6]<-F1_a
rst[row,7]<-F1_b
rst[row,8]<-auROC
rst[row,9]<-auPR
rst[row,10:12]<-best_cut

#(3) validation_set2----
data_validation_set2 <- read.csv("validation_set2.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')

data_valid2<-data_validation_set2[,prmtr]
data_valid2$AST.ALT<-(data_valid2$AST)/(data_valid2$ALT)
data_valid2$AST.PLT<-(data_valid2$AST)/(data_valid2$PLT)

g_valid2 <- data_valid2$group
pm_valid2 <- data_valid2[,idx]
FIB4_valid2 <- data_valid2$FIB4

Y <- g_train
FIB4<-FIB4_train
dataT <- cbind(Y,pm_train)
Y <- g_valid2
FIB4<-FIB4_valid2
dataV <- cbind(Y,pm_valid2)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")

pred <- predict(model, dataV, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

thrd<-best_cut[1]

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-3
rst[row,1]<-accu
rst[row,2]<-P_a
rst[row,3]<-P_b
rst[row,4]<-R_a
rst[row,5]<-R_b
rst[row,6]<-F1_a
rst[row,7]<-F1_b
rst[row,8]<-auROC
rst[row,9]<-auPR
rst[row,10:12]<-best_cut

#(4) independence evaluation~ ~ ~ ----
formatFit<-function(fit){
  p<-summary(fit)$coefficients[,4]
  wald<-summary(fit)$coefficients[,3]^2
  valueB<-coef(fit)
  valueOR<-exp(coef(fit))
  confitOR<-exp(confint(fit))
  data.frame(
    B=round(valueB,3),
    Wald=round(wald,3),
    OR_with_CI=paste(round(valueOR,3),"(",
                     round(confitOR[,1],3),"~",round(confitOR[,2],3),")",sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
}

data_train<-read.csv("discovery_set.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')
data_train<-data_train[,prmtr]

data_train$AST.ALT<-(data_train$AST)/(data_train$ALT)
data_train$AST.PLT<-(data_train$AST)/(data_train$PLT)

idx<-c('Age',	'ALT',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'TG',	'AST.PLT')

g_train <- data_train$group
pm_train <- data_train[,idx]

#~ discovery_set----
Y <- g_train
dataT <- cbind(Y,pm_train)
dataV <- cbind(Y,data_train)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")
dataV$pred <- predict(model, dataV, type='link')

fit<-glm(Y ~ pred, data=dataV,family = "binomial")
formatFit(fit)

fit<-glm(Y ~ pred+AST+HbA1c+PLT+AST.ALT+Sex, data=dataV,family = "binomial")
formatFit(fit)

#~ validation_set1----
data_validation_set1 <- read.csv("validation_set1.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')

data_valid1<-data_validation_set1[,prmtr]
data_valid1$AST.ALT<-(data_valid1$AST)/(data_valid1$ALT)
data_valid1$AST.PLT<-(data_valid1$AST)/(data_valid1$PLT)

g_valid1 <- data_valid1$group
pm_valid1 <- data_valid1[,idx]

Y <- g_train
dataT <- cbind(Y,pm_train)
Y <- g_valid1
dataV <- cbind(Y,data_valid1)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")
dataV$pred <- predict(model, dataV, type='link')

fit<-glm(Y ~ pred, data=dataV,family = "binomial")
formatFit(fit)

fit<-glm(Y ~ pred+AST+HbA1c+PLT+AST.ALT+Sex, data=dataV,family = "binomial")
formatFit(fit)

#~ validation_set2----
data_validation_set2 <- read.csv("validation_set2.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')

data_valid2<-data_validation_set2[,prmtr]
data_valid2$AST.ALT<-(data_valid2$AST)/(data_valid2$ALT)
data_valid2$AST.PLT<-(data_valid2$AST)/(data_valid2$PLT)

g_valid2 <- data_valid2$group
pm_valid2 <- data_valid2[,idx]

Y <- g_train
dataT <- cbind(Y,pm_train)
Y <- g_valid2
dataV <- cbind(Y,data_valid2)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

model<-glm(Y ~ ., data=dataT,family = "binomial")
dataV$pred <- predict(model, dataV, type='link')

fit<-glm(Y ~ pred, data=dataV,family = "binomial")
formatFit(fit)

fit<-glm(Y ~ pred+AST+HbA1c+PLT+AST.ALT+Sex, data=dataV,family = "binomial")
formatFit(fit)

#(5) Performance comparison----
#~ discovery_set----
data_train<-read.csv("discovery_set.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')
data_train<-data_train[,prmtr]

data_train$AST.ALT<-(data_train$AST)/(data_train$ALT)
data_train$AST.PLT<-(data_train$AST)/(data_train$PLT)

rst1 <- matrix(nrow = 3,ncol =12 )
colnames(rst1)<-c("accuracy","P_a","P_b","R_a","R_b","F_a","F_b","auROC","auPR","threshold","specificity","sensitivity")
rownames(rst1)<-c("GB","RF","LR")

idx<-c('Age',	'ALT',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'TG',	'AST.PLT')

g_train <- data_train$group
pm_train <- data_train[,idx]
FIB4_train<-data_train$FIB4

Y <- g_train
FIB4<-FIB4_train
dataT <- cbind(Y,pm_train)
dataV <- dataT

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

#~ GB----
set.seed(12345)
model <- gbm(Y ~ ., data=dataT, distribution = "bernoulli")

best.iter <- gbm.perf(model)
pred <- predict(model, dataV, n.trees=best.iter, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-1
rst1[row,1]<-accu
rst1[row,2]<-P_a
rst1[row,3]<-P_b
rst1[row,4]<-R_a
rst1[row,5]<-R_b
rst1[row,6]<-F1_a
rst1[row,7]<-F1_b
rst1[row,8]<-auROC
rst1[row,9]<-auPR
rst1[row,10:12]<-best_cut

#~ RF----
set.seed(12345)
model <- randomForest(factor(Y) ~ .,ntree=500,mtry=2,data=dataT)
pred <-model[["votes"]][,2]

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-2
rst1[row,1]<-accu
rst1[row,2]<-P_a
rst1[row,3]<-P_b
rst1[row,4]<-R_a
rst1[row,5]<-R_b
rst1[row,6]<-F1_a
rst1[row,7]<-F1_b
rst1[row,8]<-auROC
rst1[row,9]<-auPR
rst1[row,10:12]<-best_cut

#~ LR----
model<-glm(Y ~ ., data=dataT,family = "binomial")
pred <- predict(model, dataV, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-3
rst1[row,1]<-accu
rst1[row,2]<-P_a
rst1[row,3]<-P_b
rst1[row,4]<-R_a
rst1[row,5]<-R_b
rst1[row,6]<-F1_a
rst1[row,7]<-F1_b
rst1[row,8]<-auROC
rst1[row,9]<-auPR
rst1[row,10:12]<-best_cut

#~ validation_set1----
rst2 <- matrix(nrow = 3,ncol =12 )
colnames(rst2)<-c("accuracy","P_a","P_b","R_a","R_b","F_a","F_b","auROC","auPR","threshold","specificity","sensitivity")
rownames(rst2)<-c("GB","RF","LR")

data_validation_set1 <- read.csv("validation_set1.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')

data_valid1<-data_validation_set1[,prmtr]
data_valid1$AST.ALT<-(data_valid1$AST)/(data_valid1$ALT)
data_valid1$AST.PLT<-(data_valid1$AST)/(data_valid1$PLT)

g_valid1 <- data_valid1$group
pm_valid1 <- data_valid1[,idx]
FIB4_valid1 <- data_valid1$FIB4

Y <- g_train
FIB4<-FIB4_train
dataT <- cbind(Y,pm_train)
Y <- g_valid1
FIB4<-FIB4_valid1
dataV <- cbind(Y,pm_valid1)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

#~ GB----
set.seed(12345)
model <- gbm(Y ~ ., data=dataT, distribution = "bernoulli")

best.iter <- gbm.perf(model)
pred <- predict(model, dataV, n.trees=best.iter, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-1
rst2[row,1]<-accu
rst2[row,2]<-P_a
rst2[row,3]<-P_b
rst2[row,4]<-R_a
rst2[row,5]<-R_b
rst2[row,6]<-F1_a
rst2[row,7]<-F1_b
rst2[row,8]<-auROC
rst2[row,9]<-auPR
rst2[row,10:12]<-best_cut

#~ RF----
set.seed(12345)
model <- randomForest(factor(Y) ~ .,ntree=500,mtry=2,data=dataT)
pred <- predict(model, dataV, type='prob')[, 2]

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-2
rst2[row,1]<-accu
rst2[row,2]<-P_a
rst2[row,3]<-P_b
rst2[row,4]<-R_a
rst2[row,5]<-R_b
rst2[row,6]<-F1_a
rst2[row,7]<-F1_b
rst2[row,8]<-auROC
rst2[row,9]<-auPR
rst2[row,10:12]<-best_cut

#~ LR----
model<-glm(Y ~ ., data=dataT,family = "binomial")
pred <- predict(model, dataV, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-3
rst2[row,1]<-accu
rst2[row,2]<-P_a
rst2[row,3]<-P_b
rst2[row,4]<-R_a
rst2[row,5]<-R_b
rst2[row,6]<-F1_a
rst2[row,7]<-F1_b
rst2[row,8]<-auROC
rst2[row,9]<-auPR
rst2[row,10:12]<-best_cut

#~ validation_set2----
rst3 <- matrix(nrow = 3,ncol =12 )
colnames(rst3)<-c("accuracy","P_a","P_b","R_a","R_b","F_a","F_b","auROC","auPR","threshold","specificity","sensitivity")
rownames(rst3)<-c("GB","RF","LR")

data_validation_set2 <- read.csv("validation_set2.csv")
prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG',	 'FIB4',	'NFS')

data_valid2<-data_validation_set2[,prmtr]
data_valid2$AST.ALT<-(data_valid2$AST)/(data_valid2$ALT)
data_valid2$AST.PLT<-(data_valid2$AST)/(data_valid2$PLT)

g_valid2 <- data_valid2$group
pm_valid2 <- data_valid2[,idx]
FIB4_valid2 <- data_valid2$FIB4

Y <- g_train
FIB4<-FIB4_train
dataT <- cbind(Y,pm_train)
Y <- g_valid2
FIB4<-FIB4_valid2
dataV <- cbind(Y,pm_valid2)

dataT$Y <- (dataT$Y %>% as.factor %>% as.numeric)-1
dataV$Y <- (dataV$Y %>% as.factor %>% as.numeric)-1

#~ GB----
set.seed(12345)
model <- gbm(Y ~ ., data=dataT, distribution = "bernoulli")

best.iter <- gbm.perf(model)
pred <- predict(model, dataV, n.trees=best.iter, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-1
rst3[row,1]<-accu
rst3[row,2]<-P_a
rst3[row,3]<-P_b
rst3[row,4]<-R_a
rst3[row,5]<-R_b
rst3[row,6]<-F1_a
rst3[row,7]<-F1_b
rst3[row,8]<-auROC
rst3[row,9]<-auPR
rst3[row,10:12]<-best_cut

#~ RF----
set.seed(12345)
model <- randomForest(factor(Y) ~ .,ntree=500,mtry=2,data=dataT)
pred <- predict(model, dataV, type='prob')[, 2]

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-2
rst3[row,1]<-accu
rst3[row,2]<-P_a
rst3[row,3]<-P_b
rst3[row,4]<-R_a
rst3[row,5]<-R_b
rst3[row,6]<-F1_a
rst3[row,7]<-F1_b
rst3[row,8]<-auROC
rst3[row,9]<-auPR
rst3[row,10:12]<-best_cut

#~ LR----
model<-glm(Y ~ ., data=dataT,family = "binomial")
pred <- predict(model, dataV, type='response')

roc <- roc(dataV$Y, pred)
auROC <- auc(roc)
best_cut <- as.matrix(coords(roc, 0.8, "specificity", transpose = FALSE))

dataV$class<-ifelse(pred<0.5,0,1)

tab <- table(dataV$Y,dataV$class)
accu <- sum(diag(prop.table(tab)))
P_a <- diag(prop.table(tab,2))[1]
P_b <- diag(prop.table(tab,2))[2]
R_a <- diag(prop.table(tab,1))[1]
R_b <- diag(prop.table(tab,1))[2]
F1_a <- 2*P_a*R_a/(P_a+R_a)
F1_b <- 2*P_b*R_b/(P_b+R_b)

pr <- pr.curve(pred[dataV$Y==1] %>% na.omit,pred[dataV$Y==0] %>% na.omit)
auPR <- pr$auc.integral

row<-3
rst3[row,1]<-accu
rst3[row,2]<-P_a
rst3[row,3]<-P_b
rst3[row,4]<-R_a
rst3[row,5]<-R_b
rst3[row,6]<-F1_a
rst3[row,7]<-F1_b
rst3[row,8]<-auROC
rst3[row,9]<-auPR
rst3[row,10:12]<-best_cut

#~ ~ paper drawing ~ ~----
library(ggplot2)
library(magrittr)
library(pROC)
library(PRROC)

library(dplyr)
library(knitr)
library(reshape2)
library(ggsignif)
library(RColorBrewer)

data_discovery_set<-read.csv("discovery_set.csv")
data_validation_set1 <- read.csv("validation_set1.csv")
data_validation_set2 <- read.csv("validation_set2.csv")

prmtr<-c('group', 'Age',	'ALB',	'ALT',	'AST',	'BMI',	'DM.IFG',	'FBG',	'GGT',	'HbA1c',
         'HDL',	'LDL',	'PLT',	'Sex',	'TBIL',	'TC',	'TG', 'FIB4',	'NFS')

discovery_df <- data_discovery_set[,prmtr]
discovery_df$AST.ALT<-(discovery_df$AST)/(discovery_df$ALT)
discovery_df$AST.PLT<-(discovery_df$AST)/(discovery_df$PLT)

validation_df1<-data_validation_set1[,prmtr]
validation_df1$AST.ALT<-(validation_df1$AST)/(validation_df1$ALT)
validation_df1$AST.PLT<-(validation_df1$AST)/(validation_df1$PLT)


validation_df2<-data_validation_set2[,prmtr]
validation_df2$AST.ALT<-(validation_df2$AST)/(validation_df2$ALT)
validation_df2$AST.PLT<-(validation_df2$AST)/(validation_df2$PLT)

cols <- c("LR" = "#9900CC", "FIB-4" = "#999999", "NFS" = "#FFCC00")

#1. Model Construction and Validation in NAFLD Cohorts----
#1.1 discovery_set----
##(1) LR Models on discovery_set----
LR_model <- glm(as.numeric(factor(group))-1 ~ Age+ALT+BMI+DM.IFG+FBG+GGT+TG+AST.PLT,
                data=discovery_df, family = "binomial")

##(2.1) ROC of LR Models in discovery_set----
LR_train <- predict(LR_model, discovery_df, type='link')
ROC_LR_train <- roc(discovery_df$group, LR_train)


ROC_LR_train_CI<-round(ci(ROC_LR_train),2)
ROC_LR_train_P95CI<-paste0(ROC_LR_train_CI[1]," - ",ROC_LR_train_CI[3])

##(2.2) ROC of FIB-4 in discovery_set----
ROC_FIB4_train <- roc(discovery_df$group, discovery_df$FIB4)
ROC_FIB4_train_CI<-round(ci(ROC_FIB4_train),2)
ROC_FIB4_train_P95CI<-paste0(ROC_FIB4_train_CI[1]," - ",ROC_FIB4_train_CI[3])

##(2.3) ROC of NFS in discovery_set----
ROC_NFS_train <- roc(discovery_df$group, discovery_df$NFS)
ROC_NFS_train_CI<-round(ci(ROC_NFS_train),2)
ROC_NFS_train_P95CI<-paste0(ROC_NFS_train_CI[1]," - ",ROC_NFS_train_CI[3])

##(3.1) Combination and ROC Plotting in discovery_set----
ROC_disc_df <- rbind(
  data.frame(Specificity=ROC_LR_train$specificities,
             Sensitivity=ROC_LR_train$sensitivities,
             Method='LR', Data='Discovery', Class='S0_2 vs S34',
             AUC=round(ROC_LR_train$auc, 2),
             CI=ROC_LR_train_P95CI),
  data.frame(Specificity=ROC_FIB4_train$specificities,
             Sensitivity=ROC_FIB4_train$sensitivities,
             Method='FIB-4', Data='Discovery', Class='S0_2 vs S34',
             AUC=round(ROC_FIB4_train$auc, 2),
             CI=ROC_FIB4_train_P95CI),
  data.frame(Specificity=ROC_NFS_train$specificities,
             Sensitivity=ROC_NFS_train$sensitivities,
             Method='NFS', Data='Discovery', Class='S0_2 vs S34',
             AUC=round(ROC_NFS_train$auc, 2),
             CI=ROC_NFS_train_P95CI)
)

ROC_disc_df$Method <- factor(ROC_disc_df$Method, levels=c('LR', 'FIB-4','NFS'))
ROC_disc_df_text <- unique(ROC_disc_df[, 3:7])
ROC_disc_df_text$Text <- paste0(ROC_disc_df_text$Method, ' (AUROC=', 
                                ROC_disc_df_text$AUC, ' [',ROC_disc_df_text$CI, '])')
ROC_disc_df_text$X <- .6
lg<-length(ROC_disc_df_text$Method)
for (y in 1:lg) {
  if(ROC_disc_df_text$Method[y]=='LR') {ROC_disc_df_text$Y[y] <-.17}else{
    if(ROC_disc_df_text$Method[y]=='FIB-4') {ROC_disc_df_text$Y[y] <-.1}else{
      if(ROC_disc_df_text$Method[y]=='NFS') {ROC_disc_df_text$Y[y] <-.03}}}
}

train_ROC_plot<-ggplot(aes(x=1-Specificity, y=Sensitivity, color=Method), data=ROC_disc_df) + 
  geom_path(size=1) + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dashed') +
  ggtitle('ROC curve of discovery_set') + 
  scale_color_manual(values=cols)+
  geom_text(data=ROC_disc_df_text, aes(x=X, y=Y, label=Text), size=4.5, fontface = "bold") + 
  theme_bw() +
  theme(text=element_text(size=16), legend.position='none',
        plot.title = element_text(hjust = 0.5)) 
train_ROC_plot
ggsave(file="train_ROC_plot.pdf",plot=train_ROC_plot,width=4.8,height=5)

#1.2 validation_set1----
##(1.1) ROC of LR Models in validation_set1----
LR_vld1 <- predict(LR_model, validation_df1, type='link')

ROC_LR_vld1 <- roc(validation_df1$group, LR_vld1)
ROC_LR_vld1_CI<-round(ci(ROC_LR_vld1),2)
ROC_LR_vld1_P95CI<-paste0(ROC_LR_vld1_CI[1]," - ",ROC_LR_vld1_CI[3])

##(1.2) ROC of FIB-4 in validation_set1----
ROC_FIB4_vld1 <- roc(validation_df1$group, validation_df1$FIB4)
ROC_FIB4_vld1_CI<-round(ci(ROC_FIB4_vld1),2)
ROC_FIB4_vld1_P95CI<-paste0(ROC_FIB4_vld1_CI[1]," - ",ROC_FIB4_vld1_CI[3])

##(1.3) ROC of NFS in validation_set1----
ROC_NFS_vld1 <- roc(validation_df1$group, validation_df1$NFS)
ROC_NFS_vld1_CI<-round(ci(ROC_NFS_vld1),2)
ROC_NFS_vld1_P95CI<-paste0(ROC_NFS_vld1_CI[1]," - ",ROC_NFS_vld1_CI[3])

##(2.1) Combination and ROC Plotting in validation_set1----
ROC_vld1_df <- rbind(
  data.frame(Specificity=ROC_LR_vld1$specificities,
             Sensitivity=ROC_LR_vld1$sensitivities,
             Method='LR', Data='Validation', Class='S0_2 vs S34',
             AUC=round(ROC_LR_vld1$auc, 2),
             CI=ROC_LR_vld1_P95CI),
  data.frame(Specificity=ROC_FIB4_vld1$specificities,
             Sensitivity=ROC_FIB4_vld1$sensitivities,
             Method='FIB-4', Data='Validation', Class='S0_2 vs S34',
             AUC=round(ROC_FIB4_vld1$auc, 2),
             CI=ROC_FIB4_vld1_P95CI),
  data.frame(Specificity=ROC_NFS_vld1$specificities,
             Sensitivity=ROC_NFS_vld1$sensitivities,
             Method='NFS', Data='Validation', Class='S0_2 vs S34',
             AUC=round(ROC_NFS_vld1$auc, 2),
             CI=ROC_NFS_vld1_P95CI)
)

ROC_vld1_df$Method <- factor(ROC_vld1_df$Method, levels=c('LR', 'FIB-4','NFS'))
ROC_vld1_df_text <- unique(ROC_vld1_df[, 3:7])
ROC_vld1_df_text$Text <- paste0(ROC_vld1_df_text$Method, ' (AUROC=',
                                ROC_vld1_df_text$AUC, ' [',ROC_vld1_df_text$CI, '])')
ROC_vld1_df_text$X <- .6
lg<-length(ROC_vld1_df_text$Method)
for (y in 1:lg) {
  if(ROC_vld1_df_text$Method[y]=='LR') {ROC_vld1_df_text$Y[y] <-.17}else{
    if(ROC_vld1_df_text$Method[y]=='FIB-4') {ROC_vld1_df_text$Y[y] <-.1}else{
      if(ROC_vld1_df_text$Method[y]=='NFS') {ROC_vld1_df_text$Y[y] <-.03}}}
}

valid1_ROC_plot<-ggplot(aes(x=1-Specificity, y=Sensitivity, color=Method), data=ROC_vld1_df) + 
  geom_path(size=1) + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dashed') + 
  ggtitle('ROC curves of validation_set1') + 
  scale_color_manual(values=cols)+
  geom_text(data=ROC_vld1_df_text, aes(x=X, y=Y, label=Text), size=4.5, fontface = "bold") + 
  theme_bw() +
  theme(text=element_text(size=16), legend.position='none', 
        plot.title = element_text(hjust = 0.5)) 
valid1_ROC_plot
ggsave(file="valid1_ROC_plot.pdf",plot=valid1_ROC_plot,width=4.8,height=5)

#1.3 validation_set2----
##(1.1) ROC of LR Models in validation_set1----
LR_vld2 <- predict(LR_model, validation_df2, type='link')

ROC_LR_vld2 <- roc(validation_df2$group, LR_vld2)
ROC_LR_vld2_CI<-round(ci(ROC_LR_vld2),2)
ROC_LR_vld2_P95CI<-paste0(ROC_LR_vld2_CI[1]," - ",ROC_LR_vld2_CI[3])

##(1.2) ROC of FIB-4 in validation_set1----
ROC_FIB4_vld2 <- roc(validation_df2$group, validation_df2$FIB4)
ROC_FIB4_vld2_CI<-round(ci(ROC_FIB4_vld2),2)
ROC_FIB4_vld2_P95CI<-paste0(ROC_FIB4_vld2_CI[1]," - ",ROC_FIB4_vld2_CI[3])

##(1.3) ROC of NFS in validation_set1----
ROC_NFS_vld2 <- roc(validation_df2$group, validation_df2$NFS)
ROC_NFS_vld2_CI<-round(ci(ROC_NFS_vld2),2)
ROC_NFS_vld2_P95CI<-paste0(ROC_NFS_vld2_CI[1]," - ",ROC_NFS_vld2_CI[3])

##(2.1) Combination and ROC Plotting in validation_set1----
ROC_vld2_df <- rbind(
  data.frame(Specificity=ROC_LR_vld2$specificities,
             Sensitivity=ROC_LR_vld2$sensitivities,
             Method='LR', Data='Validation', Class='S0_2 vs S34',
             AUC=round(ROC_LR_vld2$auc, 2),
             CI=ROC_LR_vld2_P95CI),
  data.frame(Specificity=ROC_FIB4_vld2$specificities,
             Sensitivity=ROC_FIB4_vld2$sensitivities,
             Method='FIB-4', Data='Validation', Class='S0_2 vs S34',
             AUC=round(ROC_FIB4_vld2$auc, 2),
             CI=ROC_FIB4_vld2_P95CI),
  data.frame(Specificity=ROC_NFS_vld2$specificities,
             Sensitivity=ROC_NFS_vld2$sensitivities,
             Method='NFS', Data='Validation', Class='S0_2 vs S34',
             AUC=round(ROC_NFS_vld2$auc, 2),
             CI=ROC_NFS_vld2_P95CI)
)

ROC_vld2_df$Method <- factor(ROC_vld2_df$Method, levels=c('LR', 'FIB-4','NFS'))
ROC_vld2_df_text <- unique(ROC_vld2_df[, 3:7])
ROC_vld2_df_text$Text <- paste0(ROC_vld2_df_text$Method, ' (AUROC=',
                                ROC_vld2_df_text$AUC, ' [',ROC_vld2_df_text$CI, '])')
ROC_vld2_df_text$X <- .6
lg<-length(ROC_vld2_df_text$Method)
for (y in 1:lg) {
  if(ROC_vld2_df_text$Method[y]=='LR') {ROC_vld2_df_text$Y[y] <-.17}else{
    if(ROC_vld2_df_text$Method[y]=='FIB-4') {ROC_vld2_df_text$Y[y] <-.1}else{
      if(ROC_vld2_df_text$Method[y]=='NFS') {ROC_vld2_df_text$Y[y] <-.03}}}
}

valid2_ROC_plot<-ggplot(aes(x=1-Specificity, y=Sensitivity, color=Method), data=ROC_vld2_df) + 
  geom_path(size=1) + 
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dashed') + 
  ggtitle('ROC curves of validation_set2') + 
  scale_color_manual(values=cols)+
  geom_text(data=ROC_vld2_df_text, aes(x=X, y=Y, label=Text), size=4.5, fontface = "bold") + 
  theme_bw() +
  theme(text=element_text(size=16), legend.position='none', 
        plot.title = element_text(hjust = 0.5)) 
valid2_ROC_plot
ggsave(file="valid2_ROC_plot.pdf",plot=valid2_ROC_plot,width=4.8,height=5)

#3. box plot----
all_data_group <- rbind(
  data.frame(LR=round(LR_train,2), FIB4=discovery_df$FIB4, NFS=discovery_df$NFS,  
             Group=discovery_df$group, Data='discovery_set', 
             Class='S0_2 vs S34'), 
  data.frame(LR=round(LR_vld1,2), FIB4=validation_df1$FIB4, NFS=validation_df1$NFS, 
             Group=validation_df1$group, Data='validation_set1', 
             Class='S0_2 vs S34'), 
  data.frame(LR=round(LR_vld2,2), FIB4=validation_df2$FIB4, NFS=validation_df2$NFS, 
             Group=validation_df2$group, Data='validation_set2', 
             Class='S0_2 vs S34') 
)

all_data_melt <- melt(all_data_group, id.vars = c('Group', 'Data', 'Class'))

all_data_melt$Group <- paste0(all_data_melt$variable, 
                              ' (', all_data_melt$Group, ')')
Grp<-unique(all_data_melt$Group)
all_data_melt$Group<-factor(all_data_melt$Group,levels = Grp)

train_box_plot<-ggplot(all_data_melt%>% filter(Data=='discovery_set'), aes(x = Group, y = value)) + 
  geom_boxplot(aes(fill=variable)) + 
  facet_grid(.~Data) + theme_bw() + 
  geom_signif(aes(x = Group, y = as.numeric(value)),test = "wilcox.test", tip_length=5, 
              y_position=c(8.5, 8, 6),na.rm = TRUE,
              comparisons=list(c('LR (S0_2)', 'LR (S34)'),
                               c('FIB4 (S0_2)', 'FIB4 (S34)'),c('NFS (S0_2)', 'NFS (S34)'))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        text = element_text(size=12))+ 
  ylim(-10, 10) + scale_fill_manual(values=c("#9900CC", "#999999", "#FFCC00"))+
  xlab(NULL) + ylab(NULL) 
train_box_plot
ggsave(file="train_box_plot.pdf",plot=train_box_plot,width=4.8,height=5)


valid1_box_plot<-ggplot(all_data_melt%>% filter(Data=='validation_set1'), aes(x = Group, y = value)) + 
  geom_boxplot(aes(fill=variable)) + 
  facet_grid(.~Data) + theme_bw() + 
  geom_signif(aes(x = Group, y = as.numeric(value)),test = "wilcox.test", tip_length=5, 
              y_position=c(6, 7.5, 3.5),na.rm = TRUE,
              comparisons=list(c('LR (S0_2)', 'LR (S34)'),
                               c('FIB4 (S0_2)', 'FIB4 (S34)'),c('NFS (S0_2)', 'NFS (S34)'))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        text = element_text(size=12)) + 
  ylim(-10, 10) + scale_fill_manual(values=c("#9900CC", "#999999", "#FFCC00"))+
  xlab(NULL) + ylab(NULL) 
valid1_box_plot
ggsave(file="valid1_box_plot.pdf",plot=valid1_box_plot,width=4.8,height=5)


valid2_box_plot<-ggplot(all_data_melt%>% filter(Data=='validation_set2'), aes(x = Group, y = value)) + 
  geom_boxplot(aes(fill=variable)) + 
  facet_grid(.~Data) + theme_bw() + 
  geom_signif(aes(x = Group, y = as.numeric(value)),test = "wilcox.test", tip_length=5, 
              y_position=c(5.5, 6.5, 4.5),na.rm = TRUE,
              comparisons=list(c('LR (S0_2)', 'LR (S34)'),
                               c('FIB4 (S0_2)', 'FIB4 (S34)'),c('NFS (S0_2)', 'NFS (S34)'))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        text = element_text(size=12)) + 
  ylim(-10, 10) + scale_fill_manual(values=c("#9900CC", "#999999", "#FFCC00"))+
  xlab(NULL) + ylab(NULL) 
valid2_box_plot
ggsave(file="valid2_box_plot.pdf",plot=valid2_box_plot,width=4.8,height=5)
