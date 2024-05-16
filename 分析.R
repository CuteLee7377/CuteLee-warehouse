library(haven)
library(plyr)
library(dplyr)
library(arsenal)
library(survey)
demo.j <- read_xpt("/Users/lisirui/Desktop/P_DEMO.XPT")
colnames(demo.j)
demo.data <- demo.j[,c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH3', 'DMDEDUC2')]
View(demo.data)
bmx.j <- read_xpt("/Users/lisirui/Desktop/P_BMI.XPT")
bmx.data.file <- dplyr::bind_rows(list(bmx.j))
bmx.data <- bmx.data.file[,c('SEQN', 'BMXBMI')]##BMI
View(bmx.data)
ALTAST.j <- read_xpt("/Users/lisirui/Desktop/P_ALTAST.XPT")
colnames(ALTAST.j)
ALTAST.data <- ALTAST.j[,c('SEQN', 'LBXSATSI', 'LBXSASSI', 'LBDSTRSI')]##ALT,AST,TG
View(ALTAST.data)
FPG.j <- read_xpt("/Users/lisirui/Desktop/P_GLU.XPT")
FPG.data.file <- dplyr::bind_rows(list(FPG.j))
FPG.data <- FPG.data.file[,c('SEQN', 'LBDGLUSI')]##TG
View(FPG.data)
CAP.j <- read_xpt("/Users/lisirui/Desktop/P_CAP.XPT")
colnames(CAP.j)
CAP.data <- CAP.j[,c('SEQN',"LUXCAPM","LUXSMED","LUAXSTAT")]##弹性成像
HBVS.j <- read_xpt("/Users/lisirui/Desktop/P_HBV_S.XPT")
HBVS.data <- HBVS.j[,c('SEQN',"LBXHBS")]##HBV
HCV.j <- read_xpt("/Users/lisirui/Desktop/P_HCV.XPT")
HCV.data <- HCV.j[,c('SEQN',"LBXHCR","LBDHCI")]##HCV
liverdisease.j <- read_xpt("/Users/lisirui/Desktop/P_MCQ.XPT")
colnames(liverdisease.j)##肝癌及自身免疫性肝病
liverdisease.data <- liverdisease.j[,c('SEQN',"MCQ510E", "MCQ230A", "MCQ230B", "MCQ230C")]
dr1.j <- read_xpt("/Users/lisirui/Desktop/P_DR1TOT.XPT")
dr1.data <- dr1.j[,c('SEQN',"DR1TALCO")]##第一天饮酒量
dr2.j <- read_xpt("/Users/lisirui/Desktop/P_DR2TOT.XPT")
dr2.data <- dr2.j[,c('SEQN',"DR2TALCO")]##第二天饮酒量
alco.data <- merge(dr1.data, dr2.data)
dim(alco.data)
View(alco.data)
output <- plyr::join_all(list(demo.data, bmx.data, ALTAST.data,FPG.data,CAP.data,HBVS.data,
                              HCV.data,liverdisease.data,alco.data), by='SEQN', type='left')
dim(output)##15560
View(output)
###原始数据提取合并完成##15560
##排除重要变量缺失的：BMI,ALT, AST,TG,FPG
table(output$BMXBMI)##BMI
table(output$LBXSATSI)##ALT
table(output$LBXSASSI)##AST
table(output$LBDSTRSI)##TG
table(output$LBDGLUSI)##FPG
table(output$LUAXSTAT)##CAP
length(output)
BMI.exclude.index <- which(is.na(output$BMXBMI))
length(BMI.exclude.index)
data.BMI.exist <- output[-BMI.exclude.index, ]
dim(data.BMI.exist)##13137
ALT.exclude.index <- which(is.na(data.BMI.exist$LBXSATSI))
length(ALT.exclude.index)
data.ALT.exist <- data.BMI.exist[-ALT.exclude.index, ]
dim(data.ALT.exist)##9322
AST.exclude.index <- which(is.na(data.ALT.exist$LBXSASSI))
length(AST.exclude.index)
data.AST.exist <- data.ALT.exist[-AST.exclude.index, ]
dim(data.AST.exist)##9285
TG.exclude.index <- which(is.na(data.AST.exist$LBDSTRSI))
length(TG.exclude.index)
FPG.exclude.index <- which(is.na(data.AST.exist$LBDGLUSI))
length(FPG.exclude.index)
data.FPG.exist <- data.AST.exist[-FPG.exclude.index, ]
dim(data.FPG.exist)##4531
View(data.FPG.exist)
table(data.FPG.exist$LUAXSTAT)##弹性成像检查状态
##   1完成   2部分    3不符合条件    4 未完成
####  4077    249        154            51
cap.exclude.index <- which(data.FPG.exist$LUAXSTAT != 1 ## 弹性成像检查状态-LUAXSTAT
                           | is.na(data.FPG.exist$LUXCAPM)  #可控衰减参数中位数-LUXCAPM
                           | is.na(data.FPG.exist$LUXSMED)  # 中位硬度-LUXSMED
                           | data.FPG.exist$LUXCAPM <238)##无脂肪肝的
length(cap.exclude.index)
cap.exist.index <- data.FPG.exist[-cap.exclude.index,]
dim(cap.exist.index)#4077
livercancer.index <- which(cap.exist.index$MCQ230A == 22|cap.exist.index$MCQ230B == 22|
                             cap.exist.index$MCQ230C == 22)
length(livercancer.index)
lc.exist.index <- cap.exist.index[-livercancer.index,]
dim(lc.exist.index)##4075除去肝癌
autoimmh.index <- which(lc.exist.index$MCQ510E == 5)
autoimmh.exist.index <- lc.exist.index[-autoimmh.index,]
dim(autoimmh.exist.index)##4064除去自免肝
HBV.index <- which(autoimmh.exist.index$LBXHBS == 1)##HBV
length(HBV.index)
HBVNA.index<- autoimmh.exist.index[-HBV.index,]
dim(HBVNA.index)##2971除去HBV
HCV.index<- which(HBVNA.index$LBXHCR == 1|HBVNA.index$LBDHCI == 1)
healthl.index<- HBVNA.index[-HCV.index,]
dim(healthl.index)##2927除去HCV
total.alco <- apply(healthl.index[,c('DR1TALCO', 'DR2TALCO')], 1, mean)##计算过量饮酒
healthl.index$total.alco <- total.alco
day.2.na.index <- which(is.na(healthl.index$DR2TALCO))
healthl.index$total.alco[day.2.na.index] <- healthl.index$DR1TALCO[day.2.na.index]
View(healthl.index[day.2.na.index,c('DR1TALCO', 'DR2TALCO', 'total.alco')])
excessive.alco.male <- ifelse(healthl.index$RIAGENDR == 1 & healthl.index$total.alco > 20, 1, 0)
excessive.alco.female <- ifelse(healthl.index$RIAGENDR == 2 & healthl.index$total.alco > 10, 1, 0)
table(excessive.alco.female)
excessive.alco.index <- which(excessive.alco.male == 1 | excessive.alco.female == 1, 1, 0)
table(excessive.alco.index)##过量饮酒人数
finalpeople.index<- healthl.index[-excessive.alco.index,]
dim(finalpeople.index)##1585
lost1.index <- which(is.na(finalpeople.index$RIDAGEYR))
length(lost1.index)
lost2.index<- which(is.na(finalpeople.index$RIAGENDR))
length(lost2.index)
lost3.index<- which(is.na(finalpeople.index$RIDRETH3))
length(lost3.index)
lost4.index<- which(is.na(finalpeople.index$DMDEDUC2))
length(lost4.index)
exist1.index<- finalpeople.index[-lost4.index,]
dim(exist1.index)##1414除去教育程度缺失
##接下来，计算ZJU=BMI+FPG+TG+3*(ALT/AST)(+2，如果是女性）<32,排除NAFLD；>38，确诊
##CAP>238,脂肪肝
exist1.index$data1<-3*exist1.index$LBXSATSI/exist1.index$LBXSASSI
dim(exist1.index)
exist1.index$ZJU<-ifelse(exist1.index$RIAGENDR==1,exist1.index$data1+exist1.index$BMXBMI+exist1.index$LBDSTRSI+exist1.index$LBDGLUSI,exist1.index$data1+exist1.index$BMXBMI+exist1.index$LBDSTRSI+exist1.index$LBDGLUSI+2)
dim(exist1.index)
View(exist1.index)#1414
save(exist1.index, file = "paperdata.RData")##脂肪肝人群的基础数据
exist1.index$postive<-ifelse(exist1.index$ZJU>38,1,0)##1=NAFLD且ZJU>38阳性人群,0=脂肪肝人群但ZJU<=38
table(exist1.index$postive)##1=1144,0=270
library(pROC)
head(exist1.index)
plot.roc(exist1.index$LUXCAPM,
         exist1.index$ZJU,
         col="#A31621",
         percent=TRUE,
         lwd=2,
         print.auc=TRUE,
         print.auc.cex=1,
         print.auc.pattern="ZJU:%.1f%%",
         print.auc.y=50)
