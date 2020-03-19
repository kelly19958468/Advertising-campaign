install.packages("sem")
library(sem)
install.packages("lavaan")
library(lavaan)
setwd("/Users/caiyunan/Desktop/03350331/temp")
updata <- read.table("./updata.csv", header = TRUE, sep = ",")

# Multiple Linear Regression 複迴歸

Regression <- lm(d ~ a + b + c ,data=updata)

#d1 
Regression1 <- lm(D1 ~ A1 + A2 + A3 + A4 ,data=updata)
# Show the results
summary(Regression1)

#d2 
Regression2 <- lm(D2 ~ A1 + A2 + A3 + A4 ,data=updata)
# Show the results
summary(Regression2)


#d3 
Regression3 <- lm(D3 ~ A1 + A2 + A3 + A4 ,data=updata)
# Show the results
summary(Regression3)

#d4 
Regression4 <- lm(D4 ~ B + C + D1 + D2 + D3 ,data=updata)
# Show the results
summary(Regression4)

#逐步迴歸
model1<- lm(d ~ a + b + c ,data=data)

model2 <- step(model1)

summary(model2)


install.packages("GPArotation")
install.packages("corpcor")
install.packages("psych")
install.packages("rela")
install.packages("parallel")
install.packages("MASS")

library(GPArotation)
library(corpcor)
library(psych)
library(rela)
library(parallel)
library(MASS)



#信度分析

data1 <- read.table("./data.csv", header = TRUE, sep = ",")

#整體信度
All <- data[,2:50] 
All_=as.matrix(All)
All.item <- itemanal(All_)
summary(All.item)

#可靠性(a1~4)
A1 <- data[,2:5] 
A1_=as.matrix(A1)
A1.item <- itemanal(A1_)
summary(A1.item)

#專業性(a5~8)
A2 <- data[,6:9]
A2_=as.matrix(A2)
A2.item <- itemanal(A2_)
summary(A2.item)

#吸引力(a9~13)
A3 <- data[,10:14]
A3_=as.matrix(A3)
A3.item <- itemanal(A3_)
summary(A3.item)

#適配性(a14~18)
A4 <- data[,15:19]
A4_=as.matrix(A4)
A4.item <- itemanal(A4_)
summary(A4.item)

#個人因素(b1~8)
B <- data[,20:27]
B_=as.matrix(B)
B.item <- itemanal(B_)
summary(B.item)

#道德強度(c1~6)
C <- data[,28:33]
C_=as.matrix(C)
C.item <- itemanal(C_)
summary(C.item)

#廣告記憶(d1~4)
D1 <- data[,34:37]
D1_=as.matrix(D1)
D1.item <- itemanal(D1_)
summary(D1.item)

#廣告態度(d5~9)
D2 <- data[,38:42]
D2_=as.matrix(D2)
D2.item <- itemanal(D2_)
summary(D2.item)

#品牌態度(d10~13)
D3 <- data[,43:46]
D3_=as.matrix(D3)
D3.item <- itemanal(D3_)
summary(D3.item)

#購買意願(d14~17)
D4 <- data[,47:50]
D4_=as.matrix(D4)
D4.item <- itemanal(D4_)
summary(D4.item)




#探索性因素分析EFA


v <- list( )
v$items <- c("id", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12","a13","a14","a15","a16","a17","a18","b1","b2", "b3", "b4", "b5", "b6", "b7", "b8", "c1", "c2" ,"c3","c4","c5","c6","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","d12","d13","d14","d15","d16","d17","e1","e2","e3","e4","e5","e6","e7","e8","e9","e10")
head(data[,v$items])

#憛怎????賊??敶????
data1$uniquecount <- apply(data1[,v$items], 1, function(x) length(unique(x)))

table(data1$uniquecount)
#截
psych::describe(data1[,v$items])
sapply(data1[,v$items],table)

#探索性分析

#因素分析所有李克特等級項目
factanal(data1[,2:50],10)
#scree plot
scree(data1[,v$items])
factanal(data[,2:50],10)
#平行分析
plot(data)
psych::fa.parallel(data[,2:50])


#因素命名取因素負荷量>.4
fita <- factanal(data1[,2:19], 4)
print(fita, cutoff = .4)

#相關係數矩陣到小數第3位
corfacta = cor(data1[,2:19])
round(corfacta, 3)
#應用rela套件執行因素分析
library(rela)
str(data1)
data1=as.matrix(data1)
paf.corfact = paf(data1[,2:19], eigcrit=1, convcrit=.001)
summary(paf.corfacta)
#執行Cronbach’s α的R程式
itemanal(corfacta)


#b
#因素命名取因素負荷量>.4
fitb <- factanal(data1[,20:27], 3)
print(fitb, cutoff = .4)
#相關係數矩陣到小數第3位
corfactb = cor(data1[,20:27])
round(corfactb, 3)
#應用rela套件執行因素分析
library(rela)
str(data1)
data1=as.matrix(data1)
paf.corfactb = paf(data1[,20:27], eigcrit=1, convcrit=.001)
summary(paf.corfactb)
#執行Cronbach’s α的R程式
itemanal(corfactb)

#c
#因素命名取因素負荷量>.4
fitc <- factanal(data1[,28:33], 1)
print(fitc, cutoff = .4)
#相關係數矩陣到小數第3位
corfactc = cor(data1[,28:33])
round(corfactc, 3)
#應用rela套件執行因素分析
library(rela)
str(data1)
data1=as.matrix(data1)
paf.corfactc = paf(data1[,28:33], eigcrit=1, convcrit=.001)
summary(paf.corfactc)
#執行Cronbach’s α的R程式
itemanal(corfactc)

#d
#因素命名取因素負荷量>.4
fitd <- factanal(data1[,34:50], 4)
print(fitd, cutoff = .4)
#相關係數矩陣到小數第3位
corfactd = cor(data1[,34:50])
round(corfactd, 3)
#應用rela套件執行因素分析
library(rela)
str(data1)
data1=as.matrix(data1)
paf.corfactd = paf(data1[,34:50], eigcrit=1, convcrit=.001)
summary(paf.corfactd)
#執行Cronbach’s α的R程式
itemanal(corfactd)



#一階
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
cfa.model = 'A=~ aa+ab+ac+ad
B=~ b1+b2+b3+b4+b5+b6+b7+b8
C=~ c1+c2+c3+c4+c5+c6
D=~ da+db+dc+dd
'
fit <- cfa(cfa.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)



#二階
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
cfa.model = 'A=~ aa+ab+ac+ad
B=~ b1+b2+b3+b4+b5+b6+b7+b8
C=~ c1+c2+c3+c4+c5+c6
D=~ da+db+dc+dd
g=~ A+B+C+D'
fit <- cfa(cfa.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)



#sem
#sem圖
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "aa=~a1+a2+a3+a4
ab=~a5+a6+a7+a8
ac=~a9+a10+a11+a12+a13
ad=~a14+a15+a16+a17+a18
A=~ aa+ab+ac+ad
B=~ b1+b2+b3+b4+b5+b6+b7+b8
C=~ c1+c2+c3+c4+c5+c6
dd=~d14+d15+d16+d17
da=~d1+d2+d3+d4
db=~d5+d6+d7+d8+d9
dc=~d10+d11+d12+d13
dd=~A+B+C+da+db+dc

"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)





####
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "A=~ aa+ab+ac+ad
B=~ b1+b2+b3+b4+b5+b6+b7+b8
C=~ c1+c2+c3+c4+c5+c6
dd=~d14+d15+d16+d17
A + B+ C=~ dd
"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)


sem.model1 = "da=~d1+d2+d3+d4
db=~d5+d6+d7+d8+d9
dc=~d10+d11+d12+d13
dd=~d14+d15+d16+d17
da+db+dc=~ dd

"
fit1 <- sem(sem.model1, data= data)
semPaths(fit1, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)


#可靠性sem圖
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "aa=~a1+a2+a3+a4
da=~d1+d2+d3+d4
db=~d5+d6+d7+d8+d9
dc=~d10+d11+d12+d13
aa=~ da + db+ dc
"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)


#專業性sem圖
data=read.csv("D:/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "ab=~a5+a6+a7+a8
da=~d1+d2+d3+d4
db=~d5+d6+d7+d8+d9
dc=~d10+d11+d12+d13
ab=~ da + db+ dc
"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)

#吸引力sem圖
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "ac=~a9+a10+a11+a12+a13
da=~d1+d2+d3+d4
db=~d5+d6+d7+d8+d9
dc=~d10+d11+d12+d13
ac=~ da + db+ dc
"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)

#人格特質sem圖
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "ad=~a14+a15+a16+a17+a18
da=~d1+d2+d3+d4
db=~d5+d6+d7+d8+d9
dc=~d10+d11+d12+d13
ad=~ da + db+ dc
"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)


#購買意願sem圖
data=read.csv("/Users/caiyunan/Desktop/03350331/temp/datacfa.csv")
library(lavaan)
sem.model = "B=~ b1+b2+b3+b4+b5+b6+b7+b8
C=~ c1+c2+c3+c4+c5+c6

dd=~d14+d15+d16+d17
dd~B+C
"

fit <- sem(sem.model, data= data)
summary(fit, fit.measures = TRUE, standardized=TRUE, rsquare=TRUE)
lavaan:::print.fit.measures(fitMeasures(fit))
library(semPlot)
semPaths(fit, layout="tree2", whatLabels="std", style="lisrel", edge.color=c("blue"), color=c("greenyellow"), nDigits=3)


