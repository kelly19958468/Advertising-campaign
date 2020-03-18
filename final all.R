
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


