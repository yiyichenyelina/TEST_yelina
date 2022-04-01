#1
df<-data.frame(temp=c(85	,90,	76,	91,	84,	94,	88,	85,	97,86,	82,	78,	77,83),
  electricity=c(22.5, 23.7, 20.3, 23.4, 24.2, 23.5, 22.9, 22.4, 26.1, 23.1, 22.5, 20.9, 21.0, 22.6))
temp=df$temp
elec=df$elec
#a
plot(temp,elec)
#b
lm1<-lm(elec~temp)
abline(lm1)
summary(lm1)
#c
plot(fitted(lm1), residuals(lm1), xlab= "Fitted values", ylab = "Residuals")
abline(h=0)
cor(temp,elec)
summary(lm1)
qqnorm(scale(residuals(lm1)))
abline(0,1)
#d
predict(lm1, newdata = data.frame(temp = 93))
#e
summary(lm1)

#2
df2<-data.frame(
father<-c(60,62,64, 65,66, 67,68, 70,  72, 74),
son<-c(63.6,65.2,66,65.5,66.9,67.1,67.4,68.3, 70.1,70)
)
lm2<-lm(son~father, data=df2)
summary(lm2)
plot(father, son,data=df2)
abline(lm2)
upperbound.onesided=0.46457+qt(0.99,df=9)*0.03298

#5
#a
2*pt(3.4408,df=62,lower=FALSE)
2*pt(3.646,df=62,lower=FALSE)
2*pt(3.5502,df=62,lower=FALSE)
2*pt(1.675,df=62,lower=FALSE)
1-pf(5.4798,df1=3,df2 = 62)
1-pt(3.5502,62)

#6
#a
data(iris)
lm.iris=lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris)
summary(lm.iris)
#b
lm.0=lm(Sepal.Length~Petal.Width,data=iris)
anova(lm.0,lm.iris)
#c
install.packages("car")
library ("car")
linearHypothesis(lm.iris,"Sepal.Width=Petal.Length")

lm.wo.sw=lm(Sepal.Length~Petal.Length+Petal.Width,data=iris)
lm.wo.pl=lm(Sepal.Length~Sepal.Width+Petal.Width,data=iris)
anova(lm.wo.sw,lm.iris)
anova(lm.wo.pl,lm.iris)

#7
#a
cig <- read.table("/Users/yiyichen/Downloads/P088.txt",header=TRUE, sep="")
lm.full=lm(Sales~Age+HS+Income+Black+Female+Price,data=cig)
lm.wofemale=lm(Sales~Age+HS+Income+Black+Price,data=cig)
anova(lm.wofemale,lm.full)
#b
lm.wfemale=lm(Sales~Age+HS+Income+Black+Female+Price,data=cig)
lm.reduced=lm(Sales~Age+Income+Black+Price,data=cig)
anova(lm.reduced,lm.full)
#c
anova(lm.full)
summary(lm.full)
confint(lm.full,Income,level=0.95)
upperbound=0.01895+0.01022*1.855
lowerbound=0.01895-0.01022*1.855
#d
lm.wo.income=lm(Sales~Age+HS+Black+Female+Price,data=cig)
summary((lm.wo.income))
