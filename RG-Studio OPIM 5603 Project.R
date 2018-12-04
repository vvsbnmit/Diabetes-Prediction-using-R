#OPIM 5603 Statistics in Business Analytics RG-Studio Project
#Created file RG-Studio OPIM 5603 Project
#Objective: Statistical analysis of the diagnostic measures of female patients and their impact on the outcome
#Outcome is likelihood of having Type II diabetes
library(MASS)
#Import file diabetes dataset that is pre-processed and missing values addressed using KNN clustering 
DiabetesP <- read_csv("File Destination/DiabetesP.csv")

#Data Exploration 

summary(DiabetesP)
hist(DiabetesP$Pregnancies, prob=F)
lines(density(DiabetesP$Pregnancies), col="green")


opar=par()                          #graphical parameters are set with the par() command prior to drawing the graph
par(bg="white",mfrow=c(3,3),las=2,col="red")  #mfrow to split the plotting region into 3 rows and 3 columns
plot(DiabetesP$Outcome, DiabetesP$Pregnancies, xlab="outcome",ylab="Pregnancies")
plot(DiabetesP$Outcome, DiabetesP$Glucose, xlab="outcome",ylab="Glucose")
plot(DiabetesP$Outcome, DiabetesP$BloodPressure, xlab="outcome",ylab="Blood Pressure")
plot(DiabetesP$Outcome, DiabetesP$SkinThickness, xlab="outcome",ylab="Skin Thickness")
plot(DiabetesP$Outcome, DiabetesP$Insulin, xlab="outcome",ylab="Insulin")
plot(DiabetesP$Outcome, DiabetesP$BMI, xlab="outcome",ylab="BMI")
plot(DiabetesP$Outcome, DiabetesP$DiabetesPedigreeFunction, xlab="outcome",ylab="Diabetes Pedigree function")
plot(DiabetesP$Outcome, DiabetesP$Agexlab="outcome",ylab="Age")
par(opar)


#Statistical Testing

#1. Non Parametric test to check which variable has more impact on the outcome. We chose modifiable risk factors which are Glucose, BP, Insulin, BMI

#1.b  Hypothesis that IQR of GLucose is same for both outcome values
#Subset outcome 1 rows and outcome 0 rows 
Outcome1 = DiabetesP[DiabetesP$Outcome==1,]
Outcome1
Outcome0 = DiabetesP[DiabetesP$Outcome==0,]
Outcome0

nrow(Outcome1)
nrow(Outcome0)

IQR(Outcome1$Glucose)
IQR(Outcome0$Glucose)


g = DiabetesP$GLucose
sample(g)

#function to check the difference in IQR in values for Outcome 1 and outcome 0 
f1 = function()
{
  x = sample(g)
  z = abs(IQR(x[1:268])-IQR(x[269:768]))
  return(z)
}
# replicate to create the sampling distribution
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="green")
tstat = abs(IQR(Outcome1$Glucose) - IQR(Outcome0$Glucose))
tstat
abline(v=tstat)
rside = dist[dist>tstat]
pvalue = length(rside)/length(dist)
pvalue



#1.c  Hypothesis that IQR of Blood Pressure is same for both outcome values
Outcome1 = DiabetesP[DiabetesP$Outcome==1,]
Outcome1
Outcome0 = DiabetesP[DiabetesP$Outcome==0,]
Outcome0

nrow(Outcome1)
nrow(Outcome0)

IQR(Outcome1$BloodPressure)
IQR(Outcome0$BloodPressure)


g = DiabetesP$BloodPressure
sample(g)

#function to check the difference in IQR in values for Outcome 1 and outcome 0 
f1 = function()
{
  x = sample(g)
  z = abs(IQR(x[1:268])-IQR(x[269:768]))
  return(z)
}
# replicate to create the sampling distribution
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="green")
tstat = abs(IQR(Outcome1$BloodPressure) - IQR(Outcome0$BloodPressure))
tstat
abline(v=tstat)
rside = dist[dist>tstat]
pvalue = length(rside)/length(dist)
pvalue



#1.d Hypothesis that IQR of Insulin is same for both outcome values
Outcome1 = DiabetesP[DiabetesP$Outcome==1,]
Outcome1
Outcome0 = DiabetesP[DiabetesP$Outcome==0,]
Outcome0

nrow(Outcome1)
nrow(Outcome0)

IQR(Outcome1$Insulin)
IQR(Outcome0$Insulin)


g = DiabetesP$Insulin
sample(g)

#function to check the difference in IQR in values for Outcome 1 and outcome 0 
f1 = function()
{
  x = sample(g)
  z = abs(IQR(x[1:268])-IQR(x[269:768]))
  return(z)
}
# replicate to create the sampling distribution
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="green")
tstat = abs(IQR(Outcome1$Insulin) - IQR(Outcome0$Insulin))
tstat
abline(v=tstat)
rside = dist[dist>tstat]
pvalue = length(rside)/length(dist)
pvalue


#########################################################################################################

#2. Correlation among diagnostic measurements
install.packages("corrgram")
library(corrgram)
#To produce graphical display of a correlation matrix, called a correlogram.
corrgram(DiabetesP, order=T, upper.panel=panel.pie) #order = true for variables to be reordered. upper.panel to use separate panel function above and below the diagonal.

#######################################################################################################


#3. Parametric test to check the hypothesis that there is no correlation between BMI and SKinThickness
BMI = DiabetesP$BMI
SkinThickness = DiabetesP$SkinThickness
n = length(DiabetesP)

f1 = function()
{
  s1 = rnorm(n,mean = mean(BMI),sd = sd(BMI))
  s2 = rnorm(n,mean = mean(SkinThickness),sd=sd(SkinThickness))
  return(cor(s1,s2))
}
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="yellow")

tstat = cor(BMI,SkinThickness)
tstat

abline(v=tstat)
# computing the p-value
gap = abs(mean(dist)-tstat)
lside = dist[dist<mean(dist)-gap]
rside = dist[dist>mean(dist)+gap]
pvalue = (length(lside)+length(rside))/length(dist)
pvalue

cor.test(BMI, SkinThickness)
cor(DiabetesP$BMI, DiabetesP$SkinThickness) #Correlation coefficient also known as Pearson product moment correlation coefficient

#pvalue is 0.02 which shows that there is no evidence to support the claim. Therefore reject the hypothesis
#That is, if BMI is high, skin thickness will also be high for the patient 


################################################################################################################


#4. Visualizing the correlation between BMI and Skin thickness using ggpubr package
install.packages("ggpubr")
library(ggpubr)
#to create a scatter plot between BMI and Skin Thickness. correlation method is Pearson.
#Added regression line; confidence intervals;and also correlation coefficient
ggscatter(DiabetesP, x="BMI", y="SkinThickness", cor.method="pearson", color="orange", add="reg.line",add.params=list(color="blue",fill="lightgray"),conf.int=TRUE,cor.coef=TRUE)

help("ggscatter")

#################################################################################################################
#Interaction Terms
#5. Hypothesis is that the interaction term Glucose*Insulin has no impact on outcome 
Outcome1 = DiabetesP[DiabetesP$Outcome==1,]
Outcome1
Outcome0 = DiabetesP[DiabetesP$Outcome==0,]
Outcome0
nrow(Outcome1)
nrow(Outcome0)
IQR(Outcome1$Glucose*Outcome1$Insulin)
IQR(Outcome0$Glucose*Outcome0$Insulin)
g = DiabetesP$Glucose*DiabetesP$Insulin
sample(g)
#function to check the difference in IQR in values for Outcome 1 and outcome 0 
f1 = function()
{
  x = sample(g)
  z = abs(IQR(x[1:268])-IQR(x[269:768]))
  return(z)
}
# replicate to create the sampling distribution
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="green")
tstat = abs(IQR(Outcome1$Glucose*Outcome1$Insulin) - IQR(Outcome0$Glucose*Outcome0$Insulin))
tstat
abline(v=tstat)
rside = dist[dist>tstat]
pvalue = length(rside)/length(dist)
pvalue


#5.1 Hypothesis is that the interaction term BMI * Diabetes pedigree function has no impact on outcome 

Outcome1 = DiabetesP[DiabetesP$Outcome==1,]
Outcome1
Outcome0 = DiabetesP[DiabetesP$Outcome==0,]
Outcome0

nrow(Outcome1)
nrow(Outcome0)

IQR(Outcome1$BMI*Outcome1$DiabetesPedigreeFunction)
IQR(Outcome0$BMI*Outcome0$DiabetesPedigreeFunction)


g = DiabetesP$BMI*DiabetesP$DiabetesPedigreeFunction
sample(g)

#function to check the difference in IQR in values for Outcome 1 and outcome 0 
f1 = function()
{
  x = sample(g)
  z = abs(IQR(x[1:268])-IQR(x[269:768]))
  return(z)
}
# replicate to create the sampling distribution
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="green")
tstat = abs(IQR(Outcome1$Glucose*Outcome1$Insulin) - IQR(Outcome0$Glucose*Outcome0$Insulin))
tstat
abline(v=tstat)
rside = dist[dist>tstat]
pvalue = length(rside)/length(dist)
pvalue

###########################################################################################################################################

#6. Hypothesis that 35% of the female patients are more likely to have Type II diabetes 

v = c(0,1)
p = c(0.65,0.35)

f1 = function()
{
  s = sample(x =v,replace = T,prob = p,size = n )
  return(prop.table(table(s))[2])
}
dist = replicate(10000,f1())
plot(density(dist))
polygon(density(dist),col="yellow")

tstat = prop.table(table(DiabetesP$Outcome))[2]
abline(v=tstat)
# computing the p-value
gap = abs(mean(dist)-tstat)
lside = dist[dist<mean(dist)-gap]
rside = dist[dist>mean(dist)+gap]
pvalue = (length(lside)+length(rside))/length(dist)
pvalue
#pvalue is 0.93 which shows that there is strong evidence to support the claim. Therefore, I fail to reject the hypothesis

#################################################################################################################################

#7. Visualization using ggplot package

#7.1 

install.packages("ggplot2")
library(ggplot2)

ggplot(DiabetesP,aes(x = log(DiabetesP$BloodPressure), y = log(DiabetesP$Glucose)))+geom_point()+ 
  facet_grid(. ~ DiabetesP$Outcome)+
  stat_smooth(method = "lm", col = "red")+
  coord_equal()
help("ggplot")


qplot(log(DiabetesP$Glucose),log(DiabetesP$BloodPressure),data = DiabetesP, color = DiabetesP$BMI)


ggplot(DiabetesP, aes(x = DiabetesP$Outcome, y = DiabetesP$Glucose))+
  geom_point()+
  geom_boxplot()

boxplot(data = DiabetesP, DiabetesP$Glucose~DiabetesP$Outcome)


DiabetesP$Outcome <- as.factor(DiabetesP$Outcome)
head(DiabetesP)

########################################################################################################

#7.2 

attach(DiabetesP)

ggplot(DiabetesP, aes(x = Outcome , y = Glucose))+
  geom_point()+
  geom_boxplot()

ggplot(DiabetesP, aes(x = Outcome , y = Pregnancies))+
  geom_point()+
  geom_boxplot()

ggplot(DiabetesP, aes(x = Outcome , y = BloodPressure))+
  geom_point()+
  geom_boxplot()

ggplot(DiabetesP, aes(x = Outcome , y = Insulin))+
  geom_point()+
  geom_boxplot()

ggplot(DiabetesP, aes(x = Outcome , y = BMI))+
  geom_point()+
  geom_boxplot()

####################################################################################################

#7.3 

View(DiabetesP)

ggplot(DiabetesP, aes(x = Glucose , y = Insulin, color = Outcome))+
  geom_point()

ggplot(DiabetesP, aes(x = SkinThickness , y = BMI, color = Outcome))+
  geom_point()

####################################################################################################

#7.4 

install.packages("rgl")
library(rgl)
require(rgl)

with(DiabetesP,plot3d(DiabetesP$Glucose,DiabetesP$Glucose,DiabetesP$Insulin, type = "s", col = as.integer(DiabetesP$Outcome)))

#########################################################################################################
#########################################################################################################

######Parametric One sample test##############

#8. Hypothesis - Population Mean BMI  = 31

n = nrow(DiabetesP)
BMI = DiabetesP$BMI
# Simulate one sample and compute the metric
f1 = function()
{
  s = rnorm(n,mean = 31,sd(BMI))
  return(mean(s))
}

# replicate to create the sampling distribution
dist = replicate(100000,f1())
plot(density(dist))
polygon(density(dist),col="green")

# compute the test statistic from the actual sample
tstat = mean(BMI)
tstat
abline(v=tstat)  

# computing the p-value
gap = abs(mean(dist)-tstat)
gap
lside = dist[dist<mean(dist)-gap]
rside = dist[dist>mean(dist)+gap]
lside
pvalue = (length(lside)+length(rside))/length(dist)
pvalue


#9. Non Parametric test: the BMI value of the diabetic female population is greater than 30.
n = nrow(DiabetesP) 
n
tstat = sum(ifelse(DiabetesP$BMI>32,1,0))
f1 = function()
{
  v = c(1,0)
  p = c(0.5,0.5)
  x = sample(x = v,replace = T,prob = p,size = n)
  return(sum(x))
}
sdist = replicate(10000,f1()) 
plot(density(sdist)) 
polygon(density(sdist),col="green")
gap = abs(mean(sdist)-tstat) 
gap
abline(v=mean(sdist)-gap,lwd=2) 
abline(v=mean(sdist)+gap,lwd=2)
lset = sdist[sdist<mean(sdist)-gap]
rset = sdist[sdist>mean(sdist)+gap] 
p_value = (length(lset)+length(rset))/length(sdist)
p_value

x=DiabetesP$BMI
bdist = replicate(10000,mean(sample(x,768,replace = T)))
q = quantile(bdist,probs = c(0.025,1-0.025))
abline(v=q)
q

#nonparametric statistical testing
#10. hypothesis: the Glucose value of the diabetic female population is 140.
n = nrow(DiabetesP) 
tstat = sum(ifelse(DiabetesP$Glucose>140,1,0))
f1 = function()
{
  v = c(1,0)
  p = c(0.5,0.5)
  x = sample(x = v,replace = T,prob = p,size = n)
  return(sum(x))
}
sdist = replicate(10000,f1()) 
plot(density(sdist)) 
polygon(density(sdist),col="green")
gap = abs(mean(sdist)-tstat) 
gap
abline(v=mean(sdist)-gap,lwd=2) 
abline(v=mean(sdist)+gap,lwd=2)
lset = sdist[sdist<mean(sdist)-gap]
rset = sdist[sdist>mean(sdist)+gap] 
p_value = (length(lset)+length(rset))/length(sdist)
p_value

#bootstrapping confidence interval for Glucose
x=diabetes$Glucose
bdist = replicate(10000,mean(sample(x,768,replace = T)))
q = quantile(bdist,probs = c(0.025,1-0.025))
abline(v=q)
q

##############################################################################################
##############################################################################################
#Fitting the Models for diabetes dataset

#11. Logistic Regression
install.packages("bbmle")
library(bbmle)
library(readr)

#create function

f1 = function(a0,a1,a2,a3,a4,a5,a6,a7)
{
  x = a0 + a1*DiabetesP$Pregnancies+a2*DiabetesP$Glucose+a3*DiabetesP$BloodPressure+a4*DiabetesP$BMI+a5*DiabetesP$DiabetesPedigreeFunction+a6*DiabetesP$Age+a7*DiabetesP$Insulin
  L = ifelse(DiabetesP$Outcome==0,1/(1+exp(x)),exp(x)/(1+exp(x)))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

#provide mle function with starting values
res = mle2(minuslogl = f1,start=list(a0=0,a1=0,a2=0,a3=0,a4=0,a5=0,a6=0,a7=0))
summary(res)


# -9.07 + 0.12*preganancies + 0.037*glucose - 0.009*bloodpressure + 0.09*BMI + 0.86*diabetespredegreefunction + 0.013*age-0.0007*Insulin


##########################################################################################################################
#11.1. Logistic Regression using glm() for Binary outcome
#Partition the data into training (70%) and validation (30%)
dataset <- DiabetesP[-4] #To exclude skin Thickness

set.seed(2345)

train <- sample(nrow(dataset), 0.7*nrow(dataset)) # number of rows and size of dataset, use sample to create training annd validation sets

dataset.train <- dataset[train,]
dataset.train

df.validation <- dataset[-train,]


#11.1. logistic model usning glm

fitlogic2 = glm(Outcome ~ ., data = dataset.train, family = binomial())
summary(fitlogic2)


# calculate the performance of the model
prob = predict(fitlogic2, df.validation, type = "response")
predicted = factor(prob > 0.5,levels = c(FALSE, TRUE), labels = c("nondiabete", "diabete"))#log probability
performance = table(df.validation$Outcome, predicted, dnn = c("Actual", "Predicted"))
#create the confusion matrix
performance
# the total accuracy of logistic regression is 79.7%
#Accuracy of 1s is 79.2%

reg1 = fitlogic2
summary(reg1)

#find the possible intersaction terms 
res = step(reg1,~.^2)
res$anova

#Include the intersection term with an AIC of 501.40
reg2 = glm(Outcome ~. + Glucose:Insulin, data = dataset.train,family = binomial())
summary(reg2)
# analysis including Glucose:Insulin + Insulin:Age
AIC(reg1,reg2)
#accuracy of 1 of reg 2
prob = predict(reg2, df.validation, type = "response")
predicted = factor(prob > 0.5,levels = c(FALSE, TRUE), labels = c("nondiabete", "diabete"))#log probability
performance = table(df.validation$Outcome, predicted, dnn = c("Actual", "Predicted"))
performance
