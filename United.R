
#==================================================================
# Install packages not already installed in a list
#==================================================================

rm(list=ls())

list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate","doParallel",
       "stringi","ggplot2","maps","httr","rsdmx","devtools","plyr","dplyr","ggplot2","caret","elasticnet",
       "magrittr","broom","glmnet","Hmisc",'knitr',"RSQLite","RANN","lubridate","ggvis","plotly","lars",
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","parallel","readxl","ggmosaic",
       "vcd","Amelia","d3heatmap","ResourceSelection","ROCR","plotROC","DT","aod","mice","Hmisc","data.table",
       "corrplot","gvlma")



list_packages <- list
new.packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

R<-suppressWarnings(suppressMessages(sapply(list, require, character.only = TRUE)))

setwd("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UnitedAirlines")

data=data.table::fread("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UnitedAirlines/Take_home_Stats_test_Data.xlsx")
  



#install.packages("xlsx")
require(xlsx)
FirstTable <- read.xlsx("Take_home_Stats_test_Data.xlsx", 1 , stringsAsFactors=F)
read.xlsx("Take_home_Stats_test_Data.xlsx", sheetName = "Sheet1")
read.xlsx2("Take_home_Stats_test_Data.xlsx", sheetName = "Sheet1")


#devtools::install_github("hadley/readxl") # development version


library(readxl)

# read_excel reads both xls and xlsx files
uniteddata=read_excel("Take_home_Stats_test_Data.xlsx")


# Specify sheet with a number or name
#read_excel("my-spreadsheet.xls", sheet = "data")
#read_excel("my-spreadsheet.xls", sheet = 2)

# If NAs are represented by something other than blank cells,
# set the na argument
#read_excel("my-spreadsheet.xls", na = "NA")


model1=glm(Y~X1+X2,data=uniteddata)

broom::tidy(model1)

tidyr::gather(uniteddata)

head(broom::augment(model1))

broom::glance(model1)


#install.packages("plsdepot")
library(pls)

model2=pls::plsr(Y~X1+X2,data=uniteddata,ncomp=1)

summary(model2)

biplot(model2)
plot(model2, plottype = "biplot")


library(plsdepot)
model3=plsdepot::plsreg1(uniteddata[,3:4],uniteddata["Y"],comps=1, crosval=TRUE)
model3=mvr(Y~X1+X2,data=uniteddata,comps=1, crosval=TRUE)

model3$cor.xyt
model3$reg.coefs
model3$R2
plot(model3)


selectNcomp(model3)
pls::crossval(model3)
pls::coefplot(model3)



#load semPLS and datasets
#install.packages("BiplotGUI")
library(semPLS)
data(mobi)
data(ECSImobi)

#runs PLS SEM
ecsi <- sempls(Y=X1+X2, data=uniteddata, E="C")
ecsi

#calculate percent variation
(prcomp(scale(mobi))$sdev^2)/24

#load and open BiPlotGUI
library(BiplotGUI)
Biplots(uniteddata, PointLabels=NULL)
#right click on biplot
#select "Predict points closest to cursor positions"
#Select Prediction Tab in top-right frame


pcr_model <- pcr(Y~., data = uniteddata, scale = TRUE, validation = "CV")
summary(pcr_model)
# Plot the root mean squared error
validationplot(pcr_model)
# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")
# Plot the R2
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)
coefplot(pcr_model)
# Train-test split


# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(uniteddata, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(uniteddata[,-1], 1, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(uniteddata[,-1]),cex=.7) # add variable names
# Varimax Rotated Principal Components
# retaining 5 components 
library(psych)
fit <- principal(uniteddata[,-1], nfactors=1, rotate="varimax")
fit # print results


factanal(uniteddata[,-1],1)

y_test <-uniteddata["Y"]

index <- createDataPartition(uniteddata$Y,p=0.70, list=FALSE)

trainSet <- uniteddata[index,]

testSet <- uniteddata[-index,]


pcr_model <- pcr(Y~., data = trainSet,scale =TRUE, validation = "CV",ncomp=1)

pcr_pred <- predict(pcr_model, testSet, ncomp = 1)
mean((pcr_pred - y_test)^2)


data.table::melt.data.table(as.data.table(uniteddata))

tidyr::gather(uniteddata)

data.table::dcast.data.table()


reshape2::melt(uniteddata)

uniteddata2=data_frame(No.Trial=c(seq(1:13)),No=c(29,16,17,4,3,9,4,5,1,1,1,3,7)
,Yes=c(198,107,55,38,18,22,7,9,5,3,6,6,12))
 

m1=reshape2::melt(uniteddata2,id.vars ="No.Trial",variable.name ="Driving.school",value.name ="Frequency")                        
m2= reshape2::dcast(reshape2::melt(uniteddata2,id.vars ="No.Trial"),No.Trial~variable )


hist(m1$Frequency,breaks = "fd")

hist(log(m1$Trials),breaks = "fd")

l=list(xtabs(~Trials+Driving.school+No.Trial,data=m1))

do.call(chisq.test,xtabs(~No.Trial+Driving.school+Trials,data=m1))

tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
do.call("paste", c(tmp, sep = ""))
by(m1, m1["No.Trial"], function(x) chisq.test)

fisher.test(xtabs(No.Trial~Driving.school+Trials, data=m1),workspace = 200000000)

chisq.test(xtabs(No.Trial~Driving.school+Frequency, data=m1),simulate.p.value = T)%>%tidy()

ggplot(m1, aes(No.Trial,Frequency, group=Driving.school, linetype=Driving.school, shape=Driving.school)) + 
  geom_line() +geom_point() 
 

p=ggplot(m1, aes(No.Trial,Frequency, color=Driving.school)) + 
  geom_line() + 
  geom_point()+theme_bw()+xlab("Number of Trials")
ggplotly(p)
ggsave("p.pdf")

p1=ggplot(m1, aes( No.Trial,Frequency, color=Driving.school)) + theme_bw()+
  geom_smooth(se = FALSE, method = "loess")
ggplotly(p1)
ggsave("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UnitedAirlines/p1.pdf")


p2=ggplot(m1, aes( No.Trial,Frequency, color=Driving.school)) + theme_bw()+
  geom_histogram()
ggplotly(p2)
ggsave("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UnitedAirlines/p2.pdf")


p2=ggplot(m1, aes(No.Trial,Frequency, color=Driving.school)) + 
  geom_line() + 
 theme_bw()
ggplotly(p2)

model4=glm(log(Frequency)~(No.Trial)+Driving.school+No.Trial*Driving.school,data=m1)
summary(model4)

m1$Driving.school=if_else(m1$Driving.school=="Yes",1,0)
model5=glm(Driving.school~(No.Trial)+log(Frequency)+No.Trial*log(Frequency),data=m1,family="binomial")
summary(model5)

model6=glm(Driving.school~(No.Trial)+log(Frequency)+No.Trial*log(Frequency),data=m1,family="binomial")
summary(model6)

str(m1)


pq=qplot(log( m1$Frequency), geom="histogram",binwidth = 0.5)+theme_minimal()+xlab("log(Number of Trials)")
ggplotly(pq)
ggsave("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UnitedAirlines/plott.pdf")

qplot((m1$Frequency), geom="histogram",binwidth = 2)+theme_minimal()+xlab("Number of Trials")
ggsave("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/UnitedAirlines/plot3.pdf")


?tapply
