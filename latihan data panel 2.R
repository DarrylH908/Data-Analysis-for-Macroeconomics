#Load the library
library(plm)
library(lmtest)

#Load the dataset
makro1 <- read.csv("D:\\Dataset BPS\\Data Makroekonomi 1.csv")
makro1

#See the corelation matrix
cor(makro1[,c('Unemployment','FDI_Net_Inflows','GDP_Nominal','Inflation')])

#Made the dataset to panel
panelis1 <- pdata.frame(makro1,index=c('Negara','Tahun'))
panelis1

sum(is.na(panelis1))

#Pooled OLS
pool1 <- plm(log(GDP_Nominal)~log(FDI_Net_Inflows)+Inflation+Unemployment,data=panelis1,model='pooling')
summary(pool1)

#Fixed Effect
fixed1 <- plm(log(GDP_Nominal)~log(FDI_Net_Inflows)+Inflation+Unemployment,data=panelis1,model='within')
summary(fixed1)

#Random Effect
random1 <- plm(log(GDP_Nominal)~log(FDI_Net_Inflows)+Inflation+Unemployment,data=panelis1,model='random')
summary(random1)

#Test the chow test
pooltest(pool1,fixed1)

#Test The Hausman Test
phtest(fixed1,random1)

#Test the Lagrange Multiplier
plmtest(random1,type='bp')

#So we choose random test because in hausman we choose random and p value is high than 0.05

#Heteroskedasticity test
bptest(random1)
#There are heteroskedasticity because p value is below 0.05

#Autocorelation test
pbgtest(random1)
#There are serial corelation in autocorelation because p value below 0.05

#Fix the heteroskedasticity
coeftest(random1,vcov.=vcovHC(random1,type='HC1',cluster='group'))
#So after the test the inflation is not significant for the change of GDP nominal
#And unemployment is negative significant influence the GDP Nominal it inferred by every 1 percent unemployment decrease of GDP Nominal
#FDI have positive significant result on GDP Nominal and every 1% of FDI increase the 8 percent of GDP Nominal


