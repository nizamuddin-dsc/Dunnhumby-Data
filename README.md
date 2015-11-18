# Dunnhumby-Data
# Dunnhumby Data available @ https://drive.google.com/file/d/0B4nGPdxvGoGIMnFvTmdLY2lYN00/view?usp=sharing #
##"Measuring the impact of promotional activities"##
######      Dunhumby Transaction Data analysis              ########

####################################################################
###                       Read the files                         ### 
###                                                              ###
####################################################################


## first read all the data set 
dhtr <- read.csv("C:/Users/user/Desktop/new dunhumby/dh_Transaction_Data.csv", header =TRUE)
dhpr <- read.csv("C:/Users/user/Desktop/new dunhumby/dh_Products_Lookup.csv", header =TRUE)
dhst <- read.csv("C:/Users/user/Desktop/new dunhumby/dh_store_lookup.csv", header =TRUE)
dhstate <-read.csv("C:/Users/user/Desktop/new dunhumby/State_code_of_USA.csv", header =TRUE)


####################################################################
###                     Data Cleaning and Modelling              ### 
###                                                              ###
####################################################################


## check  the transaction data set 

str(dhtr)
dim(dhtr)
head(dhtr)
tail(dhtr)
sum(is.na(dhtr$PRICE)) ## checking count of na 


## check the product data set

str(dhpr)
dim(dhpr)
head(dhpr)
tail(dhpr)

## check the  store data set 

str(dhst)
dim(dhst)
head(dhst)
tail(dhst)
sum(is.na(dhst$AVG_WEEKLY_BASKETS)) ## checking count of na 
sum(duplicated(dhst$STORE_ID))## finding number of duplicatd records
dhst[duplicated(dhst$STORE_ID),]## selecting duplicate records 


## check the  state code data set 

str(dhstate)
dim(dhstate)
head(dhstate)
tail(dhstate)

## Merge the data set

colnames(dhst)[c(1)] <- c("STORE_NUM")    #changing the name
merged_store_and_transaction = merge(dhst,dhtr,by="STORE_NUM") # code for merging
str(merged_store_and_transaction)
merge_alldata = merge(merged_store_and_transaction,dhpr,by="UPC")# entire dataset merged
merged_state = merge(merge_alldata,dhstate,by="ADDRESS_STATE_PROV_CODE")
str(merged_state)
View(merged_state)


##  new data set for regression model

cols<- c(7,9:20,22,26)
regression_data <- merged_state[,cols]
#View(regression_data)
#write.csv(regression_data)


## Loading the merged data set
## regression_data <-read.csv("C:/Users/user/Desktop/new dunhumby/regression_data.csv")

#str(regression_data)

regression_data<-na.omit(regression_data) #Removing the missing values

#Data management (Variable creation and factor define)

regression_data$Promo<- ifelse(regression_data$BASE_PRICE==regression_data$PRICE, 0, 1) #Creating promotion variable
regression_data$Promo<-as.factor(regression_data$Promo)
regression_data$Brand<- ifelse(regression_data$MANUFACTURER=="PRIVATE LABEL", 0, 1) #Creating Brand variable
regression_data$Brand<-as.factor(regression_data$Brand)
regression_data$FEATURE<-as.factor(regression_data$FEATURE)
regression_data$DISPLAY<-as.factor(regression_data$DISPLAY)
regression_data$TPR_ONLY<-as.factor(regression_data$TPR_ONLY)
regression_data$WEEK_END_DATE<-as.factor(regression_data$WEEK_END_DATE)


summary(regression_data)

# attach(regression_data)

# scatter plot
names(regression_data)

# round the correlation data # [how to round up to 2 decimal point]

correg <- na.omit(regression_data[,c(2:3,5:10)])
round(cor(correg),2)

#multiple scatter plots reg data
pairs(correg, col ="red")

## log converiosn
hist(regression_data$SPEND,col="red", main="Main data plot")
regression_data$lspend<-log(regression_data$SPEND)
regression_data$lprice<-log(regression_data$PRICE)
regression_data<-regression_data[regression_data$lspend>0,]
regression_data<-regression_data[regression_data$lprice>0,]
hist(regression_data$lspend,col="green",main="Log transformed SPEND")

#Descriptive analysis on Dependent variable:

par(mfrow=c(1,2))
hist(SPEND,col="red", main="Main data plot")
data$lspend<-log(data$SPEND)
data<-data[data$lspend>0,]
hist(data$lspend,col="green",main="Log transformed SPEND")

##### Correlation coefficients and Box plots
par(mfrow=c(1,1))

M <- cor(data[,1:5]) # get correlations
library(xtable)
#xtable(M)
#install.packages("corrplot")
library(corrplot) #package corrplot
corrplot(M, method = "circle") #plot matrix

par(mfrow=c(3,2))
boxplot(SPEND~FEATURE,data,outline =F,xlab="FEATURE",ylab="SPEND" ,main="Boxplot of FEATURE category",col="blue")
boxplot(SPEND~DISPLAY,data,outline =F,xlab="DISPLAY",ylab="SPEND",main="Boxplot of DISPLAY category",col="blue")
boxplot(SPEND~TPR_ONLY,data,outline =F,xlab="TPR_ONLY",ylab="SPEND",main="Boxplot of TPR_ONLY category",col="blue")

boxplot(SPEND~Promo,data,outline =F,xlab="Promo ",ylab="SPEND",main="Boxplot by Promo",col="blue")

boxplot(SPEND~Brand,data,outline =F,xlab="Brand ",ylab="SPEND",main="Boxplot by Brand",col="blue")

par(mfrow=c(2,1))
boxplot(SPEND~SEG_VALUE_NAME,data,outline =F,xlab="SEG_VALUE_NAME ",ylab="SPEND",main="Boxplot by SEG_VALUE_NAME ",col="blue")

boxplot(SPEND~State,data,outline =F,xlab="State ",ylab="SPEND",main="Boxplot by State ",col="blue")

## promotional effect of on differnt geography
regression_data_Kentucky<-regression_data[(regression_data$State=="Kentucky"),]
par(mfrow=c(1,3))
boxplot(SPEND~FEATURE,regression_data_Kentucky,outline =F,xlab="FEATURE",ylab="SPEND" ,main="Boxplot of FEATURE category",col="blue")
boxplot(SPEND~DISPLAY,regression_data_Kentucky,outline =F,xlab="DISPLAY",ylab="SPEND",main="Boxplot of DISPLAY category",col="blue")
boxplot(SPEND~TPR_ONLY,regression_data_Kentucky,outline =F,xlab="TPR_ONLY",ylab="SPEND",main="Boxplot of TPR_ONLY category",col="blue")

regression_data_Ohio<-regression_data[(regression_data$State=="Ohio"),]
par(mfrow=c(1,3))
boxplot(SPEND~FEATURE,regression_data_Ohio,outline =F,xlab="FEATURE",ylab="SPEND" ,main="Boxplot of FEATURE category",col="blue")
boxplot(SPEND~DISPLAY,regression_data_Ohio,outline =F,xlab="DISPLAY",ylab="SPEND",main="Boxplot of DISPLAY category",col="blue")
boxplot(SPEND~TPR_ONLY,regression_data_Ohio,outline =F,xlab="TPR_ONLY",ylab="SPEND",main="Boxplot of TPR_ONLY category",col="blue")

regression_data_Texas<-regression_data[(regression_data$State=="Texas"),]
par(mfrow=c(1,3))
boxplot(SPEND~FEATURE,regression_data_Texas,outline =F,xlab="FEATURE",ylab="SPEND" ,main="Boxplot of FEATURE category",col="blue")
boxplot(SPEND~DISPLAY,regression_data_Texas,outline =F,xlab="DISPLAY",ylab="SPEND",main="Boxplot of DISPLAY category",col="blue")
boxplot(SPEND~TPR_ONLY,regression_data_Texas,outline =F,xlab="TPR_ONLY",ylab="SPEND",main="Boxplot of TPR_ONLY category",col="blue")


###### Model fitting

fit<-lm(lspend~UNITS+VISITS+HHS+FEATURE+DISPLAY+TPR_ONLY+State+SEG_VALUE_NAME+Promo+Brand,data=data,na.action=na.exclude)
summary(fit)

resid<-fit$resid
qqnorm(resid,col = "red")
qqline(fit$resid)





####                  Time series Analysis                      ###

####################################################################
###                       Read the files                         ### 
###                                                              ###
####################################################################

dat<-read.csv("C:/Users/user/Desktop/new dunhumby/single_product_single_store.csv", header = T)
#head(dat)

#Define the data as time series object: 
SPEND<- ts(dat$SPEND, frequency=52, start=c(2009,2),end=c(2011,42))
SPEND_total<- ts(dat$SPEND, frequency=52, start=c(2009,2),end=c(2012,0))

library(forecast)
library(tseries)
tsdisplay(SPEND,main ="Time series plots for main data")
adf.test(SPEND)

dif1<-diff(SPEND)
adf.test(dif1)

tsdisplay(dif1,main="Time series plots for differenced data")


################## Selecting p, d and q by this function ########

arima.mod<-function(data,diff=1,method="ML",max=c(3,3)){
  require(tseries)
  m.name<-c()
  loglik<-c()
  Aic<-c()
  Bic<-c()
  Aicc<-c()
  for(i in 0:max[1]){
    for(j in 0:max[2]){
      model<-arima(data,order=c(i,diff,j),method=method)
      mn<-paste("ARIMA(",i,",",diff,",",j,")",sep="")
      m.name<-c(m.name,mn)
      
      npar <- length(model$coef) + 1
      nstar <- length(model$residuals) - model$arma[6] - model$arma[7] * model$arma[5]
      
      bic <- model$aic + npar * (log(nstar) - 2)
      aicc <- model$aic + 2 * npar * (nstar/(nstar - npar - 1) - 1)
      Bic<-c(Bic,bic)
      Aicc<-c(Aicc,aicc)
      Aic<-c(Aic,model$aic)
      loglik<-c(loglik,model$loglik)
    }
  }
  d<-data.frame(model=m.name,loglik=loglik,aic=Aic,AICc=Aicc,bic=Bic)
  max.line<-which(Aicc==min(Aicc))
  l=list(mode=d,minline=d[max.line,])
  return(l)
}
#################

#Selected model:

c<-arima(SPEND,order=c(3,1,0),seas=list(order=c(1,1,0),period=7), method="ML")
summary(c)



#p value:
z<-c$coef/sqrt(diag(c$var.coef))
p.value<-2*(1-pnorm(abs(z),mean=0,sd=1))
p.value


#normal Q-Q plot
qqnorm(c$residuals)
qqline(c$residuals)


#check of indipendence
Box.test(c$residuals,lag=10,type="Ljung-Box")
tsdiag(c)


#Check of randomness
y<-factor(sign(c$residuals))
runs.test(y)


forcast<-forecast(c,20)
write.csv(forcast,file="Weekly_spend_SARIMA.csv")
plot(forcast,ylab="Weekly SPEND", xlab="Time")
lines(SPEND_total,col="red",lty=4)
legend("topleft",legend=c("Actual","Forecasted"),lty=c(4,1),col=c("red","blue"))


##################### Holt-winter modelling    ##########


require(graphics)
Holt_fit<- HoltWinters(SPEND)
summary(Holt_fit)
accuracy(Holt_fit)
forecasts2<- forecast.HoltWinters(Holt_fit, h=20)
plot(forecasts2, ylab="Weekly SPEND", xlab="Time")
write.csv(forecasts2,file="Weekly_SPEND_Holt.csv")
lines(SPEND_total,col="red",lty=4)
legend("topleft",legend=c("Actual","Forecasted"),lty=c(4,1),col=c("red","blue"))


####################################################################
###                                                              ###
###                   End of program                             ###
###                                                              ###
####################################################################


