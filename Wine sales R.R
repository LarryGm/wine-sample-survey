
'''{r}
# Analyzing Wine Sales for Distribution Sampling program
#needed packages
library(mice)
library(Hmisc)
library(psych)
library(CARET)
library(VIM)
library(MASS)

set.wd(~/"411 folder")

wncase1<-read.csv("wine.csv")
str(wncase)
#checking the information based on Target values.
wncase1
Target8<-subset(wncase1,TARGET == 8)
summary(Target8)
Targetup<-subset(wncase1,TARGET>=4)
summary(Targetup)
Target0<-subset(wncase1,TARGET==0)
summary(Target0)

winecs<-wncase1[,2:16]#to eliminate index from our analysis

#we are closing out the na values on stars and then creating new variable of no rating
winecs$STARS[ is.na(winecs$STARS) ]<- 0
summary(winecs$STARS)
winecs$NoRating<-ifelse(winecs$STARS == 0,1,0)
summary(winecs$NoRating)

#does this work for correlation?

nostarna<- na.omit(wncase1)
cor(nostarna$TARGET,nostarna$STARS) 
cor(WineTg$TARGET,WineTg$STARS)
#so moving na values in STARS to zero makes an impact.  Creating the new variable did not.

cor(WineTg$TARGET,WineTg$NoRating) #for it is negative and highly collinear to STARS.

#checking chemical variables with correlation to find collinearity,but have to tackle na values
#prediction vs. median
imputed_data<- mice(winecs, m=3, maxit=20, method= pmm, seed=500) #pmm for continuous, cart for binary
head(imputed_data$imp$Chlorides, 25)
WineTt<-complete(imputed_data,1) # (3) is a manual choice from the results of the imputed data calculations

#set up several variables imputing median values and we will test correlations.
#found two with similar # of na values so it is a comparable analysis
winecs$Chlorides[ is.na(winecs$Chlorides) ]<- .0460 #median value
winecs$TotalSulfurDioxide[ is.na(winecs$TotalSulfurDioxide) ]<- 123.0 #median value

#Finding Correlation with TARGET
cor(winecs$TARGET,winecs$Chlorides) # -0.03725814
cor(winecs$TARGET,winecs$TotalSulfurDioxide) #0.05011113
cor(WineTg$TARGET,WineTg$Chlorides) # -0.03672294
cor(WineTg$TARGET,WineTg$TotalSulfurDioxide) # 0.0523294
#winner is predicted values (unusual event)

#compare sugar values
Lowsugar<-subset(WineTg,ResidualSugar<=5)
Highsugar<-subset(WineTg,ResidualSugar > 5)

par(mfrow=c(1,2))
plot(Lowsugar$ResidualSugar,Lowsugar$TARGET,main = "Low Sugar Effects")
plot(Highsugar$ResidualSugar,Highsugar$TARGET,main = "High Sugar Effects")

#look at marketing items vs, Target
plot(wncase1$STARS,wncase1$TARGET, main = "Stars vs. Target")
plot(wncase1$LabelAppeal,wncase1$TARGET, main = "Label vs. Target")

#look at ph and acidity
hist(WineTg$pH,main = "pH values")
hist(WineTg$VolatileAcidity, main = "Volatile Acidity")

#fix the few negative alcohol values 
WineTg$Alcohol<- abs(WineTg$Alcohol)

#correlation with the entire list of variables to see correlation with target and each other. Lots of opportunity for collineation
cor.ci(WineTg)

#review our dependent variable to learn of best method.
hist(WineTg$TARGET)
abline(v=3, col="red")
summary(WineTg$TARGET)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.000   3.000   3.029   4.000   8.000 

sd(WineTg$TARGET) # 1.926368

#Calculate variance
1.93^2  # 3.7249   #variance is 3.72 while mean is 3.03, we will just have to try them all.

#trying poisson, nb, and zero poisson with best variable STARS.
library(pscl)

Starpoi<-glm(TARGET~STARS,WineTg, family = poisson)
Summary(Starpoi) #to check the stats on coefficients check
AIC(Starpoi) #47598

Starnb<-glm.nb(TARGET~STARS,WineTg)
AIC(Starnb) #47600
vuong(Starnb,Starpoi)#looking at AIC, poisson wins

Starzero<-zeroinfl(TARGET~STARS,WineTg)
AIC(Starzero) #42975 cant do vuoung, zero influ has different counts

#Analyzing the best two variables STARS and LabelAppeal with various predictive analysis
top2glm<-glm(TARGET~STARS + LabelAppeal,WineTg, family = poisson)
summary(top2glm) #coef. good statistically
AIC(top2glm) #47168 better than Star alone but not zero.

top2nb<-glm.nb(TARGET~STARS + LabelAppeal,WineTg)
AIC(top2nb) #47171

vuong(top2nb,top2glm) #no longer using this.  I can see from AIC.
top2zero<-zeroinfl(TARGET~STARS + LabelAppeal,WineTg)
AIC(top2zero) # 41355

#zero influence top AIC.  Will start using zero nb. We will drop Nb, since it is the weakest of the models so far.

mlvpoi<-glm(TARGET~STARS + LabelAppeal + Alcohol + TotalSulfurDioxide + FreeSulfurDioxide + AcidIndex, WineTg, family = poisson)
summary(mlvpoi)# checking significance Alchol is in a bad spot, will keep it in but will pull up another summary to check it
AIC(mlvpoi) # 46283

mlvzero<-zeroinfl(TARGET~STARS + LabelAppeal + Alcohol + TotalSulfurDioxide + FreeSulfurDioxide + AcidIndex, WineTg)
summary(mlvzero) #alcohol is saved!
AIC(mlvzero) #40510

mlvznb<-zeroinfl(TARGET~STARS + LabelAppeal + Alcohol + TotalSulfurDioxide + FreeSulfurDioxide + AcidIndex, WineTg, dist = negbin)
AIC(mlvznb) #40512.5
        
#QQ plots to compare analysis         
qqnorm(mlvpoi$residuals,main = "QQ-NB for STARs")
qqline(mlvpoi$residuals)

qqnorm(mlvzero$residuals,main = "QQ-NB for STARs")
qqline(mlvzero$residuals) 
#Result : both have some discrepancies, but the AIC speaks louder.
                 
#Tried to use ROC, but with the zero influence method, it changes the observation record, so  
#cant seem a work around to use it to determine model effectiveness.  Book says QQplot.
                 
#bring in test data to apply our best model
winetest<-read.csv("wine_test.csv")
                 
summary(winetest)
                 
# manipulate similar dataset to create same context for modeling
winetest$STARS[ is.na(winetest$STARS) ]<- 0 # no na's
                 
#impute other nas
imputed_data<- mice(winetest, m=3, maxit=20, method='pmm', seed=500)
WineTt<-complete(imputed_data,1)
summary(WineTt)
                 
#Alcohol needs to be be positive
WineTt$Alcohol<- abs(WineTt$Alcohol)
                 
#shorten variable names
Stars<-WineTt$STARS
Label<-WineTt$LabelAppeal
Alcohol<-WineTt$Alcohol
TSD<-WineTt$TotalSulfurDioxide
FSD<-WineTt$FreeSulfurDioxide
Acid<-WineTt$AcidIndex
    
#engage model
count<- 1.18 +  .101*(STARS) + .234*(LabelAppeal)  -.019*(AcidIndex) + .007*(Alcohol) -
                   .0000196 *(TSD) +  .000028*(FSD)     
                 
P_SCORE_ZIP_ALL <- exp(count)
                 
zeroi <- (-3.07) - 2.376 *(Stars) + .726*(Label)  + .428*(Acid) + .028*(Alcohol) - .001 *(TSD) - .00075*(FSD)

#to tally the final numbers.....
P_SCORE_ZERO <- exp(zero)/(1+exp(zero))
                 
P_SCORE_ZIP = P_SCORE_ZIP_ALL * (1-P_SCORE_ZERO)
                 
P_SCORE_ZIP<-round(P_SCORE_ZIP, 0)
WineScore<-cbind(WineTt$INDEX,P_SCORE_ZIP)
head(WineScore)

#Finish by writing results to Excel.                 
write.csv(WineScore, file = "Winesong.csv")
'''
