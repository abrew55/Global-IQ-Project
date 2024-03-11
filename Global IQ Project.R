IQ = read.csv("C:/Users/david/Downloads/AveIQ.csv")
library(lme4)
library(lmerTest)
library(pbkrtest)
library(ggplot2)
summary(finalmod)
###### DATA CLEANING / PREPROCESSING ######
IQ$Population = as.numeric(IQ$Population)
na = which(is.na(IQ), arr.ind=TRUE)

ind=0
for(x in 1:length(na)){
  print(IQ[na[ind], "Country"])
  ind=ind+1
}

#some of the prominent countries with na's are Taiwan, North Korea, Peurto Rico, and Finland.
#None of the counties represent a major loss of population or data in general. Therefore, the countries with missing data will be removed from the analysis (14 in total). 

IQ = na.omit(IQ)

####### DATA EXPLORATION ######
#The first thing I wanted to test was the correlation between HDI and other predictors. As stated in the report, the HDI is a composite measure that houses some of the other predictors. 
cor(IQ$HDI, IQ$GNI)
cor(IQ$HDI, IQ$Mean_years._of_schooling)
#While the HDI and GNI could be a concern with a correlation of 0.7878, HDI and mean years of schooling is a major concern with a correlation of 0.915.
#Here are some plots to further show that.
plot(IQ$HDI, IQ$GNI, xlab = "HDI", ylab="GNI")
plot(IQ$HDI, IQ$Mean_years._of_schooling, xlab="HDI", ylab="Mean years of schooling")
#Just from those basic plots it's very easy to distinguish linear relationships.

#Now I'm going to test for correlations between the response (IQ score) and predictors.  

#Removing the categorical variables.
IQcor = IQ[,-c(2,4)]
cor(IQcor)
#Every variable except for Nobel prizes has a correlation greater than 0.63. Rank would logically have a high correlation since it is a function of IQ (and will therefore be removed).
#This should grant me plenty of flexibility in building my model, but I will need to keep the previously discovered correlations in mind. 
#I may also need to make a decision between mean years of schooling and literacy rates. These would logically be correlated and the results prove that. 

pairs(IQcor)
#This shows the correlations in graphical form. It looks like all predictors that exhibit a relationship with average IQ, do so in a linear fashion. The only one in contention is GNI, but that looks mostly linear with a slight tail at the end.


###### MODEL BUILDING / MODEL SELECTION ######
#The random effect for this project will be the continents. Further explanation will be provided in my report. 
rantest = aov(AverageIQ ~ Continent, data=IQ)
summary(rantest)
#There is clearly a significant average IQ difference among the continents. This provides some justification for the inclusion of the random effects term.

#Building this full model took some effort. First, the model didn't accept the countries as a factor. I don't know if it has to do with sheer size or what, but would be interesting to examine the differences between each nation.
#Second, the model automatically dropped highly collinear predictors. I had already made the choice to leave out HDI for now, but the model automatically dropped literacy rates due to the aforementioned relationship with mean years of schooling.
#Due to these factors, I may build some alternate models to include some of the predictors that were left out during initial testing.
fullmod = lmer(AverageIQ ~ (1|Continent)+NobelPrices+Mean_years._of_schooling+GNI+Population, data=IQ, REML = FALSE)
summary(fullmod)
#I will be using the Kenward-Rogers method of F-test with adjusted degrees of freedom. Just based off of the t value and tested correlations, the first predictor I'm going to test is Nobel prizes.

nobelmod = lmer(AverageIQ ~ (1|Continent)+Mean_years._of_schooling+GNI+Population, data=IQ, REML = FALSE)
KRmodcomp(fullmod, nobelmod)
#With a p-val of 0.9285, it is very clear that Nobel prizes contribute very little to this model and to influencing a nation's average IQ.

#Next predictor to be tested is population. 
popmod = lmer(AverageIQ ~ (1|Continent)+Mean_years._of_schooling+GNI, data=IQ, REML = FALSE)
KRmodcomp(nobelmod, popmod)
#With a p-val of 0.0565, I will choose to leave it in the model. I would remove it if using a significance level of 0.05, but I am going to use a cutoff of 0.1 for this project. 

#Next predictor to be tested is GNI
GNImod = lmer(AverageIQ ~ (1|Continent)+Mean_years._of_schooling+Population, data=IQ, REML = FALSE)
KRmodcomp(nobelmod, GNImod)
#GNI is highly significant and will be included in the model.

#The final predictor to be tested is mean years of schooling.
schoolmod = lmer(AverageIQ ~ (1|Continent)+GNI+Population, data=IQ, REML = FALSE)
KRmodcomp(nobelmod, schoolmod)
#Mean years of schooling is highly significant.

#The fixed effects to be included in the final model are GNI, population, and mean years of schooling. 
#Here is another quick test of significance of the random effects term, this time within the context of the model.
ranova(nobelmod)
#Continent as a random effects term is clearly significant.
finalmod = nobelmod
mod1 = lm(AverageIQ ~ Population, data=IQ)

###### RANDOM VS FIXED ######
fixanova = aov(AverageIQ ~ Continent+Mean_years._of_schooling+GNI+Population, data=IQ)
model.tables(fixanova)
ranef(finalmod)$Continent
#There is clearly a shrinkage for all of the continents when used as a random effect vs fixed effect.
fixmod = lm(AverageIQ ~ Continent+Mean_years._of_schooling+GNI+Population, data=IQ)
summary(fixmod)

###### DIAGNOSTICS ######

plot(finalmod)
#The fitted vs residuals plot shows that a transformation of the response could be beneficial to the model. 
#The code doesn't show it, but I tested common transformations on the response and actually encountered worse performance on this test. 
mod = fortify.merMod(finalmod)
ggplot(mod, aes(sample=.resid))+
  stat_qq()
#The qq plot indicates normality of the residuals. 

###### Predictions ######

x = table(IQ$Continent)
#I'm first going to predict the average IQ for each continent given the average value for each predictor in the data.
pop = rep(mean(IQ$Population), times=8)
gni = rep(mean(IQ$GNI), times=8)
schl = rep(mean(IQ$Mean_years._of_schooling), times=8)
conts = names(x)
test = data.frame("Population"=pop,"GNI"=gni,"Mean_years._of_schooling"=schl, "Continent"=conts)  

predict(finalmod, newdata=test)
test$Continent
#It's clear that there is a lot of variation in the IQ predictions across the continents. 
#Oceania has the highest predicted average IQ and Africa has the lowest.

#I'll perform a bootstrap procedure to determine the uncertainty in these predictions. 
group.sd <- as.data.frame(VarCorr(finalmod))$sdcor[1] # sqrt of var. comp. due to operators
resid.sd <- as.data.frame(VarCorr(finalmod))$sdcor[2] # sqrt of var. comp. due to errors
pv <- data.frame("Africa","Asia","Central America","Europe","Eastern Europe","North America","Oceania","South America") # predicted values
ind=1
while(ind<9){
  for(i in 1:1000){
    y <- unlist(simulate(finalmod, use.u=TRUE))
    # if use.u=TRUE, generate a simulation conditional on the current random-effects estimates.
    newmod <- refit(finalmod, y)
    pv[i,ind] <- predict(newmod, newdata=test[ind, ]) + rnorm(n=1,sd=resid.sd)
    
  }
  ind=ind+1
}
pv$X.Africa. = as.numeric(pv$X.Africa.)
pv$X.Asia. = as.numeric(pv$X.Asia.)
pv$X.Central.America. = as.numeric(pv$X.Central.America.)
pv$X.Europe. = as.numeric(pv$X.Europe.)
pv$X.Eastern.Europe. = as.numeric(pv$X.Eastern.Europe.)
pv$X.North.America. = as.numeric(pv$X.North.America.)
pv$X.Oceania. = as.numeric(pv$X.Oceania.)
pv$X.South.America. = as.numeric(pv$X.South.America.)
for(x in 1:ncol(pv)){
  print(conts[x])
  print(quantile(pv[,x], c(0.025, 0.975)))
}
conts

#The focus of my predictions will center around predicting past IQ's for nations.
#I will specifically focus on the US for simplicity sake.
#The data is recorded every five  years, starting in the year 1960.
USA = read.csv("C:/Users/david/Downloads/USAdata.csv")
ind=1
for(x in 1:nrow(USA)){
  USA[ind,1] = gsub(",","",USA[ind,1])
  USA[ind,2] = gsub(",","",USA[ind,2])
  ind=ind+1
}
USA$Population = as.numeric(USA$Population)
USA$GNI = as.numeric(USA$GNI)

USAtest = USA[1:12,-4]
preds = predict(finalmod, newdata=USAtest)

USAplot = data.frame(USA,preds)
USAplot$Year = as.factor(USAplot$Year)
ggplot(USAplot, aes(x=Year,y=preds,color="red"))+
  geom_line(group=1)+
  xlab("Year")+
  ylab("Average IQ")+
  ylim(70,110)+
  theme(legend.position = "none")+
  ggtitle("Average IQ in the United States 1965-2020")
