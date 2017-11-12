Question 1: 1.	The results of an experiment involving a chemical reaction are given 
#in the file CHEM.DAT 

chem <- read.table("C:/Users/awong/Downloads/CHEM.dat")
chem
names(chem) <- c("IV", "%UCSM", "%CDP", "%UBP", "Temp", "Conc", "Time")
chem

#b.	Model V2 (%UCSM), V3 (%CDP), V4(%UBP) as response variables and V5 (Temp), V6 (Conc) and V7 (Time) as covariates 
#using a multivariate multiple regression

summary(chem)
pairs(chem)

qqnorm(chem$`%UCSM`) #Looks fairly normal
qqnorm(chem$`%CDP`) #Looks fairly normal
qqnorm(chem$`%UBP`) #Looks fairly normal

#Create model
mlml <- lm(cbind(`%UCSM`, `%CDP`, `%UBP`) ~ Temp + Conc + Time , data = chem)
summary(mlml)
#All sig for %UCSM
#Only Temp sig for %CDP
#Temp, Conc sig for %UBP

#Model with just Temp since it was the only variable sig for all. 
mlml2 <- update(mlml, .~. -Conc -Time )
summary(mlml2)

#Compare mlml and mlml2
anova(mlml, mlml2) #pvalue is less than .01
#Since pvalue is sig low, we need to add more terms. 

#Model with Conc and Temp since just Temp model wasn't good enough
mlml3 <- update(mlml, .~. -Time )

#Compare mlml2 to mlml3
anova(mlml3, mlml2) #pvalue low so better model would include Conc and Temp

#Compare mlml3 to mlml to see if all terms are needed
anova(mlml, mlml3) #Since pvalue is low it is better to have all the terms in the model than just Conc and Temp

##Looks like best model is mlml with all the terms. 


##variance/covariance matrix is joint for both responses
vcov(mlml)
##To test if variables should be included in the model
anova(mlml) #Says all terms should be included just like I said earlier

##Check diagnostics
#Look at residuals
head(resid(mlml))

#Look at fitted
head(fitted(mlml))

#plots 
plot(fitted(mlml) [,1], resid(mlml)[,1]) #No pattern so constant variances
