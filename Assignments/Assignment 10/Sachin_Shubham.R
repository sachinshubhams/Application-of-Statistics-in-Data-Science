library(lme4) 
library(arm)  
library(Matrix)
setwd("C:/Users/sachi/Downloads")
politeness_data<-read.csv(file.choose())


#0

boxplot(frequency ~ attitude*gender, data=politeness_data, col=c("#ef8a62","#67a9cf"),
        main="Relationship between Gender politeness and pitch",
        xlab="Gender Attitude", ylab="Frequency",
        names = c("Inf.Female","Pol.Female","Inf.Male","Pol.Male"))
#Result:
#	In	both	cases,	the	median	is	lower	 for	 the	 polite	 than	 for	 the	
#informal	but,	 there	is	more	overlap	between	the	informal and polite	for
#the males	than	for	the females.
#1

lmer_attitude = lmer(frequency ~ attitude  + (1|subject) + (1|scenario), data=politeness_data)
summary(lmer_attitude)



#2

lmer_gender <- lmer(frequency ~ gender + (1|subject) + (1|scenario), data=politeness_data)
summary(lmer_gender)


#3

lmer_attitude_gender <- lmer(frequency ~ gender + attitude + (1|subject) + (1|scenario), data=politeness_data)
summary(lmer_attitude_gender)


anova(lmer_attitude,lmer_gender)
anova(lmer_attitude,lmer_attitude_gender)
anova(lmer_attitude_gender,lmer_gender)
