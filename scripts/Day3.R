
library(tidyverse)
library(lmerTest)
library(emmeans)

setwd("C:/Users/cla473/Documents/DataSchool/StatModel/raw_data")

respire <- read.csv("Prac 4 dark respiration.csv")
str(respire)

respire$Leaf_stage <- factor(respire$Leaf_stage)
respire$Leaf_section <- factor(respire$Leaf_section)
str(respire)

ggplot(respire, aes(x=Species, y=Dry_mass_resp, colour=Leaf_section)) + geom_boxplot()
#doesn't seem to be an interaction between section (same pattern for all)
#there is a section effect, no apparent species affect
ggplot(respire, aes(x=Species, y=Dry_mass_resp, colour=Leaf_stage)) + geom_boxplot()
#nothing conclusive from this one

#comparing between plants  (9)
#                  leaves with plants  (27)
#                  sections within a Leaf  (135)

model <- lmer(Dry_mass_resp~Species*Leaf_stage*Leaf_section+(1|Plant_ID) + (1|Leaf_stage:Plant_ID), data=respire)
anova(model)
plot(model)


#===========================================================
#Exam
#===========================================================
AraData <- read.csv("Prac 5 Arabidopsis nitrogen.csv")
str(AraData)

AraData$Tank <- factor(AraData$Tank)
AraData$Nitrogen <- factor(AraData$Nitrogen)
AraData$Day <- factor(AraData$Day)
str(AraData)

ggplot(AraData, aes(x=Nitrogen, y=PlantDM, colour=Day)) + geom_point(position=position_dodge(width=0.9))
ggplot(AraData, aes(x=Day, y=PlantDM, colour=Nitrogen)) + geom_point(position=position_dodge(width=0.9))

ggplot(AraData, aes(x=Day, y=PlantDM, colour=Nitrogen)) + geom_boxplot()

#variability may be an issue over time .... not much variation on day 10
#but much larger variation on day 24
ggplot(AraData, aes(x=Day, y=log(PlantDM), colour=Nitrogen)) + geom_boxplot()

lm1 <- lm(PlantDM~Nitrogen*Day, data=AraData)
anova(lm1)
lm2 <- lm(log(PlantDM)~Nitrogen*Day, data=AraData)
anova(lm2)

lm1er <- lmer(PlantDM~Nitrogen*Day+(1|Tank), data=AraData)
anova(lm1er)
lm1er2 <- lmer(log(PlantDM)~Nitrogen*Day+(1|Tank), data=AraData)
anova(lm1er2)
#blocking for tank doesn't make any difference

#the 'type="response"' converts from log back to real values
emmeans(lm1er, ~Nitrogen*Day)
emmeans(lm1er2, ~Nitrogen*Day, type="response")
emmeans(lm1er2, pairwise~Nitrogen*Day, type="response")
emmeans(lm1er2, pairwise~Nitrogen|Day, type="response")  #this is Nitrogen within Day

plot(lm1er)
plot(lm1er2)



#========================================================================================
plants <- read.csv("Prac 5 plant growth.csv")
str(plants)

plants$Dose <- factor(plants$Dose)
plants$Row <- factor(plants$Row)
plants$Column <- factor(plants$Column)

ggplot(plants, aes(x=Dose, y=height)) + geom_point() 

lmp <- lm(height~Dose, data=plants)
summary(lmp)
anova(lmp)

lmp2 <- lm(height~as.numeric(Dose), data=plants)
summary(lmp2)
anova(lmp2)

lm3 <- lmer(height~as.numeric(Dose) +(1|Row)+(1|Column), data=plants)
anova(lm3)

lm4 <- lmer(height~Dose +(1|Row)+(1|Column), data=plants)
anova(lm4)
summary(lm4)

emmeans(lm4, pairwise~Dose)
plot(lm4)

#====================================================================
nem <- read.csv(("Prac 5 nematode.csv"))
str(nem)

nem$Experiment <- factor(nem$Experiment)
nem$Genotype <- relevel(nem$Genotype, ref="WT")
new$Treatment <- relevel(nem$Treatment, ref="Uninfected")
str(nem)

ggplot(nem, aes(x=Genotype, y=Plant.weight, colour=Treatment)) + 
    geom_point(position=position_dodge(width=0.9))

ggplot(nem, aes(x=Genotype, y=Plant.weight, colour=Treatment)) + 
    geom_boxplot() +
    facet_wrap(~Experiment)

ggplot(nem, aes(x=Genotype, y=Plant.weight, colour=Treatment)) + 
    geom_boxplot()

lmn <- lm(Plant.weight~Genotype*Treatment, data=nem)
summary(lmn)
anova(lmn)

lmn2 <- lmer(Plant.weight~Genotype*Treatment + (1|Experiment), data=nem)
summary(lmn2)
anova(lmn2)

emmeans(lmn, pairwise~Genotype)
plot(lmn)

#=================================================================
banana <- read.csv("Prac 5 respiration data.csv")
str(banana)

banana$shelter <- factor(banana$shelter)
banana$plantID <- factor(banana$plantID)
banana$Temp <- factor(banana$Temp, levels = c("low", "med", "high", "v high"))
str(banana)

ggplot(banana, aes(x=Genotype, y=rrarea1, colour=Temp)) + geom_point(position=position_dodge(width=0.5))

model1 <- lm(rrarea1~Genotype*Temp+shelter+plantID, data=banana)  

model2 <- lmer(rrarea1~Genotype*Temp+shelter + (1|shelter) + (1|plantID:shelter), data=banana)       
anova(model2)
summary(model2)

emmeans(model2, ~Genotype)
emmeans(model2, pairwise~Genotype)

plot(model2, which=1)
