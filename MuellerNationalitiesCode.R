
#set working directory 
setwd("~/6.pending/2.articles/BR effects")


#load libraries
library(foreign)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)    
library(stargazer)
library(sjPlot) 
library(sjmisc)
library(sjlabelled)
library(car)
library(readxl)
library(xlsx)
library(systemfit)
library(jtools)
library(xtable)
library(corrplot)
library(effects)
library(RSwissMaps)
library(swissdd)
library(ggrepel)
library(skimr)
library(tidyverse)
library(lubridate)
library(data.table)
library(lme4)
library(lmerTest)
library(plm)
library(huxtable)
library(officer)
library(flextable)
library(margins)
library(ggeffects)
library(jtools)
library(TMB)
library(performance)
library(Hmisc)


#load  and prepare data

long2 <- readRDS("BReffectslong.Rda") 

#create new dummy for govt presence
long2$govtminister <- ifelse(long2$BR == '1'| long2$BR == '2' , 1, 0) #double presences BE and ZH counted as single presence

#participation as numeric
long2$bet <- as.numeric(long2$bet)

#add periods
long2$period[long2$year <= 1891] <- "1848-91"
long2$period[long2$year >1891 & long2$year <= 1919] <- "1892-1919"
long2$period[long2$year >1919 & long2$year <= 1959] <- "1920-59"
long2$period[long2$year >1959 & long2$year <= 1992] <- "1960-92"
long2$period[long2$year >1992] <- "1993-2022"

#create linguistic minority dummies
long2$language <- "0"
long2$language[long2$canton == "GE" | long2$canton == "VD" | long2$canton == "FR" |
                 long2$canton == "VS" | long2$canton == "JU" | long2$canton == "NE"] <- "1"
long2$language[long2$canton == "TI"] <- "2"
table(long2$canton,long2$language)
long2$language <- factor(long2$language, labels=c("german","french","italian"))

#create Sonderbund dummy

long2$sonderbund <- "0"
long2$sonderbund[long2$canton == "FR" | long2$canton == "LU" | long2$canton == "NW" |
                 long2$canton == "OW" | long2$canton == "SZ" | long2$canton == "UR" |
                 long2$canton == "VS" | long2$canton == "ZG"] <- "1"
table(long2$canton,long2$sonderbund)
long2$sonderbund <- factor(long2$sonderbund, labels=c("no","yes"))

###multilevel models

#add labels using sjlabelled
set_label(long2$bet) <- "cantonal turnout"
set_label(long2$diff) <- "abs. cantonal approval difference to RoCH"
set_label(long2$diffpart) <- "cantonal turnout difference to RoCH"
set_label(long2$NRapproval) <- "% approval in National Council"
set_label(long2$rechtsform) <- "legal type of referendums"
set_label(long2$konkordanz2) <- "major party recommendations"
set_label(long2$govtminister) <- "federal council member"
long2$vote <- long2$anr
long2$govtminister <- set_labels(long2$govtminister, labels = c("no", "yes"))

#new tables for paper: TURNOUT
M1a <- lmer (bet ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton),data=long2)
M2a <- lmer (bet ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1 | canton),data=long2)
M3a <- lmer (bet ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1 | canton),data=long2)

tab_model(M1a,M2a,M3a,file="tabA2.htm",show.aic=T)

plot_model(M1a,vline.color = "red",ci.lvl = 0.9)+theme_bw()+scale_colour_grey()
me1 <- ggpredict(M1a, terms = c("govtminister"),type = "re",ci.lvl = 0.9)

me2 <- ggpredict(M2a, terms = c("language","govtminister"),type = "re",ci.lvl = 0.9)
plot(me2)+scale_colour_grey()

me3 <- ggpredict(M3a, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me3)+scale_colour_grey()

check_collinearity(M1a)


#Table A-3: turnout differentials
M1b <- lmer (diffpart ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton),data=long2)
M2b <- lmer (diffpart ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1 | canton),data=long2)
M3b <- lmer (diffpart ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1 | canton),data=long2)

tab_model(M1b,M2b,M3b,file="tabA3.htm",show.aic=T)

plot_model(M1b,vline.color = "red",ci.lvl = 0.9)+theme_bw()+scale_colour_grey()

me1 <- ggpredict(M1b, terms = c("canton","govtminister"),type = "re",ci.lvl = 0.9)
plot(me1)+scale_colour_grey()

me2 <- ggpredict(M2b, terms = c("language","govtminister"),type = "re",ci.lvl = 0.9)
plot(me2)+scale_colour_grey()
me2

me3 <- ggpredict(M3b, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me3)+scale_colour_grey()

#Table A-4: official (probit or logit: egal laut AH! :)
M1c <- glmer (official ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton), family = binomial("logit"), data=long2)
M2c <- glmer (official ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1 | canton), family = binomial("logit"),data=long2)
M3c <- glmer (official ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1 | canton), family = binomial("logit"),data=long2)

tab_model(M1c,M2c,M3c,file="tabA4.htm",show.aic=T)

plot_model(M1c,vline.color = "red",ci.lvl = 0.9)+theme_bw()+scale_colour_grey()
me1 <- ggpredict(M1c, terms = c("govtminister"),type = "re",ci.lvl = 0.9)
me1


me2 <- ggpredict(M2c, terms = c("language","govtminister"),type = "re",ci.lvl = 0.9)
plot(me2)+scale_colour_grey()

me3 <- ggpredict(M3c, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me3)+scale_colour_grey()


#Table A-5: overruled
M1d <- glmer (overruled ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton), family = binomial("logit"), data=long2)
M2d <- glmer (overruled ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1 | canton), family = binomial("logit"),data=long2)
M3d <- glmer (overruled ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1 | canton), family = binomial("logit"),data=long2)

tab_model(M1d,M2d,M3d,file="tabA5.htm",show.aic=T)

plot_model(M1d,vline.color = "red",ci.lvl = 0.9)+theme_bw()+scale_colour_grey()

me2 <- ggpredict(M2d, terms = c("language","govtminister"),type = "re",ci.lvl = 0.9)
plot(me2)+scale_colour_grey()

me3 <- ggpredict(M3d, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me3)+scale_colour_grey()


#Table A-6: approval differential 
M1e <- lmer (diff ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton), data=long2)
M2e <- lmer (diff ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1 | canton),data=long2)
M3e <- lmer (diff ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1 | canton),data=long2)

tab_model(M1e,M2e,M3e,file="tabA6.htm",show.aic=T)

plot_model(M1e,vline.color = "red",ci.lvl = 0.9)+theme_bw()+scale_colour_grey()
me1 <- ggpredict(M1e, terms = c("govtminister"),type = "re",ci.lvl = 0.9)

me2 <- ggpredict(M2e, terms = c("language","govtminister"),type = "re",ci.lvl = 0.9)
plot(me2)+scale_colour_grey()

me3 <- ggpredict(M3e, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me3)+scale_colour_grey()


tab_model(M2a,M2b,M2c,M2d,M2e,file="tab1.htm",show.aic=T)



#robustness 1: year instead of periods

M1 <- lmer (bet ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1 | canton),data=long2)
M2 <- lmer (bet ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1+govtminister | canton),data=long2)
M3 <- lmer (bet ~ rechtsform + year + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long2)
M4 <- lmer (bet ~ rechtsform + year + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long2)

tab_model(M1,M2,M3,M4,file="tabA2-year.htm",show.aic=T)

check_collinearity(M1)


M1 <- lmer (diffpart ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1 | canton),data=long2)
M2 <- lmer (diffpart ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1+govtminister | canton),data=long2)
M3 <- lmer (diffpart ~ rechtsform + year + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long2)
M4 <- lmer (diffpart ~ rechtsform + year + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long2)

tab_model(M1,M2,M3,M4,file="tabA3-year.htm",show.aic=T)

M1 <- glmer (official ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1 | canton), family = binomial("logit"), data=long2)
M2 <- glmer (official ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M3 <- glmer (official ~ rechtsform + year + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M4 <- glmer (official ~ rechtsform + year + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)

tab_model(M1,M2,M3,M4,file="tabA4-year.htm",show.aic=T)

M1 <- glmer (overruled ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1 | canton), family = binomial("logit"), data=long2)
M2 <- glmer (overruled ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M3 <- glmer (overruled ~ rechtsform + year + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M4 <- glmer (overruled ~ rechtsform + year + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)

tab_model(M1,M2,M3,M4,file="tabA5-year.htm",show.aic=T)

me4 <- ggpredict(M4, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me4)+scale_colour_grey()


M1 <- lmer (diff ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1 | canton), data=long2)
M2 <- lmer (diff ~ govtminister + rechtsform + year + konkordanz2 + (1 | vote) + (1+govtminister | canton),data=long2)
M3 <- lmer (diff ~ rechtsform + year + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long2)
M4 <- lmer (diff ~ rechtsform + year + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long2)

tab_model(M1,M2,M3,M4,file="tabA6-year.htm",show.aic=T)


#correlations

test <- subset(long2, select=c("rechtsform","year", "period","konkordanz2","NRapproval"))

describe(test)
datamatrix0 <- cor(test,use="pairwise.complete.obs")
corrplot(datamatrix0, method="number")

ggplot(long2, aes(x=konkordanz2, y=NRapproval)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=23, size=4)

ggplot(long2, aes(x=rechtsform, y=NRapproval)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=23, size=4)

ggplot(long2, aes(x=konkordanz2, y=NRapproval,color=rechtsform)) + 
  geom_boxplot()+ stat_summary(fun=mean, geom="point", shape=23, size=4)

aggregate(long2$NRapproval ~long2$rechtsform,FUN=median, na.rm = TRUE)
aggregate(long2$NRapproval ~long2$rechtsform,FUN=mean, na.rm = TRUE)

aggregate(long2$NRapproval ~long2$konkordanz2,FUN=mean, na.rm = TRUE)


####robustness 2: NR approval instead of konkordanz

M1 <- lmer (bet ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1 | canton),data=long2)
M2 <- lmer (bet ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1+govtminister | canton),data=long2)
M3 <- lmer (bet ~ rechtsform + period + NRapproval + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long2)
M4 <- lmer (bet ~ rechtsform + period + NRapproval + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long2)

tab_model(M1,M2,M3,M4,file="tabA2-NR.htm",show.aic=T)

check_collinearity(M1)


M1 <- lmer (diffpart ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1 | canton),data=long2)
M2 <- lmer (diffpart ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1+govtminister | canton),data=long2)
M3 <- lmer (diffpart ~ rechtsform + period + NRapproval + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long2)
M4 <- lmer (diffpart ~ rechtsform + period + NRapproval + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long2)

tab_model(M1,M2,M3,M4,file="tabA3-NR.htm",show.aic=T)

M1 <- glmer (official ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1 | canton), family = binomial("logit"), data=long2)
M2 <- glmer (official ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M3 <- glmer (official ~ rechtsform + period + NRapproval + govtminister*language + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M4 <- glmer (official ~ rechtsform + period + NRapproval + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)

tab_model(M1,M2,M3,M4,file="tabA4-NR.htm",show.aic=T)

M1 <- glmer (overruled ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1 | canton), family = binomial("logit"), data=long2)
M2 <- glmer (overruled ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M3 <- glmer (overruled ~ rechtsform + period + NRapproval + govtminister*language + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)
M4 <- glmer (overruled ~ rechtsform + period + NRapproval + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long2)

tab_model(M1,M2,M3,M4,file="tabA5-NR.htm",show.aic=T)


M1 <- lmer (diff ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1 | canton), data=long2)
M2 <- lmer (diff ~ govtminister + rechtsform + period + NRapproval + (1 | vote) + (1+govtminister | canton),data=long2)
M3 <- lmer (diff ~ rechtsform + period + NRapproval + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long2)
M4 <- lmer (diff ~ rechtsform + period + NRapproval + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long2)

tab_model(M1,M2,M3,M4,file="tabA6-NR.htm",show.aic=T)


####robustness 3: without always excluded ones
long3 <- subset(long2, canton != "JU" & canton != "SH" & canton != "UR" & canton != "SZ" & canton != "NW")

M1 <- lmer (bet ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton),data=long3)
M2 <- lmer (bet ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1+govtminister | canton),data=long3)
M3 <- lmer (bet ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long3)
M4 <- lmer (bet ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long3)

tab_model(M1,M2,M3,M4,file="tabA2-excluded.htm",show.aic=T)

check_collinearity(M1)


M1 <- lmer (diffpart ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton),data=long3)
M2 <- lmer (diffpart ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1+govtminister | canton),data=long3)
M3 <- lmer (diffpart ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long3)
M4 <- lmer (diffpart ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long3)

tab_model(M1,M2,M3,M4,file="tabA3-excluded.htm",show.aic=T)

M1 <- glmer (official ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton), family = binomial("logit"), data=long3)
M2 <- glmer (official ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long3)
M3 <- glmer (official ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long3)
M4 <- glmer (official ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long3)

tab_model(M1,M2,M3,M4,file="tabA4-excluded.htm",show.aic=T)

M1 <- glmer (overruled ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton), family = binomial("logit"), data=long3)
M2 <- glmer (overruled ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long3)
M3 <- glmer (overruled ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long3)
M4 <- glmer (overruled ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton), family = binomial("logit"),data=long3)

tab_model(M1,M2,M3,M4,file="tabA5-excluded.htm",show.aic=T)

me4 <- ggpredict(M4, terms = c("sonderbund","govtminister"),type = "re",ci.lvl = 0.9)
plot(me4)+scale_colour_grey()


M1 <- lmer (diff ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1 | canton), data=long3)
M2 <- lmer (diff ~ govtminister + rechtsform + period + konkordanz2 + (1 | vote) + (1+govtminister | canton),data=long3)
M3 <- lmer (diff ~ rechtsform + period + konkordanz2 + govtminister*language + (1 | vote) + (1+govtminister | canton),data=long3)
M4 <- lmer (diff ~ rechtsform + period + konkordanz2 + govtminister*sonderbund + (1 | vote) + (1+govtminister | canton),data=long3)

tab_model(M1,M2,M3,M4,file="tabA6-excluded.htm",show.aic=T)

#Descriptives/Table A-1
cols <- c('govtminister',"bet","diffpart","overruled","official","diff","period","NRapproval",
          "rechtsform","year","konkordanz2","sonderbund","language")
table <- skimr::skim(long2[,cols])
table(long2$govtminister)
table(long2$period)

write.xlsx(table,"Descriptives.xlsx")
