setwd("~/Dropbox/Sun's data/Local adaptation/new local adaptation result")

library(lme4)
library(car)
library(emmeans)
library(MASS)

##Mite preference experiment
#first choice experiment
data=read.csv("first_choice.csv")
model=glmer(num~sp*miteorigin+sex+bodysize+(1|bl)+(1|year),offset = log(sum),family=poisson,data=data)
Anova(model,type=3)

#post-hoc comparisons for choice among different beetle species for each mite population 
a=emmeans (model,  ~ sp|miteorigin, adjust="tukey")
pairs(a)

#post-hoc comparisons for strength between populations for each different beetle species
a=emmeans (model,  ~ miteorigin|sp, adjust="tukey")
pairs(a)

#second choice experiment
data=read.csv("second_choice.csv")
#specify specific mite species by behaviours to their new corresponding beetle species
data=data[data$newcomp=="1",]

model=glmer(num~sp*miteorigin+bodysize+sex+(1|bl)+(1|year),offset = log(sum),family=poisson,data=data)
Anova(model,type=3)

#post-hoc comparisons for different mite species within the same mite population  
a=emmeans (model,  ~ sp | miteorigin, adjust="tukey")
pairs(a)

####Experimental manipulation of mite####
data=read.csv("g_mite.csv")

#brood mass
model=glm(log(broodmass+1)~tr+wt,gaussian,data=data)
Anova(model,type=3)

#post-hoc comparisons for different mite treatment 
a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)


#average larval mass
model=glm(log(yield)~tr+larvaldensity,gaussian,data=data)
Anova(model,type=3)

#post-hoc comparisons for different mite treatment 
a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)

#number of mite deutonymphs
model=glm.nb(mitenum~tr+wt,data=data)
Anova(model,type=3)

####Local adaptation experiment####
data=read.csv("local_adaptation_data.csv")
#brood mass
model=glmer(log(broodmass)~mite*beetle+wt+(1|bl),family="gaussian",data=data)
Anova(model,type=3)

#post-hoc comparisons for different mite treatment within the same beetle treatment 
a=emmeans (model,  ~ mite | beetle, adjust="tukey")
pairs(a)

#post-hoc comparisons for different beetle treatment within the same mite treatment 
a=emmeans (model,  ~ beetle | mite, adjust="tukey")
pairs(a)

#avaerage larval mass
model=glmer(log(yield)~mite*beetle+density+(1|bl),family="gaussian",data=data)
Anova(model,type=3)

#post-hoc comparisons for different mite treatment within the same beetle treatment 
a=emmeans (model,  ~ mite | beetle, adjust="tukey")
pairs(a)

#post-hoc comparisons for different beetle treatment within the same mite treatment 
a=emmeans (model,  ~ beetle | mite, adjust="tukey")
pairs(a)

#number of mite deutonymphs
model=glmer.nb((mitenum)~mite*beetle+wt+(1|bl),data=data)

#post-hoc comparisons for different mite treatment within the same beetle treatment 
a=emmeans (model,  ~ mite | beetle, adjust="tukey")
pairs(a)

#post-hoc comparisons for different beetle treatment within the same mite treatment 
a=emmeans (model,  ~ beetle | mite, adjust="tukey")
pairs(a)