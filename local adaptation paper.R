setwd("~/Dropbox/Sun's data/Local adaptation/new local adaptation result")
setwd("/Users/syuan-jyunsun/Library/CloudStorage/Dropbox/Sun's data/Local adaptation/new local adaptation result")

library(lme4)
library(car)
library(emmeans)
library(MASS)
library(ggplot2)
library(dplyr)
library(ggpubr)

##Mite preference experiment
#first choice experiment
data=read.csv("first_choice.csv")
data$sp <- factor(data$sp, levels=c("ves", "hum", "int", "inv"))

model=glmer(num~sp*miteorigin+sex+bodysize+(1|bl)+(1|year),offset = log(sum),family=poisson,data=data)
Anova(model,type=3)

model=glmer(num~sp+sex+bodysize+(1|bl)+(1|year),offset = log(sum),family=poisson,data=data[data$miteorigin=="G",])
Anova(model,type=3)

model=glmer(num~sp+sex+bodysize+(1|bl)+(1|year),offset = log(sum),family=poisson,data=data[data$miteorigin=="W",])
Anova(model,type=3)

#post-hoc comparisons for choice among different beetle species for each mite population 
a=emmeans (model,  ~ sp|miteorigin, adjust="tukey")
pairs(a)

#post-hoc comparisons for strength between populations for each different beetle species
a=emmeans (model,  ~ miteorigin|sp, adjust="tukey")
pairs(a)

summary_data <- data %>%
  group_by(miteorigin, sp) %>%
  summarise(
    mean_mites = mean(num/sum),
    se_mites = sd(num/sum)/sqrt(n())
  ) %>%
  ungroup()

print(summary_data)

Gen1=ggplot(summary_data, aes(x = sp, y = mean_mites, fill = miteorigin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),width =0.7) +
  geom_errorbar(aes(ymin = mean_mites - se_mites, ymax = mean_mites + se_mites), width = 0.25, position = position_dodge(width = 0.8)) +
  labs(title = "Generation 1")+
  labs(y = expression("Proportion of " * italic("P. carabi"))) +
  labs(x = "Woodland of origin")+ 
  scale_y_continuous(expand = c(0, 0),limits=c(0,1))+
  scale_fill_manual(values=c("G"="#479BD5", "W"="#E4191C"))+
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black", linewidth = 0.8) +
  theme_classic()+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")
Gen1

ggarrange(Gen1, Gen2, 
          labels = c("(a)", "(b)"),
          ncol = 1, nrow = 2)

#second choice experiment
data=read.csv("second_choice.csv")
#specify specific mite species by behaviours to their new corresponding beetle species
data=data[data$newcomp=="1",]
data$Species <- factor(data$Species, levels=c("ves", "hum", "int", "inv"))

model=glmer(num~sp*miteorigin+bodysize+sex+(1|bl)+(1|year),offset = log(sum),family=poisson,data=data)
Anova(model,type=3)

#post-hoc comparisons for different mite species within the same mite population  
a=emmeans (model,  ~ sp | miteorigin, adjust="tukey")
pairs(a)

summary_data <- data %>%
  group_by(miteorigin, Species) %>%
  summarise(
    mean_mites = mean(num/sum),
    se_mites = sd(num/sum)/sqrt(n())
  ) %>%
  ungroup()


print(summary_data)

Gen2=ggplot(summary_data, aes(x = Species, y = mean_mites, fill = miteorigin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),width =0.7) +
  geom_errorbar(aes(ymin = mean_mites - se_mites, ymax = mean_mites + se_mites), width = 0.25, position = position_dodge(width = 0.8)) +
  labs(title = "Generation 2")+
  labs(y = expression("Proportion of " * italic("P. carabi"))) +
  labs(x = "Woodland of origin")+ 
  scale_y_continuous(expand = c(0, 0),limits=c(0,1))+
  scale_fill_manual(values=c("G"="#479BD5", "W"="#E4191C"))+
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "black", linewidth = 0.8) +
  theme_classic()+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

Gen2
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

summary_data <- data %>%
  group_by(tr) %>%
  summarise(
    mean_broodmass = mean(broodmass),
    se_broodmass = sd(broodmass)/sqrt(n())
  ) 

print(summary_data)

new_labels <- c("control" = "no mites", "mix" = "mix", "ves" = "ves")

treatment_colors <- c("black", "#999999", "#E95210")
broodmass <- ggplot(data, aes(x = tr, y = broodmass,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 1, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_broodmass, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_broodmass - se_broodmass, ymax = mean_broodmass + se_broodmass, color = tr), width = 0.15) + 
  scale_x_discrete(labels=new_labels)+
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(title = "beetle reproductive success")+
  labs(y = "Brood mass (g)") +
  labs(x = "Mite treatments")+ 
  scale_y_continuous(limits=c(-0.01,8))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(broodmass)

data=data[is.na(data$yield)=="FALSE",]
summary_data <- data %>%
  group_by(tr) %>%
  summarise(
    mean_yield = mean(yield),
    se_yield = sd(yield)/sqrt(n())
  ) 

print(summary_data)

new_labels <- c("control" = "no mites", "mix" = "mix", "ves" = "ves")

treatment_colors <- c("black", "#999999", "#E95210")
yield <- ggplot(data, aes(x = tr, y = yield,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 1, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_yield, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_yield - se_yield, ymax = mean_yield + se_yield, color = tr), width = 0.15) + 
  scale_x_discrete(labels=new_labels)+
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(title = "beetle reproductive success")+
  labs(y = "Average larval mass (g)") +
  labs(x = "Mite treatments")+ 
  scale_y_continuous(limits=c(0.09,0.30))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(yield)

data=read.csv("g_mite.csv")
data=data[data$tr!="control",]
summary_data <- data %>%
  group_by(tr) %>%
  summarise(
    mean_mitenum = mean(mitenum),
    se_mitenum = sd(mitenum)/sqrt(n())
  ) 

print(summary_data)

new_labels <- c("mix" = "mix", "ves" = "ves")

treatment_colors <- c( "#999999", "#E95210")
mite <- ggplot(data, aes(x = tr, y = mitenum,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 1, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_mitenum, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_mitenum - se_mitenum, ymax = mean_mitenum + se_mitenum, color = tr), width = 0.15) + 
  scale_x_discrete(labels=new_labels)+
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(title = "mite reproductive success")+
  labs(y = "Number of mite deutonymphs") +
  labs(x = "Mite treatments")+ 
  scale_y_continuous(limits=c(0,300))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(mite)
 
ggarrange(broodmass, yield, mite,
          labels = c("(a)", "(b)","(c)"),
          ncol = 1, nrow = 3)

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

data<-data[data$beetle!="C",]
summary_data <- data %>%
  group_by(beetle,mite) %>%
  summarise(
    mean_broodmass = mean(broodmass),
    se_broodmass = sd(broodmass)/sqrt(n())
  ) 

print(summary_data)

new_labels <- c("C" = "no mites", "G" = "Gamlingay", "W" = "Waresley")

treatment_colors <- c("#479BD5", "#E4191C")

broodmass <- ggplot(data, aes(x = mite, y = broodmass,fill=beetle)) +
  geom_point(aes(color = beetle), position = position_dodge(width = 0.2), size = 1, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = mite, y = mean_broodmass, color = beetle), shape = 16, size = 4,position = position_dodge(width = 0.5)) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_broodmass - se_broodmass, ymax = mean_broodmass + se_broodmass, color = beetle), width = 0.15,position = position_dodge(width = 0.5)) + 
  geom_line(data = summary_data, aes(x = mite, y = mean_broodmass, group = beetle, color = beetle), position = position_dodge(width = 0.5)) + # Connecting lines
  scale_x_discrete(labels=new_labels)+
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(title = "beetle reproductive success")+
  labs(y = "Brood mass (g)") +
  labs(x = "Mite treatments")+ 
  scale_y_continuous(limits=c(-0.01,6),breaks=seq(0,6,by=1.5))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(broodmass)

data<-data[data$beetle!="C",]
summary_data <- data %>%
  group_by(beetle,mite) %>%
  summarise(
    mean_yield = mean(yield),
    se_yield = sd(yield)/sqrt(n())
  ) 

print(summary_data)

new_labels <- c("C" = "no mites", "G" = "Gamlingay", "W" = "Waresley")

treatment_colors <- c("#479BD5", "#E4191C")

yield <- ggplot(data, aes(x = mite, y = yield,fill=beetle)) +
  geom_point(aes(color = beetle), position = position_dodge(width = 0.2), size = 1, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = mite, y = mean_yield, color = beetle), shape = 16, size = 4,position = position_dodge(width = 0.5)) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_yield - se_yield, ymax = mean_yield + se_yield, color = beetle), width = 0.15,position = position_dodge(width = 0.5)) + 
  geom_line(data = summary_data, aes(x = mite, y = mean_yield, group = beetle, color = beetle), position = position_dodge(width = 0.5)) + # Connecting lines
  scale_x_discrete(labels=new_labels)+
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(title = "beetle reproductive success")+
  labs(y = "Average larval mass (g)") +
  labs(x = "Mite treatments")+ 
  scale_y_continuous(limits=c(0.1,0.25),breaks=seq(0.1,0.25,by=0.05))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(yield)

data=read.csv("local_adaptation_data.csv")
data<-data[data$mite!="C",]
summary_data <- data %>%
  group_by(beetle,mite) %>%
  summarise(
    mean_mitenum = mean(mitenum),
    se_mitenum = sd(mitenum)/sqrt(n())
  ) 

print(summary_data)

new_labels <- c("C" = "no beetles", "G" = "Gamlingay", "W" = "Waresley")

treatment_colors <- c("#479BD5", "#E4191C")

mite <- ggplot(data, aes(x = beetle, y = mitenum, fill=mite)) +
  geom_point(aes(color = mite), position = position_dodge(width = 0.2), size = 1, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = beetle, y = mean_mitenum, color = mite), shape = 16, size = 4,position = position_dodge(width = 0.5)) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_mitenum - se_mitenum, ymax = mean_mitenum + se_mitenum, color = mite), width = 0.15,position = position_dodge(width = 0.5)) + 
  geom_line(data = summary_data, aes(x = beetle, y = mean_mitenum, group = mite, color = mite), position = position_dodge(width = 0.5)) + # Connecting lines
  scale_x_discrete(labels=new_labels)+
  theme_classic() +
  scale_fill_manual(values = treatment_colors,labels=c('Gamlingay', 'Waresley')) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors,labels=c('Gamlingay', 'Waresley')) + 
  labs(title = "mite reproductive success")+
  labs(y = "Number of mite deutonymphs") +
  labs(x = "Beetle treatments")+ 
  scale_y_continuous(limits=c(0,500),breaks=seq(0,500,by=100))+
  #scale_fill_discrete(labels=c('Gamlingay', 'Waresley'))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position=c(0.8, 0.9),
    legend.title = element_blank())



print(mite)

ggarrange(broodmass, yield, mite,
          labels = c("(a)", "(b)","(c)"),
          ncol = 1, nrow = 3)

