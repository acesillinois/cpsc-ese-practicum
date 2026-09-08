library(plyr); library(abind); library(permute); library(lme4); 
library(R2WinBUGS); library(coda);library(vegan); library(arm); 
library(lattice); library(sciplot); library(reshape); library(cluster);
library(nortest); library(ggplot2);library(nlme); library(lme4);
library(scales); library(labdsv); library(stats); library(glmmML); 
library(MuMIn); library(effects); library(car); library(MASS); 
library(tidyverse); library(rsq); library(arm); library(plotrix); library(base);
library(sciplot); library(simr); library(readxl); library(emmeans);
library(multcomp);library(ggpubr); library(DHARMa); library(indicspecies);
library(readxl); library(glmmTMB); library(AICcmodavg);
library(devtools); library(grid); library(ggbiplot); library(forcats);
library(geosphere); library(mgcv); library(maps); library(ade4);
library(bipartite); library(metafor); library(SciViews); library(grDevices); 
library(DataCombine); library(report);


### Set wd
setwd("/Users/32495/Dropbox/Werk/2022_Blueberry_HBlog")
setwd("D:/Users/meeraert/Dropbox/Werk/2022_Blueberry_HBlog")



###
### RQ1: pollination deficits for berry weight, in relation to cultivar
###



### Import dataset
data <- read_excel("BBcul_final.xlsx")
attach(data)
str(data)

# Remove BW NA's
# Specify variables/columns where you want to check for NA values
variables_with_na <- c('Data_ID', 'Study_ID', 'Cultivar', 'Location_ID', 'BW_open', 'BW_supp', 'HB', 'WB', 'Richness')
# Omit rows with NA values in specified columns
data2 <- data[complete.cases(data[, variables_with_na]), ]
attach(data2)
str(data2)

# subset desired data -
data2 = data.frame(Data_ID, Study_ID, BW_open, BW_supp)
attach(data2)
str(data2)

# Wide to long
data3 <- pivot_longer(data2, cols = "BW_open":"BW_supp", names_to = "BW_treat",
                      values_to = "BW", values_drop_na = TRUE)
attach(data3)
str(data3)
dotchart(BW)

# Standardize data between zero and one per study
data4 <- data3 %>%
  group_by(Study_ID)%>%
  mutate(zBW=(BW-min(BW))/(max(BW)-min(BW)))
attach(data4)
str(data4)
dotchart(zBW)

# long to wide
data5 = data.frame(Data_ID, BW_treat, zBW)
attach(data5)
str(data5)
data6 <- pivot_wider(data = data5, id_cols = c("Data_ID"), 
                     names_from = "BW_treat", values_from = "zBW")
attach(data6)
str(data6)

### Link datasets with each other
data_1 = data1 %>% select(Data_ID, Study_ID, Cultivar, Location_ID, HB, WB, Richness)
data_2 = data6 %>% select(Data_ID, BW_open, BW_supp)
# Join the datasets 
merged_data <- full_join(data_1, data_2, by="Data_ID")
attach(merged_data)
str(merged_data)

### Calculate pollen limitation
merged_data <- merged_data %>%
  mutate(zBW = (BW_supp-BW_open))
attach(merged_data)
str(merged_data)
dotchart(zBW)

### Determine mean and SE per cultivar
merged_data2 <- merged_data %>%  
  select(Cultivar, zBW) %>% 
  group_by(Cultivar) %>% 
  summarise(across(zBW, .fns = se))

### Run pollen limitation model
# select variables
# response
BW_lim = merged_data$zBW
# fixed and random
cultivar = merged_data$Cultivar
study = merged_data$Study_ID
location = merged_data$Location_ID

# Check outliers of response
dotchart(BW_lim)
boxplot(BW_lim)
histogram(BW_lim)

# Run model
m1 = lme(BW_lim ~ -1 + cultivar, random=~1|study/location)
#m1 = lmer(BW_lim ~ -1 + cultivar + (1|study/location))
# extract estimate, SE and t-value
summary(m1)
anova.lme(m1)

# Validate model output
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))
lillie.test(resid(m1))

# Plot results
# Determine mean and SE per cultivar
merged_data2 <- merged_data %>%  
  select(Cultivar, zBW) %>% 
  group_by(Cultivar) %>% 
  summarise(across(zBW, .fns = se))

Cultivar2 = c("Bluecrop", "Duke")
mean = c(0.06211485, 0.05646901)
se = c(0.06713825, 0.06973229)
my_cis = data.frame(Cultivar2, mean, se)

culcol = c("darkred", "darkblue")

ggplot(data=data1, aes(x = Cultivar, col = Cultivar)) +
  geom_jitter(aes(y=BW_lim), size = 1, alpha = 0.25,height=0.01, width=0.1)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), data = my_cis, width = 0.015, color = c(culcol)) +
  geom_point(aes(y = mean), data = my_cis, color = c(culcol), size = 1.5)+
  geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_discrete("Cultivar",labels = c("Bluecrop", "Duke")) +
  scale_x_discrete("Berry weight deficit")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave("RQ1_BWdeficit.png", width = 3, height = 3, dpi = 300)





###
### RQ2: pollinadtion deficit berry weight ~ bee visitation
###

# Standardize data of fixed variables between zero and one per study
merged_data2 <- merged_data %>%
  group_by(Study_ID)%>%
  mutate(zHB=(HB-min(HB))/(max(HB)-min(HB)))%>%
  mutate(zWB=(WB-min(WB))/(max(WB)-min(WB)))%>%
  mutate(zRich=(Richness-min(Richness))/(max(Richness)-min(Richness)))
attach(merged_data2)
str(merged_data2)


### Wild bee and honey bee visitation model

# Select data per cultivar
# Cultivar is either 'Duke' or 'Bluecrop'
merged_data3 = subset(merged_data2, merged_data2$Cultivar == "Duke")
attach(merged_data3)
str(merged_data3)

### Run models
# Variables
testvar = merged_data3$zBW
var = (merged_data3$zHB + merged_data3$zWB)
study = merged_data3$Study_ID
location = merged_data3$Location_ID

# Check outliers of response
dotchart(testvar)
boxplot(testvar)
histogram(testvar)

# Run models
m1 = lme(testvar ~ 1, random=~1|study/location)
m2 = lme(testvar ~ var, random=~1|study/location)
m3 = lme(testvar ~ exp(-var), random=~1|study/location)

# Compare model performance by AIC
Cand.models <- list(m1, m2, m3)
Modnames <- paste("mod", 1:length(Cand.models), sep = " ")
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = FALSE))

# best model
m = m3
summary(m)
anova.lme(m)

#Model validation
plot(m)
qqnorm(resid(m))
qqline(resid(m))
lillie.test(resid(m))

#Figure
m = lme(testvar ~ exp(-var), random=~1|study/location)
Predicttestvar<-as.data.frame(effect("exp(-var)",m,xlevels=50))
ggplot(data=merged_data3, aes(x=var))+
  geom_ribbon(data=Predicttestvar, aes(ymin=lower, ymax=upper), fill="grey65",alpha=0.4)+
  geom_line(data=Predicttestvar, aes(x=var, y=fit),color="black", alpha = 1)+
  geom_point(aes(y=testvar), col = "black", size = 1, alpha = 0.25)+
  geom_hline(yintercept=0, linetype = "dashed")+
  xlab("Total bee visitation")+
  ylab("Berry weight deficit")+
  labs(subtitle="Bluecrop")+
  guides(color = FALSE, size = FALSE)+
  theme_classic()
ggsave("RQ2_BW_BC.png", width = 3.75, height = 3.75, dpi = 300)



### Richness model

# Select data per cultivar
# Cultivar is either 'Duke' or 'Bluecrop'
merged_data3 = subset(merged_data2, merged_data2$Cultivar == "Duke")
attach(merged_data3)
str(merged_data3)

### Run models
# Select variables
testvar = merged_data3$zBW
Rich = merged_data3$zRich
study = merged_data3$Study_ID
location = merged_data3$Location_ID

# Check outliers of response
dotchart(testvar)
boxplot(testvar)
histogram(testvar)

# Run models
m1 = lme(testvar ~ 1, random=~1|study/location)
m2 = lme(testvar ~ Rich, random=~1|study/location)
m3 = lme(testvar ~ exp(-Rich), random=~1|study/location)

# Compare model performance by AIC
Cand.models <- list(m1, m2, m3)
Modnames <- paste("mod", 1:length(Cand.models), sep = " ")
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE))

# best model
m = m3
summary(m)
anova.lme(m)

#Model validation
plot(m)
qqnorm(resid(m))
qqline(resid(m))
lillie.test(resid(m))

#Figure
# No figure for richness

