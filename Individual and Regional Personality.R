##################################################################################################
##################################################################################################
############## The Impact of Individual and Regional Personality on Social Distancing ############
##################################################################################################
##################################################################################################

# Load libraries
library(dplyr)
library(lmerTest)
library(ggplot2)


###########################################
###### Analyses for United States #########
###########################################

# read data
US <- read.csv("US_countyadd.csv") # individual level survey responses
fips_pers <- read.csv("df_us_pers_fips.csv") # county personality and controls

# remove outliers in income and education
US$income <- as.numeric(as.character(US$income))
hist(US$income)
US$income[US$income >= 1000000] <- 1000000
US$educ[US$educ >= 10] <- 10

# merge with regional FIPS data and prevalence data

prevalence_us <- read.csv("us_prev_fips.csv")

# calculated days continuously from March 1st
prevalence_us$month <- as.numeric(as.character(lapply(strsplit(as.character(prevalence_us$date), "\\-"), "[", 2)))
prevalence_us$day <- as.numeric(as.character(lapply(strsplit(as.character(prevalence_us$date), "\\-"), "[", 3)))
prevalence_us <- subset(prevalence_us, month == 3 | month == 4)
prevalence_us$days_added <- 0
prevalence_us$days_added[prevalence_us$month == 4] <- 31

prevalence_us$days <- prevalence_us$day + prevalence_us$days_added
prevalence_us <- prevalence_us[c("county_fips", "days", "rate_day")]

US <- merge(prevalence_us, US, by = c("county_fips", "days"))


# Get descriptives
US_descriptives <- merge(US, fips_pers, by = "county_fips")
US_descriptives <- US_descriptives %>% dplyr::select(county_fips, socdist, ope, con, ext, agr, neu, age.x, age.y, income, educ, hhmember, gender, days, rate_day, pers_o, pers_c, pers_e, pers_a, pers_n, airport_dist, conservative,  male, popdens, manufact, tourism, academics, medinc, healthcare) 
US_descriptives <- na.omit(US_descriptives)

round(mean(US_descriptives$ope, na.rm = T), digits = 2)
round(sd(US_descriptives$ope, na.rm = T), digits = 2)
round(mean(US_descriptives$con, na.rm = T), digits = 2)
round(sd(US_descriptives$con, na.rm = T), digits = 2)
round(mean(US_descriptives$ext, na.rm = T), digits = 2)
round(sd(US_descriptives$ext, na.rm = T), digits = 2)
round(mean(US_descriptives$agr, na.rm = T), digits = 2)
round(sd(US_descriptives$agr, na.rm = T), digits = 2)
round(mean(US_descriptives$neu, na.rm = T), digits = 2)
round(sd(US_descriptives$neu, na.rm = T), digits = 2)

round(mean(US_descriptives$age.x, na.rm = T), digits = 2)
round(sd(US_descriptives$age.x, na.rm = T), digits = 2)
table(US_descriptives$gender)
round(mean(US_descriptives$educ, na.rm = T), digits = 2)
round(sd(US_descriptives$educ, na.rm = T), digits = 2)
round(mean(US_descriptives$income, na.rm = T), digits = 2)
round(sd(US_descriptives$income, na.rm = T), digits = 2)
round(mean(US_descriptives$hhmember, na.rm = T), digits = 2)
round(sd(US_descriptives$hhmember, na.rm = T), digits = 2)

# calculate correlations
US_correlations <- round(cor(US_descriptives[,c("socdist", "ope", "pers_o", "con", "pers_c", "ext", "pers_e", "agr", "pers_a",  "neu", "pers_n", "age.x", "age.y", "gender", "male", "conservative", "educ", "academics", "income", "medinc", "manufact", "airport_dist", "tourism", "healthcare", "hhmember", "popdens", "days", "rate_day")]), digits = 3)
write.csv(US_correlations, file = "US_correlations.csv")


# For the main analyses scale the datasets separately and then merge them 

US_scaled <- US %>% dplyr::select(county_fips, socdist, ope, con, ext, agr, neu, age, income, educ, hhmember, gender, days, rate_day) %>%
  mutate_at(vars(-gender, -county_fips, - days), scale)

fips_scaled <- fips_pers %>% dplyr::select(county_fips, pers_o, pers_c, pers_e, pers_a, pers_n, airport_dist, conservative, age, male, popdens, manufact, tourism, academics, medinc, healthcare) %>%
  mutate_at(vars(-county_fips),scale)

US_scaled <- merge(US_scaled, fips_scaled, by = "county_fips")




### Multilevel Models ###

summary(m1 <- lmer(socdist ~ ope + pers_o + days + rate_day + (ope + pers_o|county_fips), data = US_scaled))
summary(m2 <- lmer(socdist ~ ope + pers_o + age.x + gender+ conservative + age.y + male +days + rate_day + (ope + pers_o|county_fips), data = US_scaled))
summary(m3 <- lmer(socdist ~ ope + pers_o +  educ + income  + manufact +  academics + medinc +  days + rate_day + (ope + pers_o|county_fips), data = US_scaled))
summary(m4 <- lmer(socdist ~ ope + pers_o + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (ope + pers_o|county_fips), data = US_scaled))
summary(m5 <- lmer(socdist ~ ope + pers_o + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (ope + pers_o|county_fips), data = US_scaled))
coefs_US_O <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_US_O$outcome <- "O"



summary(m1 <- lmer(socdist ~ con + pers_c + days + rate_day + (con + pers_c|county_fips), data = US_scaled))
summary(m2 <- lmer(socdist ~ con + pers_c + age.x + gender + conservative + age.y + male +days + rate_day + (con + pers_c|county_fips), data = US_scaled))
summary(m3 <- lmer(socdist ~ con + pers_c +  educ + income  + manufact +  academics + medinc +  days + rate_day + (con + pers_c|county_fips), data = US_scaled))
summary(m4 <- lmer(socdist ~ con + pers_c + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (con + pers_c|county_fips), data = US_scaled))
summary(m5 <- lmer(socdist ~ con + pers_c + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (con + pers_c|county_fips), data = US_scaled))
coefs_US_C <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_US_C$outcome <- "C"

summary(m1 <- lmer(socdist ~ ext + pers_e + days + rate_day + (ext + pers_e|county_fips), data = US_scaled))
summary(m2 <- lmer(socdist ~ ext + pers_e + age.x + gender + conservative + age.y + male +days + rate_day + (ext + pers_e|county_fips), data = US_scaled))
summary(m3 <- lmer(socdist ~ ext + pers_e +  educ + income  + manufact +  academics + medinc +  days + rate_day + (ext + pers_e|county_fips), data = US_scaled))
summary(m4 <- lmer(socdist ~ ext + pers_e + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (ext + pers_e|county_fips), data = US_scaled))
summary(m5 <- lmer(socdist ~ ext + pers_e + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (ext + pers_e|county_fips), data = US_scaled))
coefs_US_E <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_US_E$outcome <- "E"

summary(m1 <- lmer(socdist ~ agr + pers_a + days+ rate_day + (agr + pers_a|county_fips), data = US_scaled))
summary(m2 <- lmer(socdist ~ agr + pers_a + age.x + gender + conservative + age.y + male +days + rate_day + (agr + pers_a|county_fips), data = US_scaled))
summary(m3 <- lmer(socdist ~ agr + pers_a +  educ + income  + manufact +  academics + medinc +  days + rate_day + (agr + pers_a|county_fips), data = US_scaled))
summary(m4 <- lmer(socdist ~ agr + pers_a + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (agr + pers_a|county_fips), data = US_scaled))
summary(m5 <- lmer(socdist ~ agr + pers_a + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (agr + pers_a|county_fips), data = US_scaled))
coefs_US_A <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_US_A$outcome <- "A"


summary(m1 <- lmer(socdist ~ neu + pers_n + days+ rate_day +(neu + pers_n|county_fips), data = US_scaled))
summary(m2 <- lmer(socdist ~ neu + pers_n + age.x + gender + conservative + age.y + male +days + rate_day + (neu + pers_n|county_fips), data = US_scaled))
summary(m3 <- lmer(socdist ~ neu + pers_n +  educ + income  + manufact +  academics + medinc +  days + rate_day + (neu + pers_n|county_fips), data = US_scaled))
summary(m4 <- lmer(socdist ~ neu + pers_n + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (neu + pers_n|county_fips), data = US_scaled))
summary(m5 <- lmer(socdist ~ neu + pers_n + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (neu + pers_n|county_fips), data = US_scaled))
coefs_US_N <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_US_N$outcome <- "N"

all_US_coefs_multi <- as.data.frame(rbind(coefs_US_O, coefs_US_C, coefs_US_E, coefs_US_A, coefs_US_N))
all_US_coefs_multi$country <- "United States"





###########################################
###### Analyses for Germany ###############
###########################################

# read data
Germany <- read.csv("Germany_kreisadd.csv") # individual level survey responses
kreis_pers <- read.csv("df_ger_pers_kreis.csv") # kreis personality and controls


# remove outliers in income and education
Germany$income <- as.numeric(as.character(Germany$income))
hist(Germany$income)
Germany$income[Germany$income >= 1000000] <- 1000000
Germany$educ[Germany$educ >= 10] <- 10

# merge with regional kreis and prevalence data

prevalence_ger <- read.csv("ger_prev_kreis.csv")

# calculated days continuously from March 1st
prevalence_ger$month <- as.numeric(as.character(lapply(strsplit(as.character(prevalence_ger$date), "\\-"), "[", 2)))
prevalence_ger$day <- as.numeric(as.character(lapply(strsplit(as.character(prevalence_ger$date), "\\-"), "[", 3)))
prevalence_ger <- subset(prevalence_ger, month == 3 | month == 4)
prevalence_ger$days_added <- 0
prevalence_ger$days_added[prevalence_ger$month == 4] <- 31

prevalence_ger$days <- prevalence_ger$day + prevalence_ger$days_added
prevalence_ger <- prevalence_ger[c("kreis", "days", "rate_day")]


Germany <- merge(prevalence_ger, Germany, by = c("kreis", "days"))


# Get descriptives
Germany_descriptives <- merge(Germany, kreis_pers, by = "kreis")
head(Germany_descriptives)

Germany_descriptives <- Germany_descriptives %>% dplyr::select(kreis, socdist, ope, con, ext, agr, neu, age.x, age.y, income, educ, hhmember, gender, days, rate_day, pers_o, pers_c, pers_e, pers_a, pers_n, airport_dist, conservative,  male, popdens, manufact, tourism, academics, medinc, healthcare) 
Germany_descriptives <- na.omit(Germany_descriptives)

round(mean(Germany_descriptives$ope, na.rm = T), digits = 2)
round(sd(Germany_descriptives$ope, na.rm = T), digits = 2)
round(mean(Germany_descriptives$con, na.rm = T), digits = 2)
round(sd(Germany_descriptives$con, na.rm = T), digits = 2)
round(mean(Germany_descriptives$ext, na.rm = T), digits = 2)
round(sd(Germany_descriptives$ext, na.rm = T), digits = 2)
round(mean(Germany_descriptives$agr, na.rm = T), digits = 2)
round(sd(Germany_descriptives$agr, na.rm = T), digits = 2)
round(mean(Germany_descriptives$neu, na.rm = T), digits = 2)
round(sd(Germany_descriptives$neu, na.rm = T), digits = 2)

round(mean(Germany_descriptives$age.x, na.rm = T), digits = 2)
round(sd(Germany_descriptives$age.x, na.rm = T), digits = 2)
table(Germany_descriptives$gender)
round(mean(Germany_descriptives$educ, na.rm = T), digits = 2)
round(sd(Germany_descriptives$educ, na.rm = T), digits = 2)
round(mean(Germany_descriptives$income, na.rm = T), digits = 2)
round(sd(Germany_descriptives$income, na.rm = T), digits = 2)
round(mean(Germany_descriptives$hhmember, na.rm = T), digits = 2)
round(sd(Germany_descriptives$hhmember, na.rm = T), digits = 2)


# Calculate correlations
Germany_correlations <- round(cor(Germany_descriptives[,c("socdist", "ope", "pers_o", "con", "pers_c", "ext", "pers_e", "agr", "pers_a",  "neu", "pers_n", "age.x", "age.y", "gender", "male", "conservative", "educ", "academics", "income", "medinc", "manufact", "airport_dist", "tourism", "healthcare", "hhmember", "popdens", "days", "rate_day")]), digits = 3)
write.csv(Germany_correlations, file = "Germany_correlations.csv")


# For the main analyses scale the datasets separately and then merge them 

Germany_scaled <- Germany %>% dplyr::select(kreis, socdist, ope, con, ext, agr, neu, age, income, educ, hhmember, gender, days, rate_day) %>%
  mutate_at(vars(-kreis, -gender, - days), scale)

kreis_scaled <- kreis_pers %>% dplyr::select(kreis, pers_o, pers_c, pers_e, pers_a, pers_n, airport_dist, conservative, age, male, popdens, manufact, tourism, healthcare, medinc, academics) %>%
  mutate_at(vars(-kreis), scale)

Germany_scaled <- merge(Germany_scaled, kreis_scaled, by = "kreis")


### Multilevel Models ###

summary(m1 <- lmer(socdist ~ ope + pers_o + days + rate_day + (ope + pers_o|kreis), data = Germany_scaled))
summary(m2 <- lmer(socdist ~ ope + pers_o + age.x + gender+ conservative + age.y + male +days + rate_day + (ope + pers_o|kreis), data = Germany_scaled))
summary(m3 <- lmer(socdist ~ ope + pers_o +  educ + income  + manufact +  academics + medinc +  days + rate_day + (ope + pers_o|kreis), data = Germany_scaled))
summary(m4 <- lmer(socdist ~ ope + pers_o + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (ope + pers_o|kreis), data = Germany_scaled))
summary(m5 <- lmer(socdist ~ ope + pers_o + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (ope + pers_o|kreis), data = Germany_scaled))
coefs_GER_O <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_GER_O$outcome <- "O"


summary(m1 <- lmer(socdist ~ con + pers_c + days + rate_day + (con + pers_c|kreis), data = Germany_scaled))
summary(m2 <- lmer(socdist ~ con + pers_c + age.x + gender + conservative + age.y + male +days + rate_day + (con + pers_c|kreis), data = Germany_scaled))
summary(m3 <- lmer(socdist ~ con + pers_c +  educ + income  + manufact +  academics + medinc +  days + rate_day + (con + pers_c|kreis), data = Germany_scaled))
summary(m4 <- lmer(socdist ~ con + pers_c + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (con + pers_c|kreis), data = Germany_scaled))
summary(m5 <- lmer(socdist ~ con + pers_c + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (con + pers_c|kreis), data = Germany_scaled))
coefs_GER_C <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_GER_C$outcome <- "C"

summary(m1 <- lmer(socdist ~ ext + pers_e + days + rate_day + (ext + pers_e|kreis), data = Germany_scaled))
summary(m2 <- lmer(socdist ~ ext + pers_e + age.x + gender + conservative + age.y + male +days + rate_day + (ext + pers_e|kreis), data = Germany_scaled))
summary(m3 <- lmer(socdist ~ ext + pers_e +  educ + income  + manufact +  academics + medinc +  days + rate_day + (ext + pers_e|kreis), data = Germany_scaled))
summary(m4 <- lmer(socdist ~ ext + pers_e + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (ext + pers_e|kreis), data = Germany_scaled))
summary(m5 <- lmer(socdist ~ ext + pers_e + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (ext + pers_e|kreis), data = Germany_scaled))
coefs_GER_E <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_GER_E$outcome <- "E"

summary(m1 <- lmer(socdist ~ agr + pers_a + days+ rate_day + (agr + pers_a|kreis), data = Germany_scaled))
summary(m2 <- lmer(socdist ~ agr + pers_a + age.x + gender + conservative + age.y + male +days + rate_day + (agr + pers_a|kreis), data = Germany_scaled))
summary(m3 <- lmer(socdist ~ agr + pers_a +  educ + income  + manufact +  academics + medinc +  days + rate_day + (agr + pers_a|kreis), data = Germany_scaled))
summary(m4 <- lmer(socdist ~ agr + pers_a + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (agr + pers_a|kreis), data = Germany_scaled))
summary(m5 <- lmer(socdist ~ agr + pers_a + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (agr + pers_a|kreis), data = Germany_scaled))
coefs_GER_A <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_GER_A$outcome <- "A"

summary(m1 <- lmer(socdist ~ neu + pers_n + days+ rate_day +(neu + pers_n|kreis), data = Germany_scaled))
summary(m2 <- lmer(socdist ~ neu + pers_n + age.x + gender + conservative + age.y + male +days + rate_day + (neu + pers_n|kreis), data = Germany_scaled))
summary(m3 <- lmer(socdist ~ neu + pers_n +  educ + income  + manufact +  academics + medinc +  days + rate_day + (neu + pers_n|kreis), data = Germany_scaled))
summary(m4 <- lmer(socdist ~ neu + pers_n + + hhmember + airport_dist + popdens + tourism + healthcare + days + rate_day + (neu + pers_n|kreis), data = Germany_scaled))
summary(m5 <- lmer(socdist ~ neu + pers_n + age.x + gender + educ + income + hhmember + airport_dist + conservative + age.y + male + popdens + manufact + tourism + academics + medinc + healthcare +days + rate_day + (neu + pers_n|kreis), data = Germany_scaled))
coefs_GER_N <- as.data.frame(rbind(summary(m1)$coefficients, summary(m2)$coefficients, summary(m3)$coefficients, summary(m4)$coefficients, summary(m5)$coefficients))
coefs_GER_N$outcome <- "N"

all_Germany_coefs_multi <- as.data.frame(rbind(coefs_GER_O, coefs_GER_C, coefs_GER_E, coefs_GER_A, coefs_GER_N))
all_Germany_coefs_multi$country <- "Germany"



### Combine all coefficients and only keep thos that are relevant

all_coefs <- as.data.frame(rbind(all_US_coefs_multi, all_Germany_coefs_multi))
head(all_coefs)
all_coefs$predictor <- rownames(all_coefs)
all_coefs$predictor <- tolower(all_coefs$predictor)
all_coefs$predictor <- gsub('[0-9]+', '', all_coefs$predictor)
all_coefs$predictor <- gsub('[[:punct:] ]+','', all_coefs$predictor)


keep <- c("ope", "con", "ext", "agr", "neu", "perso", "persc", "perse", "persa", "persn")
options(scipen = 999)

all_coefs <- all_coefs[all_coefs$predictor %in% keep,]
all_coefs$trait <- rep(c(rep("OP", times = 10), rep("CO", times = 10), rep("EX", times = 10), rep("AG", times = 10), rep("NE", times = 10)), times = 2)
all_coefs$model <- rep(c(rep("M1", times = 2),rep("M2", times = 2), rep("M3", times = 2), rep("M4", times = 2), rep("M5", times = 2)),times = 10)
all_coefs$ind_reg <- rep(c("IP", "RP"), times = 50)

# M1 = no controls, M2 = sociodemographic, M3 = economic, M4 = pandemic, M5 = all

all_coefs$model <- factor(all_coefs$model, ordered = TRUE, levels = c("M5", "M3", "M2", "M4", "M1"))
all_coefs$trait <- factor(all_coefs$trait, ordered = TRUE, levels = c("OP", "CO", "EX", "AG", "NE"), labels = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism"))
all_coefs$country <- factor(all_coefs$country, ordered = TRUE, levels = c("United States", "Germany"))


colors <- c("#faf3dd","#c8d5b9",  "#8fc0a9","#68b0ab", "#ce4256") 


# plot

ggplot(all_coefs, aes(x = ind_reg, y = Estimate, fill = model)) +
  geom_bar(stat="identity", color="white", position=position_dodge(), width = 0.9) +
  geom_errorbar(aes(ymin=Estimate - `Std. Error` , ymax=Estimate + `Std. Error`), width=.2, 
                position=position_dodge(.9), colour = "black") +
  scale_fill_manual(values = colors)+
  geom_hline(yintercept=0) +
  facet_grid(trait ~ country)+
  theme_bw() + 
  coord_flip()+
  ylab("") +
  xlab("")+
  theme(legend.position="top") +
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=12)) +
  theme(strip.text.x = element_text(size = 12))+
  theme(axis.text=element_text(size=11))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())



# Correlation Table

correlations_US <- cor(US_scaled[c("socdist", "ope", "con", "ext", "agr", "neu", "pers_o", "pers_c", "pers_e", "pers_a", "pers_n", "age", "male", "educ", "income", "hhmember", "age", "academics", "male", "healthcare", "manufact",  "airport_dist", "conservative",  "popdens", "tourism", "medinc", "days", "rate_day")], use = "complete.obs")
write.csv(correlations_US, file = "Correlations_US_IndividualLevel.csv")

correlations_Germany <- cor(Germany_scaled[c("socdist", "ope", "con", "ext", "agr", "neu", "pers_o", "pers_c", "pers_e", "pers_a", "pers_n", "age.x", "male", "educ", "income",  "hhmember", "age.y", "academics", "male", "healthcare", "manufact","airport", "conservative", "popdens", "tourism", "medinc", "days", "rate_day")], use = "complete.obs")
write.csv(correlations_Germany, file = "Correlations_Germany_IndividualLevel.csv")

