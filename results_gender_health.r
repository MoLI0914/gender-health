##############################################################
##############################################################
################### Statistique Appliquée ####################
###################### ENSAE Paris  2A #######################
##############################################################
################ Contact: helene.wang@ensae.fr ###############
###################### mo.li@ensae.fr ########################
#################### yeling_tang@ensae.fr ####################
##################### +33 06 61 96 37 73 #####################
##############################################################
##############################################################


#### The environment ####
rm(list = ls())
setwd("D:/2019-2020 ENSAE Paris/2019-2020 2eme semestre/StatApp")

#### Pretreatments of the data ####

agms <- read.csv("AGMS.csv")

ord <- c("Not at all", "1", "2", "3", "4", "5", "Very")
ord <- factor(1:length(ord), labels = ord)

agms$firstord_fem <- factor(agms$firstord_fem, levels = levels(ord))
agms$firstord_masc <- factor(agms$firstord_masc, levels = levels(ord))
agms$thirdord_fem <- factor(agms$thirdord_fem, levels = levels(ord))
agms$thirdord_masc <- factor(agms$thirdord_masc, levels = levels(ord))







is.na(agms$fear_aqnt) <- agms$fear_aqnt == ""
is.na(agms$fear_ipv) <- agms$fear_ipv == ""
is.na(agms$fear_strn) <- agms$fear_strn == ""
is.na(agms$fear_murd) <- agms$fear_murd == ""
is.na(agms$fear_sa) <- agms$fear_sa == ""
ord_fear <- c("Not at all afraid", "1", "2", "3", "4", "5", "Very afraid")
ord_fear <- factor(1:length(ord_fear), labels = ord_fear)
agms$fear_sa <- factor(agms$fear_sa, levels = levels(ord_fear))
agms$fear_aqnt <- factor(agms$fear_aqnt, levels = levels(ord_fear))
agms$fear_ipv <- factor(agms$fear_ipv, levels = levels(ord_fear))
agms$fear_strn <- factor(agms$fear_strn, levels = levels(ord_fear))
agms$fear_murd <- factor(agms$fear_murd, levels = levels(ord_fear))

is.na(agms$natarmsy) <- agms$natarmsy == ""
is.na(agms$natenviy) <- agms$natenviy == ""
is.na(agms$natcrimy) <- agms$natcrimy == ""
is.na(agms$nateducy) <- agms$nateducy == ""
is.na(agms$natsci) <- agms$natsci == ""
is.na(agms$natfarey) <- agms$natfarey == ""
ord_spending <- c("Spending too little", "Spending the right amount", "Spending too much")
ord_spending <- factor(1:length(ord_spending), labels = ord_spending)
agms$natarmsy <- factor(agms$natarmsy , levels = levels(ord_spending))
agms$natenviy <- factor(agms$natenviy , levels = levels(ord_spending))
agms$natcrimy <- factor(agms$natcrimy , levels = levels(ord_spending))
agms$nateducy <- factor(agms$nateducy , levels = levels(ord_spending))
agms$natsci <- factor(agms$natsci , levels = levels(ord_spending))
agms$natfarey <- factor(agms$natfarey , levels = levels(ord_spending))





is.na(agms$sexornt) <- agms$sexornt == ""
ord_sexornt <- c("Bisexual", "Gay, lesbian, homosexual", "Heterosexual, straight")
ord_sexornt <- factor(1:length(ord_sexornt), labels = ord_sexornt)
agms$sexornt <- factor(agms$sexornt, levels = levels(ord_sexornt))

is.na(agms$sex) <- agms$sex == ""
ord_sex <- c("Female", "Male")
ord_sex <- factor(1:length(ord_sex), labels = ord_sex)
agms$sex <- factor(agms$sex, levels = levels(ord_sex))

is.na(agms$region_gss) <- agms$region_gss == ""
ord_region_gss <- c("Midwest", "Northeast", "South", "West")
ord_region_gss <- factor(1:length(ord_region_gss), labels = ord_region_gss)
agms$region_gss <- factor(agms$region_gss, levels = levels(ord_region_gss))

is.na(agms$prtypref) <- agms$prtypref == ""
ord_prtypref <- c("Democrat", "Independent", "Other", "Republican")
ord_prtypref <- factor(1:length(ord_prtypref), labels = ord_prtypref)
agms$prtypref <- factor(agms$prtypref, levels = levels(ord_prtypref))


is.na(agms$hispanic_binary) <- agms$hispanic_binary == ""
ord_hispanic <- c("no", "yes")
ord_hispanic <- factor(1:length(ord_hispanic), labels = ord_hispanic)
agms$hispanic_binary <- factor(agms$hispanic_binary, levels = levels(ord_hispanic))


ord_income06 <- c("<$1,000", "$1,000-2,999", "$3,000-3,999", "$4,000-4,999",
                   "$5,000-5,999", "$6,000-6,999", "$7,000-7,999", "$8,000-9,999",
                   "$10,000-12,499","$12,500-14,999","$15,000-17,499","$17,500-19,999"
                   ,"$20,000-22,499","$22,500-24,999","$25,000-29,999","$30,000-34,999",
                   "$35,000-39,999","$40,000-49,999","$50,000-59,999","$60,000-74,999",
                   "$75,000-89,999","$90,000-109,999","$110,000-129,999","$130,000-149,999",
                   ">$150,000")
ord_income06 <- factor(1:length(ord_income06), labels = ord_income06)
agms$income06 <- factor(agms$income06, levels = levels(ord_income06))



agms <- agms[(agms$surveytime >= 270) & (is.na(agms$age) == F) & (is.na(agms$inc_cont) == F) & 
               (is.na(agms$region_gss) == F) & (is.na(agms$thirdord_fem) == F) & (agms$cisgender == "yes"),]


agms$fem_num <- as.numeric(agms$firstord_fem) - 1
agms$third_fem_num <- as.numeric(agms$thirdord_fem) - 1
agms$masc_num <- as.numeric(agms$firstord_masc) - 1
agms$third_masc_num <- as.numeric(agms$thirdord_masc) - 1

ord_hea <- c("Poor", "Fair", "Good", "Excellent")
ord_hea <- factor(1:length(ord_hea), labels = ord_hea)
agms$health <- factor(agms$health, levels = levels(ord_hea))
agms$health_num <- as.numeric(agms$health)

ord_health <- c("fair/poor", "good", "excellent")
ord_health <- factor(1:length(ord_health), labels = ord_health)

agms$health_3cat <- factor(agms$health_3cat, levels = levels(ord_health))

agms$health_3cat <- as.numeric(agms$health_3cat)

agms$sexornt <- relevel(agms$sexornt, "Heterosexual, straight")
agms$race_gss <- relevel(agms$race_gss, "Another race(s)")
agms$region_gss <- relevel(agms$region_gss, "Midwest")
agms$ind_born <- agms$born == "no"

agms$married_num <- as.numeric(agms$married) - 1

ind_white <- (agms$race_detail == "White")
ind_black <- (agms$race_detail == "Black/AfAm")

#### Table 2, 3 and 4 ####


# Cisgender women

women <- agms[agms$sex == "Female",]

attach(women)

ind_noncomformy <- (masc_num > fem_num)
ind_reflected_noncomformy <- (third_masc_num > third_fem_num)

ind_white <- race_detail == "White"
ind_black <- race_detail == "Black/AfAm"
ind_race_else <- 1 - ind_white - ind_black
ind_health_poor <- health_num == 1
ind_health_fair <- health_num == 2
ind_health_good <- health_num == 3
ind_health_excellent <- health_num == 4

ind_south <- (region_gss == "South")
ind_west <- (region_gss == "West")
ind_midwest <- (region_gss == "Midwest")
ind_northeast <- (region_gss == "Northeast")

ind_Hp <- hispanic_binary == "yes"
ind_born_outside_us <- born == "no"

ind_gay_les_homo <- sexornt == "Gay, lesbian, homosexual"
ind_bisexual <- sexornt == "Bisexual"
ind_heter_straight <- sexornt == "Heterosexual, straight"

## Table 2

apply(cbind(fem_num, masc_num,third_fem_num, third_masc_num, age, ind_noncomformy, ind_reflected_noncomformy, educ, inc_cont), 2, mean)
apply(cbind(fem_num, masc_num,third_fem_num, third_masc_num, age, educ, inc_cont), 2, sd)

apply(cbind(ind_health_excellent, ind_health_good, ind_health_fair, ind_health_poor,
            ind_gay_les_homo, ind_bisexual, ind_heter_straight, married_num,
            ind_white, ind_black, ind_race_else, ind_Hp, ind_born_outside_us, ind_northeast, ind_south, ind_west, ind_midwest), 2, mean)
apply(cbind(ind_health_excellent, ind_health_good, ind_health_fair, ind_health_poor,
            ind_gay_les_homo, ind_bisexual, ind_heter_straight, married_num,
            ind_white, ind_black, ind_race_else, ind_Hp, ind_born_outside_us, ind_northeast, ind_south, ind_west, ind_midwest), 2, sum)

## Table 3

# Without controls for survey condition
model3_1a <- lm(health_3cat ~ fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_1a)
BIC(model3_1a)
AIC(model3_1a)

model3_2a <- lm(health_3cat ~ masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_2a)
BIC(model3_2a)
AIC(model3_2a)


model3_3a <- lm(health_3cat ~ fem_num + masc_num + fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_3a)
BIC(model3_3a)
AIC(model3_3a)

# With controls for survey condition

model3_1a <- lm(health_3cat ~ fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model3_1a)
BIC(model3_1a)
AIC(model3_1a)

model3_2a <- lm(health_3cat ~ masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first )
summary(model3_2a)
BIC(model3_2a)
AIC(model3_2a)


model3_3a <- lm(health_3cat ~ fem_num + masc_num + fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model3_3a)
BIC(model3_3a)
AIC(model3_3a)

## Table 4

# Without controls for survey condition

model4_1a <- lm(health_3cat ~ third_fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss)
summary(model4_1a)
BIC(model4_1a)
AIC(model4_1a)

model4_2a <- lm(health_3cat ~ third_fem_num + fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss)
summary(model4_2a)
BIC(model4_2a)
AIC(model4_2a)

# With controls for survey condition

model4_1a <- lm(health_3cat ~ third_fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
              + region_gss + module_end + scales_first + self_first)
summary(model4_1a)
BIC(model4_1a)
AIC(model4_1a)

model4_2a <- lm(health_3cat ~ third_fem_num + fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
               + region_gss + module_end + scales_first + self_first)
summary(model4_2a)
BIC(model4_2a)
AIC(model4_2a)

## Table 5

# Without controls for survey condition

model5_1a <- lm(health_3cat ~ ind_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss)
summary(model5_1a)
BIC(model5_1a)
AIC(model5_1a)


model5_2a <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss)
summary(model5_2a)
BIC(model5_2a)
AIC(model5_2a)

model5_3a <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy +ind_noncomformy*ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss)
summary(model5_3a)
BIC(model5_3a)
AIC(model5_3a)

# With controls for survey condition

model5_1a <- lm(health_3cat ~ ind_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_1a)
BIC(model5_1a)
AIC(model5_1a)

model5_2a <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_2a)
BIC(model5_2a)
AIC(model5_2a)

model5_3a <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy +ind_noncomformy*ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_3a)
BIC(model5_3a)
AIC(model5_3a)

detach(women)


# Cisgender men

men <- agms[agms$sex == "Male",]

attach(men)

ind_noncomformy <- (masc_num < fem_num)
ind_reflected_noncomformy <- (third_masc_num < third_fem_num)

ind_white <- race_detail == "White"
ind_black <- race_detail == "Black/AfAm"
ind_race_else <- 1 - ind_white - ind_black
ind_health_poor <- health_num == 1
ind_health_fair <- health_num == 2
ind_health_good <- health_num == 3
ind_health_excellent <- health_num == 4

ind_south <- (region_gss == "South")
ind_west <- (region_gss == "West")
ind_midwest <- (region_gss == "Midwest")
ind_northeast <- (region_gss == "Northeast")

ind_Hp <- hispanic_binary == "yes"
ind_born_outside_us <- born == "no"

ind_gay_les_homo <- sexornt == "Gay, lesbian, homosexual"
ind_bisexual <- sexornt == "Bisexual"
ind_heter_straight <- sexornt == "Heterosexual, straight"

## Table 2

apply(cbind(fem_num, masc_num, third_fem_num, third_masc_num, ind_noncomformy, ind_reflected_noncomformy, age, educ, inc_cont), 2, mean)
apply(cbind(fem_num, masc_num, third_fem_num, third_masc_num, age, educ, inc_cont), 2, sd)

apply(cbind(ind_health_excellent, ind_health_good, ind_health_fair, ind_health_poor,
            ind_gay_les_homo, ind_bisexual, ind_heter_straight, married_num,
            ind_white, ind_black, ind_race_else, ind_Hp, ind_born_outside_us, ind_northeast, ind_south, ind_west, ind_midwest), 2, mean)
apply(cbind(ind_health_excellent, ind_health_good, ind_health_fair, ind_health_poor,
            ind_gay_les_homo, ind_bisexual, ind_heter_straight, married_num,
            ind_white, ind_black, ind_race_else, ind_Hp, ind_born_outside_us, ind_northeast, ind_south, ind_west, ind_midwest), 2, sum)

## Table 3

# Without controls for survey condition

model3_1b <- lm(health_3cat ~ fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_1b)
BIC(model3_1b)
AIC(model3_1b)

model3_2b <- lm(health_3cat ~ masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_2b)
BIC(model3_2b)
AIC(model3_2b)

model3_3b <- lm(health_3cat ~ masc_num + fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_3b)
BIC(model3_3b)
AIC(model3_3b)

# With controls for survey condition

model3_1b <- lm(health_3cat ~ fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model3_1b)
BIC(model3_1b)
AIC(model3_1b)

model3_2b <- lm(health_3cat ~ masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model3_2b)
BIC(model3_2b)
AIC(model3_2b)

model3_3b <- lm(health_3cat ~ masc_num + fem_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model3_3b)
BIC(model3_3b)
AIC(model3_3b)



## Table 4

# Without controls for survey condition

model4_1b <- lm(health_3cat ~ third_masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
               + region_gss )
summary(model4_1b)
BIC(model4_1b)
AIC(model4_1b)

model4_2b <- lm(health_3cat ~ third_masc_num + masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
               + region_gss )
summary(model4_2b)
BIC(model4_2b)
AIC(model4_2b)

# With controls for survey condition

model4_1b <- lm(health_3cat ~ third_masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model4_1b)
BIC(model4_1b)
AIC(model4_1b)

model4_2b <- lm(health_3cat ~ third_masc_num + masc_num + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model4_2b)
BIC(model4_2b)
AIC(model4_2b)

## Table 5

# Without controls for survey condition

model5_1b <- lm(health_3cat ~ ind_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model5_1b)
BIC(model5_1b)
AIC(model5_1b)


model5_2b <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model5_2b)
BIC(model5_2b)
AIC(model5_2b)

model5_3b <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy +ind_noncomformy*ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model5_3b)
BIC(model5_3b)
AIC(model5_3b)

# With controls for survey condition

model5_1b <- lm(health_3cat ~ ind_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_1b)
BIC(model5_1b)
AIC(model5_1b)


model5_2b <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_2b)
BIC(model5_2b)
AIC(model5_2b)

model5_3b <- lm(health_3cat ~ ind_noncomformy + ind_reflected_noncomformy +ind_noncomformy*ind_reflected_noncomformy + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_3b)
BIC(model5_3b)
AIC(model5_3b)



detach(men)




#### Extension : without subsamples for cisgender women and men ####

attach(agms)

cis_women <- (sex=="Female") & (gender=="Woman")
cis_men <- (sex=="Male") & (gender=="Man")

## Table 3

# Interaction with cis_women

model3_1w <- lm(health_3cat ~ fem_num*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
               + region_gss )
summary(model3_1w)
BIC(model3_1w)
AIC(model3_1w)

model3_2w <- lm(health_3cat ~ masc_num*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_2w)
BIC(model3_2w)
AIC(model3_2w)

model3_3w <- lm(health_3cat ~ fem_num*cis_women + masc_num*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_3w)
BIC(model3_3w)
AIC(model3_3w)


# Interaction with cis_men

model3_1m <- lm(health_3cat ~ fem_num*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_1m)
BIC(model3_1m)
AIC(model3_1m)

model3_2m <- lm(health_3cat ~ masc_num*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss )
summary(model3_2m)
BIC(model3_2m)
AIC(model3_2m)

model3_3m <- lm(health_3cat ~ fem_num*cis_men + masc_num*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
               + region_gss )
summary(model3_3m)
BIC(model3_3m)
AIC(model3_3m)


## Table 4

# Interaction with cis_women

model4_1w <- lm(health_3cat ~ third_fem_num*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model4_1w)
BIC(model4_1w)
AIC(model4_1w)

model4_2w <- lm(health_3cat ~ third_fem_num*cis_women + fem_num*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model4_2w)
BIC(model4_2w)
AIC(model4_2w)


# Interaction with cis_men

model4_1m <- lm(health_3cat ~ third_masc_num*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model4_1m)
BIC(model4_1m)
AIC(model4_1m)

model4_2m <- lm(health_3cat ~ third_masc_num*cis_men + masc_num*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model4_2m)
BIC(model4_2m)
AIC(model4_2m)


## Table 5 

ind_noncomformy <- ((masc_num < fem_num) & (cis_men==TRUE)) | ((fem_num < masc_num) & (cis_women==TRUE))
ind_reflected_noncomformy <- ((third_masc_num < third_fem_num) & (cis_men==TRUE)) | ((third_fem_num < third_masc_num ) & (cis_women==TRUE))

# Interaction with cis_women

model5_1w <- lm(health_3cat ~ ind_noncomformy*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_1w)
BIC(model5_1w)
AIC(model5_1w)

model5_2w <- lm(health_3cat ~ ind_noncomformy*cis_women + ind_reflected_noncomformy*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_2w)
BIC(model5_2w)
AIC(model5_2w)

model5_3w <- lm(health_3cat ~ ind_noncomformy*cis_women + ind_reflected_noncomformy*cis_women +ind_noncomformy*ind_reflected_noncomformy*cis_women + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_3w)
BIC(model5_3w)
AIC(model5_3w)


# Interaction with cis_men

model5_1m <- lm(health_3cat ~ ind_noncomformy*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_1m)
BIC(model5_1m)
AIC(model5_1m)


model5_2m <- lm(health_3cat ~ ind_noncomformy*cis_men + ind_reflected_noncomformy*cis_men + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_2m)
BIC(model5_2m)
AIC(model5_2m)

model5_3m <- lm(health_3cat ~ (ind_noncomformy*cis_men) + (ind_reflected_noncomformy*cis_men) + (ind_noncomformy*ind_reflected_noncomformy*cis_men) + age + educ + inc_cont + sexornt + married + race_gss + hispanic_binary + ind_born
                + region_gss + module_end + scales_first + self_first)
summary(model5_3m)
BIC(model5_3m)
AIC(model5_3m)







#### Factor Analyse ####

library("psych")

##  可能的变量：fear_aqnt， agms$fear_ipv， agms$fear_strn， agms$fear_murd, agms$fear_sa
##  agms$natarmsy agms$natenviy agms$natcrimy agms$nateducy agms$natsci agms$natfarey
## 
aqnt_num <- as.numeric(agms$fear_aqnt)
ipv_num <- as.numeric(agms$fear_ipv)
strn_num <- as.numeric(agms$fear_strn)
murd_num <- as.numeric(agms$fear_murd)
sa_num <- as.numeric(agms$fear_sa)

armsy_num <- as.numeric(agms$natarmsy)
enviy_num <- as.numeric(agms$natenviy)
crimy_num <- as.numeric(agms$natcrimy)
educy_num <- as.numeric(agms$nateducy)
sci_num <- as.numeric(agms$natsci)
farey_num <- as.numeric(agms$natfarey)

health_num <- as.numeric(agms$health)


mydata <- cbind(aqnt_num, ipv_num, strn_num, murd_num, sa_num, armsy_num, enviy_num,
                crimy_num, educy_num, sci_num, farey_num, health_num)

fa.parallel(mydata, fa="fa", show.legend=F, main="Scree plot with parallel analysis")
fa1 <- fa(mydata, nfactors=3, rotate="none")
fa1
