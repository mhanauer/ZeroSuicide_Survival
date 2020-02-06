---
title: "Zero Suicide Rate"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(stringr)
library(prettyR)
library(rstanarm)
library(MissMech)
library(lubridate)
library(survival)
library(survminer)
```
Load in data. Need to check variables of interest are doing ok.
Check descriptives make sure nothing is out of bounds
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/ZeroSuicide/Matt'sData")
zero_suicide = read.csv("zero_suicide.csv", header= TRUE, na.strings = c("multiple ???", "NA"))
zero_suicide_denom = read.csv("zero_suicide_denom.csv", header = TRUE)
setwd("P:/Evaluation/TN Lives Count_Writing/ZeroSuicide/CDC")
cdc_rate = read.csv("cdc_rate.csv", header = TRUE)
centerstone_cdc_pop = read.csv("centerstone_cdc_pop.csv", header = TRUE)

zero_suicide_denom
cdc_rate
centerstone_cdc_pop

```
#######################
Count analysis 
Data cleaning
#########################
Notes
Put together variables you want which are below

```{r}
head(zero_suicide)
head(zero_suicide$centernet.termination.date)
zero_suicide_dat = data.frame(death_date = zero_suicide$Date.of.Incident.k., location = zero_suicide$Centerstone.Location, dob = zero_suicide$Date.of.Birth.k., gender = zero_suicide$Gender, event = zero_suicide$Type.of.Event, path_enroll_death = zero_suicide$Enrolled.in.Pathway...time.of.Incident, cssrs_date = zero_suicide$Date.of.Most.Recent.C.SSRS, current_path_enroll_date = zero_suicide$most.current.Pathway.Date.Enrolled, current_path_disenroll_date = zero_suicide$Most.current.Pathway.Date.Disenrolled, prim_diagnosis = zero_suicide$Primary.Diagnosis, num_prior_hospital = zero_suicide$Number.of.Prior.Hospital.Admissions, total_kept_services = zero_suicide$X..of.total.kept.services, first_contact_date = zero_suicide$first.service.contact.date.with.centerstone, centernet_term_date = zero_suicide$centernet.termination.date)

head(zero_suicide_dat)
zero_suicide_dat = subset(zero_suicide_dat, event == "SDC" | event == "SDC, HCBC" | event == "SDC/HBC")
dim(zero_suicide_dat)  # matches the number in the spreadsheet
head(zero_suicide_dat)
### Change the following variables to dates death_date, dob, cssrs_date, current_path_enroll_date, current_path_disenroll_date, first_contact_date, ,centernet
library(lubridate)
zero_suicide_dat$death_date = mdy(zero_suicide_dat$death_date)
zero_suicide_dat$dob = mdy(zero_suicide_dat$dob)
zero_suicide_dat$cssrs_date = mdy(zero_suicide_dat$cssrs_date)
zero_suicide_dat$current_path_enroll_date = mdy(zero_suicide_dat$current_path_enroll_date)
zero_suicide_dat$current_path_disenroll_date = mdy(zero_suicide_dat$current_path_disenroll_date)
zero_suicide_dat$first_contact_date = mdy(zero_suicide_dat$first_contact_date)
zero_suicide_dat$centernet_term_date = mdy(zero_suicide_dat$centernet_term_date)

### Create age at time of event variable by taking difference between date of event and dob divide by 365
age_at_death = round(as.numeric(zero_suicide_dat$death_date - zero_suicide_dat$dob)/365, 0)
zero_suicide_dat$age_at_death = age_at_death
zero_suicide_dat$dob = NULL

#Hanging Y here are the original numbers changed the factors
#N / 2 = 69
#Y / 7 = 26
#Y / 8 = 1
#Not implemented yet / 5 / Pre_Path
library(prettyR)
describe.factor(zero_suicide_dat$path_enroll_death)
### See if something changes 
zero_suicide_dat$path_enroll_death = ifelse(zero_suicide_dat$path_enroll_death == " Y", "Y", zero_suicide_dat$path_enroll_death)

#Test if the numbers are the same
#dat_test = data.frame(path_enroll_death_test = zero_suicide_dat$path_enroll_death_test, path_enroll_death = zero_suicide_dat$path_enroll_death)
#head(dat_test)
#dat_test

zero_suicide_dat$path_enroll_death = ifelse(zero_suicide_dat$path_enroll_death == 5, "Pre_Path", ifelse(zero_suicide_dat$path_enroll_death == 2, "N", ifelse(zero_suicide_dat$path_enroll_death == 7, "Y", ifelse(zero_suicide_dat$path_enroll_death == 8, "Y", "Wrong"))))
describe.factor(zero_suicide_dat$path_enroll_death)

### Make prior number of hosptial visits numeric
zero_suicide_dat$num_prior_hospital = as.numeric(zero_suicide_dat$num_prior_hospital)

#### Make total number of kept services numeric
zero_suicide_dat$total_kept_services = as.numeric(zero_suicide_dat$total_kept_services)

### Get rid of death dates that are NA, make them something else then subset
sum(is.na(zero_suicide_dat$death_date))
###
zero_suicide_dat$death_date[is.na(zero_suicide_dat$death_date)] = "2020-01-01"
range(zero_suicide_dat$death_date)
zero_suicide_dat = subset(zero_suicide_dat, death_date < "2020-01-01")
sum(is.na(zero_suicide_dat$death_date))
range(zero_suicide_dat$death_date)
sum(is.na(zero_suicide_dat$death_date))
#### Create intervention varable
zero_suicide_dat$zero_suicide = ifelse(zero_suicide_dat$death_date >="2014-01-01", 1, 0)
#### Only looking at 2009 and beyond, because that is all the rate data that we have
zero_suicide_dat = subset(zero_suicide_dat, death_date >= "2009-04-01")
```


###################
Count analysis
Particpant character
###################


```{r}
range(zero_suicide_dat$death_date, na.rm = TRUE)
describe.factor(zero_suicide_dat$gender)

### Zero Suicide
describe.factor(zero_suicide_dat$zero_suicide)

range(zero_suicide_dat$current_path_disenroll_date, na.rm = TRUE)
#describe.factor(zero_suicide_dat$prim_diagnosis)
describe.factor(zero_suicide_dat$num_prior_hospital)
mean(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)
sd(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)
range(zero_suicide_dat$num_prior_hospital, na.rm = TRUE)

describe.factor(zero_suicide_dat$age_at_death)
mean(zero_suicide_dat$age_at_death, na.rm = TRUE)
sd(zero_suicide_dat$age_at_death, na.rm = TRUE)
range(zero_suicide_dat$age_at_death, na.rm = TRUE)


### Error in pathway descriptives, because there are more deaths prior to enrollment according to death date (i.e. see Zero Suicide variables)
describe.factor(zero_suicide_dat$zero_suicide)
describe.factor(zero_suicide_dat$path_enroll_death)
```
Check out what variables you have
Let's try limiting it to those on the pathway
zero_suicide_questions[is.na(zero_suicide_questions)] = "2020-01-01"

Interpretation of hazard ratio: So our HR = 0.59 implies that around 0.6 times as many females are dying as males, at any given time
https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
```{r}
zero_suicide_dat

zero_suicide_dat_pathway = zero_suicide_dat
head(zero_suicide_dat_pathway)
zero_suicide_dat_pathway =  data.frame(death_date = zero_suicide_dat_pathway$death_date, gender = zero_suicide_dat_pathway$gender, current_path_enroll_date = zero_suicide_dat_pathway$current_path_enroll_date, age_at_death = zero_suicide_dat_pathway$age_at_death)
zero_suicide_dat_pathway
library(naniar)
miss_var_summary(zero_suicide_dat_pathway)
zero_suicide_dat_pathway_complete = na.omit(zero_suicide_dat_pathway) 
zero_suicide_dat_pathway_complete$censor = rep(1, dim(zero_suicide_dat_pathway_complete)[1])
zero_suicide_dat_pathway_complete$time_to_death =  zero_suicide_dat_pathway_complete$death_date-zero_suicide_dat_pathway_complete$current_path_enroll_date
zero_suicide_dat_pathway_complete$time_to_death = 
surv_path = Surv(zero_suicide_dat_pathway_complete$time_to_death, zero_suicide_dat_pathway_complete$censor)


```
Try some survival with just those on the pathway
```{r}
fit_surv = survfit(surv_path ~ 1, data = zero_suicide_dat_pathway_complete)

fit_surv_one_year = summary(survfit(surv_path~ 1, data = zero_suicide_dat_pathway_complete), times = 12)
fit_surv_one_year

ggsurvplot(fit_surv, data = zero_suicide_dat_pathway_complete, risk.table = "abs_pct", break.time.by = c(6), surv.scale = "percent", title = "Pathway Survival Probablity", xlab = "Months", legend = "none", tables.y.text.col = FALSE)

  

### I want plot the number probability of death at one year


```
Test if there is a difference for age (may need dicot this) and or gender
```{r}
head(zero_suicide_dat_pathway_complete)
describe.factor(zero_suicide_dat_pathway_complete$gender)
zero_suicide_dat_pathway_complete = subset(zero_suicide_dat_pathway_complete, gender == "M" | gender == "F")
describe.factor(zero_suicide_dat_pathway_complete$gender)
write.csv(zero_suicide_dat_pathway_complete, "zero_suicide_dat_pathway_complete.csv", row.names = FALSE)
zero_suicide_dat_pathway_complete = read.csv("zero_suicide_dat_pathway_complete.csv", header = TRUE)
describe.factor(zero_suicide_dat_pathway_complete$gender)
fix_cox = coxph(surv_path ~ age_at_death + gender, data = zero_suicide_dat_pathway_complete)
summary(fix_cox)
```


