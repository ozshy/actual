# Code for actual_v?.tex titled: "Stated Preference and Actual Choice"
# This code uses the 2024 translation-level and individual public data

# Packages used
library(ggplot2); theme_set(theme_bw())# for graphics
#library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
#library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displayes multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 
#library(nnet)# multinomial regressions
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library("xtable") #for exporting to LaTeX
library(dplyr)# 
#library(gtools)# for stars.pval function
library(scales)# to have % in ggplot axes and tables
library(caret)#for confusionMatrix. Note: it flips the orientation so it makes actual (reference) as columns and predicted (data) as rows => unconventional. 
# ML packages
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
library("randomForest")
#library("missForest")#to impute NAs, preparation for random forest (not used because cannot impute based on selected variables (no var selection.)
#library(mice)# for imputations. => cannot impute a large number of NA?
library(missRanger)# for imputations based on "amnt" and "merch only

#setwd("C:/Oz_local_workspace_2_actual/actual_R")# work
setwd("~/Research/SCPC_DCPC/2024_SDCPC")# home
dir()
# Read transaction dataset
trans2024_1.df = readRDS("dcpc-2024-tranlevel-public.rds")
dim(trans2024_1.df)
names(trans2024_1.df)

# Read individual dataset
indiv2024_1.df = readRDS("dcpc-2024-indlevel-public.rds")
dim(indiv2024_1.df)
names(indiv2024_1.df)

#Begin: preference versus HH income, Figure 1####

# delete unneeded variables from indiv dataset
i2.df = subset(indiv2024_1.df, select = c(id, paypref_inperson, paypref_b1, paypref_web, hhincome, income_hh, age, cc_adopt, chk_adopt, chk_adopt, computer_adopt, csh_adopt, crypto_adopt, dc_adopt, elect_adopt, mb_adopt, mobile_adopt, mobile_inperson_adopt))
# variable definitions: 
# paypref_b1 (pref for bills)
# paypref inperson (pref for in person payments)
# paypref_web (pref for online)
# hhincome (bins); income_hh (continuous)
# several "adopt" variables indicates having a credit card (or made payments from), etc. See codebook for more info. No all are included above. 
sample(i2.df)

# preferences for in-person payments
i2_inperson1.df = subset(i2.df, select = c(id, paypref_inperson, hhincome, age, income_hh, cc_adopt, csh_adopt, dc_adopt))# selecting only credit, debit, and cash
# check for missing values & deleting them
#sum(is.na(i2_inperson1.df$paypref_inperson))
table(i2_inperson1.df$paypref_inperson)

# select preference for 4 PI only
i2_inperson2.df = i2_inperson1.df %>%
  mutate(paypref_inperson = case_when(
    paypref_inperson == 1 ~ "cash",
    paypref_inperson == 3 ~ "credit",
    paypref_inperson == 4 ~ "debit",
    (paypref_inperson != 2 & paypref_inperson > 4)  ~ "other"
      ))
sample_n(i2_inperson2.df,20)
dim(i2_inperson2.df)

# check for missing values & deleting them
sum(is.na(i2_inperson2.df$paypref_inperson))
i2_inperson3.df = i2_inperson2.df[complete.cases(i2_inperson2.df), ]
dim(i2_inperson2.df)
dim(i2_inperson3.df)

# number who prefer cash by income category
(pref_cash_by_income.df = i2_inperson3.df %>% group_by(hhincome) %>% summarise(pref_cash = sum(paypref_inperson=="cash")))
# number who prefer debit by income category
(pref_debit_by_income.df = i2_inperson3.df %>% group_by(hhincome) %>% summarise(pref_debit = sum(paypref_inperson=="debit")))
# number who prefer credit by income category
(pref_credit_by_income.df = i2_inperson3.df %>% group_by(hhincome) %>% summarise(pref_credit = sum(paypref_inperson=="credit")))
# number who prefer other by income category
(pref_other_by_income.df = i2_inperson3.df %>% group_by(hhincome) %>% summarise(pref_other = sum(paypref_inperson=="other")))

#making the above a data frame
(pref_inperson1.df = data.frame(income_category = pref_cash_by_income.df$hhincome, cash = pref_cash_by_income.df[,2], debit = pref_debit_by_income.df[,2], credit = pref_credit_by_income.df[,2], other = pref_other_by_income.df[,2]))

# checksum columns
sum(pref_inperson1.df)-sum(1:16)# should equal number of repondents
dim(i2_inperson3.df)# number of respondents

# number of respondents by income category
head(i2_inperson3.df)
num_resp_by_income.df = i2_inperson3.df %>% group_by(hhincome) %>% count()
num_resp_by_income.df
sum(num_resp_by_income.df$n)# equal number of resp

# append num respondents by income to 
(pref_inperson2.df = pref_inperson1.df)
(pref_inperson2.df$num_resp = num_resp_by_income.df$n)
pref_inperson2.df

# Add column fraction prefer cash by income category
(pref_inperson3.df = pref_inperson2.df)
#
(pref_inperson3.df$frac_cash = pref_inperson3.df$pref_cash/pref_inperson3.df$num_resp)
(pref_inperson3.df$frac_debit = pref_inperson3.df$pref_debit/pref_inperson3.df$num_resp)
(pref_inperson3.df$frac_credit = pref_inperson3.df$pref_credit/pref_inperson3.df$num_resp)
(pref_inperson3.df$frac_other = pref_inperson3.df$pref_other/pref_inperson3.df$num_resp)

pref_inperson3.df
# verify frac sum up to 1
pref_inperson3.df$frac_cash +pref_inperson3.df$frac_debit +pref_inperson3.df$frac_credit +pref_inperson3.df$frac_other

names(pref_inperson3.df)
ggplot(pref_inperson3.df, aes(x=income_category, y=frac_cash)) + geom_line(linetype="solid", linewidth=1.2, color="black") +geom_point(size=3, color="black") +geom_line(aes(y=frac_debit), linetype="longdash", linewidth=1.2, color="blue") +geom_point(aes(y=frac_debit), size=3, color="blue") +geom_line(aes(y=frac_credit), linetype="dotdash", linewidth=1.2, color="red") +geom_point(aes(y=frac_credit), size=3, color="red") +geom_line(aes(y=frac_other), linetype="dotted", linewidth=1.2, color="darkgreen") +geom_point(aes(y=frac_other), size=3, color="darkgreen") +scale_x_continuous(breaks = seq(0,16,1)) + scale_y_continuous(breaks = seq(0,1,0.10), labels = percent(seq(0,1,0.10))) +labs(x="Annual household income group (low to high)", y="Preferred payment method (%)") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))  +annotate("text", x = 9, y = 0.55, label = "Debit card", size = 8, color="blue") +annotate("text", x = 9, y = 0.09, label = "Other", size = 8, color="darkgreen") +annotate("text", x = 13, y = 0.18, label = "Cash", size = 8, color="black") +annotate("text", x = 11, y = 0.35, label = "Credit card", size = 8, color="red")

# Info about Figure 1 (preferences vs. HH income) (in the paper)
sum(num_resp_by_income.df$n)# equal number of resp

# find number without a credit card
names(i2_inperson3.df)
# resp #w/o CC
nrow(subset(i2_inperson3.df, cc_adopt==0))
# the above as a fraction
nrow(subset(i2_inperson3.df, cc_adopt==0))/sum(num_resp_by_income.df)
# Num without a CC, but still prefer CC
nrow(subset(i2_inperson3.df, cc_adopt==0 & paypref_inperson == "credit"))#w/o CC but prefer CC

# find number without a debit card
names(i2_inperson3.df)
nrow(subset(i2_inperson3.df, dc_adopt==0))#w/o CC
# the above as a fraction
nrow(subset(i2_inperson3.df, dc_adopt==0))/sum(num_resp_by_income.df)
# Num without a dc, but still prefer dc
nrow(subset(i2_inperson3.df, dc_adopt==0 & paypref_inperson == "debit"))#w/o CC but prefer CC

#End: preference versus HH income, Figure 1####

#+++++++++++++++

#Begin: Age versus PI preference, Figure 2####

i2_inperson4.df =as.data.frame(i2_inperson3.df)
nrow(is.na(i2_inperson4.df$age))# verify no age NAs
summary(i2_inperson4.df$age)
class(i2_inperson4.df$age)
class(i2_inperson4.df)
names(i2_inperson4.df)
dim(i2_inperson4.df)#num resp
sapply(i2_inperson4.df, class)
i2_inperson4.df$paypref_inperson =as.factor(i2_inperson4.df$paypref_inperson)

# Construct and age groups
i2_inperson5.df =i2_inperson4.df %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(18, 24, 34, 44, 54, 64, 75, Inf),
      labels = c("18–24","25–34","35–44","45–54","55–64", "65–74","75+"),
      right = TRUE   # 17 goes in "0–17", 18 in "18–24", etc.
    )
  )
table(i2_inperson5.df$age_group, useNA = "always")
sum(table(i2_inperson5.df$age_group))
nrow(i2_inperson5.df)
names(i2_inperson5.df)
class(i2_inperson5.df$age_group)

# number resp who prefer cash by age category
(pref_cash_by_age.df = i2_inperson5.df %>% group_by(age_group) %>% summarise(pref_cash = sum(paypref_inperson=="cash")))
#
# number who prefer credit card by age category
(pref_credit_by_age.df = i2_inperson5.df %>% group_by(age_group) %>% summarise(pref_credit = sum(paypref_inperson=="credit")))
#
# number who prefer credit card by age category
(pref_debit_by_age.df = i2_inperson5.df %>% group_by(age_group) %>% summarise(pref_debit = sum(paypref_inperson=="debit")))
#
# number who prefer other by age category
(pref_other_by_age.df = i2_inperson5.df %>% group_by(age_group) %>% summarise(pref_other = sum(paypref_inperson=="other")))
#

#making the above a data frame
(pref_inperson_age1.df = data.frame(age_category = pref_cash_by_age.df$age_group, cash = pref_cash_by_age.df[,2], debit = pref_debit_by_age.df[,2], credit = pref_credit_by_age.df[,2], other = pref_other_by_age.df[,2]))

# check sum columns
dim(pref_inperson_age1.df)
sum(pref_inperson_age1.df[,2:5])# should equal number of repondents
dim(i2_inperson5.df)# number of respondents

# number of respondents by age group
head(i2_inperson5.df)
num_resp_by_age.df = i2_inperson5.df %>% group_by(age_group) %>% count()
num_resp_by_age.df
sum(num_resp_by_age.df$n)# equal number of resp

# append num respondents by age to 
(pref_inperson_age2.df = pref_inperson_age1.df)
(pref_inperson_age2.df$num_resp = num_resp_by_age.df$n)
pref_inperson_age2.df

# Add column fraction prefer cash by income category
(pref_inperson_age3.df = pref_inperson_age2.df)
#
(pref_inperson_age3.df$frac_cash = pref_inperson_age3.df$pref_cash/pref_inperson_age3.df$num_resp)
(pref_inperson_age3.df$frac_debit = pref_inperson_age3.df$pref_debit/pref_inperson_age3.df$num_resp)
(pref_inperson_age3.df$frac_credit = pref_inperson_age3.df$pref_credit/pref_inperson_age3.df$num_resp)
(pref_inperson_age3.df$frac_other = pref_inperson_age3.df$pref_other/pref_inperson_age3.df$num_resp)

pref_inperson_age3.df
# delete the last row with few NAs
(pref_inperson_age4.df = pref_inperson_age3.df[1:7, ])
# verify frac sum up to 1
pref_inperson_age3.df$frac_cash +pref_inperson_age3.df$frac_debit +pref_inperson_age3.df$frac_credit +pref_inperson_age3.df$frac_other

names(pref_inperson_age4.df)
ggplot(pref_inperson_age4.df, aes(x=age_category, y=frac_cash)) +geom_point(size=3, color="black") +geom_path(aes(group=1), linetype="solid", linewidth=1.2, color="black") +geom_point(aes(y=frac_debit), size=3, color="blue") +geom_path(aes(y=frac_debit, group=1), linetype="longdash", linewidth=1.2, color="blue") +geom_point(aes(y=frac_credit), size=3, color="red") +geom_path(aes(y=frac_credit, group=1), linetype="dotdash", linewidth=1.2, color="red") +geom_point(aes(y=frac_other), size=3, color="darkgreen") +geom_path(aes(y=frac_other, group = 1), linetype="dotted", linewidth=1.2, color="darkgreen") +scale_x_discrete(breaks = pref_inperson_age4.df$age_category) + scale_y_continuous(breaks = seq(0,1,0.10), labels = percent(seq(0,1,0.10))) +labs(x="Age group", y="Preferred payment method (%)") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 4, y = 0.46, label = "Debit card", size = 8, color="blue") +annotate("text", x = 4, y = 0.065, label = "Other", size = 8, color="darkgreen") +annotate("text", x = 4, y = 0.15, label = "Cash", size = 8, color="black") +annotate("text", x = 4, y = 0.32, label = "Credit card", size = 8, color="red")

# Info about Figure 2 (preferences vs. age) (in the paper)
sum(num_resp_by_age.df$n)# equal number of resp
# number of resp with age NA deleted
pref_inperson_age3.df[8, "num_resp" ]
# Hence, respondents in Figure preferece frac vs. age
sum(num_resp_by_age.df$n)-pref_inperson_age3.df[8, "num_resp" ]

#End: Age versus PI preference, Figure 2####

#+++++++++++++++

#Begin: Table 1: Confusion matrix####
#rows: Actually used PI (reference)
#columns: Preferred payment method (predictor = data)

# merge the two original datasets by id 
m1.df = left_join(trans2024_1.df, indiv2024_1.df, by = "id")
dim(m1.df)
names(m1.df)

# selecting needed variables.
table(m1.df$in_person)# num in-person payments
table(m1.df$bill)# num bill payments
# restricting to in-person payments and non-bill payments
m2.df = subset(m1.df, in_person ==1 & bill ==0)
dim(m2.df)
# deleting unneeded variables
names(m2.df %>% select(starts_with("paypref")))
m3.df = subset(m2.df, select = c(id, ind_weight_all, cc_adopt, dc_adopt, pi, merch, gender, age, amnt, paypref_inperson, accept_cash, accept_card, cc_surcharge, discount))
dim(m3.df)# number of in-person payments

table(m3.df$paypref_inperson)
table(m3.df$pi)

sum(is.na(m3.df$pi))
m4.df = subset(m3.df, !is.na(pi))# removing missing pi
sum(is.na(m4.df$pi))

# renaming pi
m5.df = m4.df %>% 
  mutate(pi = case_when(
    pi == 1 ~ "Cash",
    pi == 3 ~ "Credit",
    pi == 4 ~ "Debit",
    pi==0 | pi == 2 | pi > 4 ~ "Other",
      ))
table(m5.df$pi)
sum(table(m5.df$pi))# num payments
dim(m4.df)# verify
m5.df$pi = as.factor(m5.df$pi)
str(m5.df$pi)
(levels(m5.df$pi) = c("Cash", "Credit", "Debit", "Other"))
table(m5.df$pi)

#renaming paypref_inperson
sum(is.na(m5.df$paypref_inperson))# removing missing 
m6.df = subset(m5.df, !is.na(paypref_inperson))
dim(m6.df)
#
table(m6.df$paypref_inperson, useNA = "always")
# Group preferences other than for cash, credit, and debit into "others"
m7.df = m6.df %>% 
  mutate(paypref_inperson = case_when(
    paypref_inperson == 1 ~ "Cash",
    paypref_inperson == 3 ~ "Credit",
    paypref_inperson == 4 ~ "Debit",
    pi==paypref_inperson | paypref_inperson == 2 | paypref_inperson > 4 ~ "Other",
  ))
names(m7.df)
nrow(m7.df[unique(m7.df$id), ])# num respondents

table(m7.df$paypref_inperson)
sum(table(m7.df$paypref_inperson))# num payments
m7.df$paypref_inperson = as.factor(m7.df$paypref_inperson)
(levels(m7.df$paypref_inperson) = c("Cash", "Credit", "Debit", "Other"))
table(m7.df$paypref_inperson)

# Restrict to merchant types with more than 200 in-person non-bill payments
(m8.df = m7.df %>% group_by(merch) %>% filter(n() >= 200))
table(m8.df$merch, useNA = "always")
length(table(m8.df$merch))# reduced number of merchant/sending types
m8.df$merch = as.factor(m8.df$merch)
str(m8.df$merch)# verify merch is a factor

# checking amount
summary(m8.df$amnt)
# which payment amounts are <= 0?
m8.df %>% filter(amnt <= 0)
# Delete these payments with amnt < 0 or = 0
dim(m8.df)
m9.df = m8.df %>% filter(amnt > 0)
dim(m9.df)
summary(m9.df$amnt)
str(m9.df$amnt)# verify amount is a number
#
table(m9.df$pi)
str(m9.df$pi)# verify it is factor => classification tree. Otherwise, Numeric => regression tree

# confusion matrix: preferred versus actual
# Note caret flips (transposes) the orientation =>
# columns: Actually used PI (actual = reference)
# rows: Preferred payment method (predictor = data)
(confusion.t = confusionMatrix(data =m9.df$paypref_inperson, reference =m9.df$pi, dnn = c("Preferred", "Actual")))

# convert it to dataframe
(confusion2.df = as.data.frame.matrix(confusion.t$table))
# adding a total column
(total_rowsum.vec = rowSums(confusion2.df[,1:4]))
confusion3.df = confusion2.df
sum(confusion2.df)# num of payments in-person (equals num of rows in m6.df)
dim(m6.df)

# adding a total column (horizontal sum)
(confusion3.df$Total = total_rowsum.vec)
confusion3.df

# Converting to % by row
(confusion_perc.t = as.data.frame.matrix(round(100*prop.table(confusion.t$table, margin=1),0)))
# constructing a column of horizontal sums.
(total_rowsum_perc.vec = rowSums(confusion_perc.t[,1:4]))
(confusion_perc2.t = confusion_perc.t)
str(confusion_perc2.t)
(confusion_perc2.t$Total = total_rowsum_perc.vec)# adding total column
confusion_perc2.t
str(confusion_perc2.t)
# adding % sign
(confusion_perc3.t =  confusion_perc2.t %>% mutate(across(where(is.numeric), ~paste0(., "%"))))

# Combine the 2 confusion tables into one
(confusion_bind1.df = rbind(confusion3.df, confusion_perc3.t))

# adding stats at the bottom to the confusion table
str(confusion.t)
(confusion_stat.df = as.data.frame.matrix(confusion.t$byClass))
(confusion_stat2.df = t(confusion_stat.df))# transpose
dim(confusion_stat2.df)
# delete rows with unneeded stats
(confusion_stat3.df = confusion_stat2.df[c(1:4, 11), ])
# modify some row names
rownames(confusion_stat3.df)
confusion_stat4.df =confusion_stat3.df
(rownames(confusion_stat4.df) =c("Sensitivity (SEN)", "Specificity (SPE)", "Pos Pred Value (PPV)", "Neg Pred Value (NPV)", "Balanced Accuracy (BA)"))

dim(confusion_bind1.df)
dim(confusion_stat4.df)
# the above is a column short. Add a column
(confusion_stat5.df = as.data.frame(confusion_stat4.df ))
confusion_stat5.df$Total = NA
confusion_stat5.df
(confusion_stat6.df = round(confusion_stat5.df, digits = 2))

# Rename columns to match those of confusion_bind1.df
names(confusion_bind1.df)
names(confusion_stat6.df) = names(confusion_bind1.df)

# Constructing TP, FP, FN, and TN table (to be appended to the above)
confusion3.df
str(confusion3.df)
dim(confusion3.df)
#
# cash
(tp_cash = confusion3.df["Cash", "Cash"])# diagonal
#(fn_cash = sum(confusion3.df["Cash", c("Credit", "Debit", "Other") ]))# row sum
(fn_cash = sum(confusion3.df[c("Credit", "Debit", "Other"), "Cash" ]))# column sum
#(fp_cash = sum(confusion3.df[c("Credit", "Debit", "Other"), "Cash" ]))# column sum
(fp_cash = sum(confusion3.df["Cash", c("Credit", "Debit", "Other")]))# row sum
(tn_cash = sum(confusion3.df[c("Credit", "Debit", "Other"), c("Credit", "Debit", "Other")]))#sum table excluding cash row and cash column
tp_cash+fn_cash+fp_cash+tn_cash# verify sum up to num payments
#
# credit
(tp_credit = confusion3.df["Credit", "Credit"])
#(fn_credit = sum(confusion3.df["Credit", c("Cash", "Debit", "Other") ]))
(fn_credit = sum(confusion3.df[c("Cash", "Debit", "Other"), "Credit"]))
#(fp_credit = sum(confusion3.df[c("Cash", "Debit", "Other"), "Credit" ]))
(fp_credit = sum(confusion3.df["Credit", c("Cash", "Debit", "Other")]))
(tn_credit = sum(confusion3.df[c("Cash", "Debit", "Other"), c("Cash", "Debit", "Other")]))
tp_credit+fn_credit+fp_credit+tn_credit# verify sum up to num payments
#
# debit
(tp_debit = confusion3.df["Debit", "Debit"])
#(fn_debit = sum(confusion3.df["Debit", c("Cash", "Credit", "Other") ]))
(fn_debit = sum(confusion3.df[c("Cash", "Credit", "Other"), "Debit"]))
#(fp_debit = sum(confusion3.df[c("Cash", "Credit", "Other"), "Debit" ]))
(fp_debit = sum(confusion3.df["Debit", c("Cash", "Credit", "Other")]))
(tn_debit = sum(confusion3.df[c("Cash", "Credit", "Other"), c("Cash", "Credit", "Other")]))
tp_debit+fn_debit+fp_debit+tn_debit# verify sum up to num payments
#
(tp_other = confusion3.df["Other", "Other"])
#(fn_other = sum(confusion3.df["Other", c("Cash", "Credit", "Debit") ]))
(fn_other = sum(confusion3.df[c("Cash", "Credit", "Debit"), "Other"]))
#(fp_other = sum(confusion3.df[c("Cash", "Credit", "Debit"), "Other" ]))
(fp_other = sum(confusion3.df["Other", c("Cash", "Credit", "Debit")]))
(tn_other = sum(confusion3.df[c("Cash", "Credit", "Debit"), c("Cash", "Credit", "Debit")]))
tp_other+fn_other+fp_other+tn_other# verify sum up to num payments

# place TP, FN, FP, TN into a data frame as columns
(tf.df = data.frame(Cash = c(tp_cash, fn_cash, fp_cash, tn_cash), Credit = c(tp_credit, fn_credit, fp_credit, tn_credit), Debit = c(tp_debit, fn_debit, fp_debit, tn_debit), Other = c(tp_other, fn_other, fp_other, tn_other) ))
# Adding row names
rownames(tf.df) = c("True Positives (TP)", "False Negatives (FN)", "False Positives (FP)", "True Negatives (TN)")
tf.df
# verify columns sum up to num payments
tf.df %>% summarise(across(c("Cash", "Credit", "Debit", "Other"), sum))

# the above is a column short. Add a column
(tf2.df = tf.df)
(tf2.df$Total = NA)
tf2.df

# combine 3 tables 
(confusion_bind2.df = rbind(confusion_bind1.df, tf2.df, confusion_stat6.df))

# export to LaTeX
nrow(confusion_bind2.df)
ncol(confusion_bind2.df)
# matrix with digits (1 extra column)
#(digit_m = matrix(2, nrow(confusion_bind2.df), ncol(confusion_bind2.df)+1))
#
#digit_m[1:8, ] = 0# zero digits
#digit_m[9:nrow(confusion_bind2.df), ] =2

print(xtable(confusion_bind2.df), hline.after =c(4,8,12))

# Notes for Table 1 in paper
dim(confusion_bind2.df)
sum(as.numeric(confusion_bind2.df[1:4,5]))# num payments
nrow(m9.df)# verify equals the above
nrow(m9.df[unique(m9.df$id), ])# num respondents

#End: Table 1: Confusion matrix####

#+++++++++++++++++++++

#Begin: random forest variable importance, Table 2####
r1.df = m9.df
# pi
class(r1.df$pi)
table(r1.df$pi, useNA = "always")
# paypref
class(r1.df$paypref_inperson)
table(r1.df$paypref_inperson, useNA = "always")
# merch
class(r1.df$merch)
table(r1.df$merch, useNA = "always")
#
## cc_adopt (not used)
class(r1.df$cc_adopt)
table(r1.df$cc_adopt, useNA = "always")
r1.df$cc_adopt =as.factor(r1.df$cc_adopt)
levels(r1.df$cc_adopt)
levels(r1.df$cc_adopt)[levels(r1.df$cc_adopt)==0] ="No"
levels(r1.df$cc_adopt)[levels(r1.df$cc_adopt)==1] ="Yes"
table(r1.df$cc_adopt, useNA = "always")

#
## dc_adopt (not used)
table(r1.df$dc_adopt, useNA = "always")
class(r1.df$dc_adopt)
r1.df$dc_adopt =as.factor(r1.df$dc_adopt)
levels(r1.df$dc_adopt)
levels(r1.df$dc_adopt)[levels(r1.df$dc_adopt)==0] ="No"
levels(r1.df$dc_adopt)[levels(r1.df$dc_adopt)==1] ="Yes"
table(r1.df$dc_adopt, useNA = "always")
#
## accept_card
table(r1.df$accept_card, useNA = "always")
# Check replies by PI
r1.df %>% group_by(pi) %>% summarise(count = sum(accept_card, na.rm = T))
r1.df %>% group_by(pi) %>% summarise(count = sum(is.na(accept_card)))
# shift all credit NA to 1 (Yes)
r1.df = r1.df %>% mutate(accept_card = ifelse(is.na(accept_card) & pi =="Credit", 1, accept_card))
table(r1.df$accept_card, useNA = "always")
r1.df %>% group_by(pi) %>% summarise(count = sum(is.na(accept_card)))
# shift all debit NA to 1 (Yes)
r1.df = r1.df %>% mutate(accept_card = ifelse(is.na(accept_card) & pi == "Debit", 1, accept_card))
table(r1.df$accept_card, useNA = "always")
r1.df %>% group_by(pi) %>% summarise(count = sum(is.na(accept_card)))
# make 3 (don't know) as NA
r1.df = r1.df %>% mutate(accept_card = ifelse(accept_card==3, NA, accept_card))
table(r1.df$accept_card, useNA = "always")
class(r1.df$accept_card)
r1.df$accept_card =as.factor(r1.df$accept_card)
table(r1.df$accept_card, useNA = "always")# 
levels(r1.df$accept_card)
levels(r1.df$accept_card)[levels(r1.df$accept_card)==0] ="No"
levels(r1.df$accept_card)[levels(r1.df$accept_card)==1] ="Yes"
table(r1.df$accept_card, useNA = "always")
# for later comparison with imputed data (frac of trans where card was accepted)
(frac_accept_card = table(r1.df$accept_card)[2]/(table(r1.df$accept_card)[1]+table(r1.df$accept_card)[2]))
#
## accept_cash
table(r1.df$accept_cash, useNA = "always")
# Check replies by PI
r1.df %>% group_by(pi) %>% summarise(count = sum(accept_cash, na.rm = T))
r1.df %>% group_by(pi) %>% summarise(count = sum(is.na(accept_cash)))
# shift all cash NA to 1 (Yes)
r1.df = r1.df %>% mutate(accept_cash = ifelse(is.na(accept_cash) & pi =="Cash", 1, accept_cash))
table(r1.df$accept_cash, useNA = "always")
# make 5 (don't know) as NA
r1.df = r1.df %>% mutate(accept_cash = ifelse(accept_cash==5, NA, accept_cash))
table(r1.df$accept_cash, useNA = "always")
# make 2 a 0 (no)
r1.df = r1.df %>% mutate(accept_cash = ifelse(accept_cash==2, 0, accept_cash))
table(r1.df$accept_cash, useNA = "always")
# make 3 a 1 (yes)
r1.df = r1.df %>% mutate(accept_cash = ifelse(accept_cash==3, 1, accept_cash))
table(r1.df$accept_cash, useNA = "always")
# make 4 a 0 (no)
r1.df = r1.df %>% mutate(accept_cash = ifelse(accept_cash==4, 0, accept_cash))
table(r1.df$accept_cash, useNA = "always")
class(r1.df$accept_cash)
r1.df$accept_cash =as.factor(r1.df$accept_cash)
table(r1.df$accept_cash, useNA = "always")# 
levels(r1.df$accept_cash)
levels(r1.df$accept_cash)[levels(r1.df$accept_cash)==0] ="No"
levels(r1.df$accept_cash)[levels(r1.df$accept_cash)==1] ="Yes"
table(r1.df$accept_cash, useNA = "always")# 
# for later comparison with imputed data
(frac_accept_cash = table(r1.df$accept_cash)[2]/(table(r1.df$accept_cash)[1]+table(r1.df$accept_cash)[2]))
#
## cc_surcharge
table(r1.df$cc_surcharge, useNA = "always")
class(r1.df$cc_surcharge)
r1.df$cc_surcharge =as.factor(r1.df$cc_surcharge)
table(r1.df$cc_surcharge, useNA = "always")# 
levels(r1.df$cc_surcharge)
levels(r1.df$cc_surcharge)[levels(r1.df$cc_surcharge)==0] ="No"
levels(r1.df$cc_surcharge)[levels(r1.df$cc_surcharge)==1] ="Yes"
table(r1.df$cc_surcharge, useNA = "always")# 
# by PI
r1.df %>% group_by(pi) %>% summarise(count =sum(is.na(cc_surcharge)))
# for later comparison with imputed data
(frac_cc_surcharge = table(r1.df$cc_surcharge)[2]/(table(r1.df$cc_surcharge)[1]+table(r1.df$cc_surcharge)[2]))
#
## discount
table(r1.df$discount, useNA = "always")
class(r1.df$discount)
r1.df$discount =as.factor(r1.df$discount)
table(r1.df$discount, useNA = "always")# 
levels(r1.df$discount)
levels(r1.df$discount)[levels(r1.df$discount)==0] ="No"
levels(r1.df$discount)[levels(r1.df$discount)==1] ="Yes"
table(r1.df$discount, useNA = "always")# 
r1.df %>% group_by(pi) %>% summarise(count =sum(discount=="Yes"))
r1.df %>% group_by(pi) %>% summarise(count =sum(discount=="No"))
r1.df %>% group_by(pi) %>% summarise(count =sum(is.na(discount)))
# for later comparison with imputed data
(frac_discount = table(r1.df$discount)[2]/(table(r1.df$discount)[1]+table(r1.df$discount)[2]))

# Verify  only merch with more than 200 in-person non-bill payments
r1.df %>% group_by(merch) %>% summarise(n())
r2.df = r1.df
table(r2.df$merch)
length(table(r2.df$merch))# reduced number of merchant/sending types
r2.df$merch = as.factor(r2.df$merch)
class(r2.df$merch)# verify merch is a factor

# checking amount
summary(r2.df$amnt)
nrow(subset(r2.df, amnt <0))# verify all amounts > 0
r3.df = as.data.frame(r2.df)
summary(r3.df$amnt)
class(r3.df$amnt)# verify amount is a number
#
table(r3.df$pi)
class(r3.df$pi)# verify it is factor => classification tree. 

# Make gender a factor
class(r3.df$gender)
table(r3.df$gender, useNA = "always")
r3.df = r3.df %>% mutate(gender =ifelse(gender==1, "Male", "Female"))
table(r3.df$gender, useNA = "always")
r3.df$gender =as.factor(r3.df$gender)

# remove unneeded vars
r4.df = subset(r3.df, select = c(id, ind_weight_all, pi, merch, amnt, paypref_inperson, accept_cash, accept_card, cc_surcharge, discount))

# Random forest model: Equation (2) in the paper
names(r4.df)
str(r4.df)
dim(r4.df)

# impute NAs
# which vars have NAs (to be imputed, except age)
names(r4.df)[sapply(r4.df, function(x) any(is.na(x)))]
nrow(r4.df[is.na(r4.df$accept_cash), ])#impute
nrow(r4.df[is.na(r4.df$accept_card), ])#impute
nrow(r4.df[is.na(r4.df$cc_surcharge), ])#impute
nrow(r4.df[is.na(r4.df$discount), ])#impute
#the imputation should be based on the following vars
nrow(r4.df[is.na(r4.df$merch), ])#base var no NA
nrow(r4.df[is.na(r4.df$amnt), ])#base var no NA
nrow(r4.df[is.na(r4.df$pi), ])#base var, no NA
nrow(r4.df[is.na(r4.df$age), ])#

set.seed(1955)
sum(is.na(r4.df))

# below is the correct specification for new versions of missRanger. However, an unexplained bug forces me to use the old specification which does not have var=
#r4_imp_accept_cash = missRanger(data = r4.df, vars ="accept_cash", formula = accept_cash ~ pi + amnt + merch)
# Also, a list of formulas does not work in missRanger
#r4_imp_test = missRanger(data = r4.df, formula = list(accept_cash=accept_cash ~ pi + amnt + merch, cc_surcharge=cc_surcharge ~ pi + amnt + merch))

# ==> Forced to impute one variable at a time (x4 variables)
# impute with respect to amount and merchant type (because these influence the 4 predicted with the NA)
r4_imp_accept_cash = missRanger(data = r4.df, formula = accept_cash ~ pi + amnt + merch)
#
r4_imp_accept_card = missRanger(data = r4.df, formula = accept_card ~ pi + amnt + merch)
#
r4_imp_cc_surcharge = missRanger(data = r4.df, formula = cc_surcharge ~ pi + amnt + merch)
#
r4_imp_discount = missRanger(data = r4.df, formula = discount ~ pi + amnt + merch)

# Merge the 4 imputations into a single data frame (in sequence)
names(r4_imp_accept_cash)
r4_imputed.df = r4_imp_accept_cash
r4_imputed.df$accept_card = r4_imp_accept_card$accept_card
r4_imputed.df$cc_surcharge = r4_imp_cc_surcharge$cc_surcharge
r4_imputed.df$discount = r4_imp_discount$discount
#
head(r4_imputed.df)
sum(is.na(r4_imputed.df))# remaining NA?
nrow(r4_imputed.df)
# check for NAs before imputations
r4_imputed.df %>% summarise(across(everything(), ~ sum(is.na(.))))

# Compare some ratios of the imputed data to the un-imputed data to verify that there are only small changes relative to the ratios in the raw data
(frac_accept_card_imputed = table(r4_imputed.df$accept_card)/nrow(r4_imputed.df))
frac_accept_card
#
(frac_accept_cash_imputed = table(r4_imputed.df$accept_cash)/nrow(r4_imputed.df))
frac_accept_cash
#
(frac_cc_surcharge_imputed = table(r4_imputed.df$cc_surcharge)/nrow(r4_imputed.df))
frac_cc_surcharge
#
(frac_discount_imputed = table(r4_imputed.df$discount)/nrow(r4_imputed.df))
frac_discount

# Model
forest_model1 = pi ~ paypref_inperson + merch + amnt + accept_card + accept_cash + cc_surcharge + discount

# random forest
set.seed(1955)
(forest_output =randomForest(forest_model1, data=r4_imputed.df, importance=T, na.action=na.roughfix))
# defaults: mtry = rounded downwards sqrt(#predictors), nodesize=1, na.action =na.roughtfix => imputations. 
forest_output$type# verify classification tree (not regression tree)
#forest_output$confusion based on the OOB sample (Instead, I use train-test subsamples for the confusion matrix)

# Table of variable importance for the entire sample
forest_importance.df =importance(forest_output) 
str(forest_importance.df)
(forest_importance2.df = as.data.frame(forest_importance.df))
# Below, Plot of variable importance (not in paper)
#varImpPlot(random_forest_output, type = 1, main ='', bg = "blue", cex=2)#default type 1&2, 

# Rename rows
(forest_importance3.df = round(forest_importance2.df, digits = 2))
(row.names(forest_importance3.df) = c("Preferred payment method",  "Spending/merchant category", "Spending amount", "Card accepted", "Cash accepted", "Credit card surcharge", "Discount for payment method"))
forest_importance3.df
#
# Delete the GINI MDA from the importance table
names(forest_importance3.df)
(forest_importance4.df = forest_importance3.df %>% select(-"MeanDecreaseGini"))

#
print(xtable(forest_importance4.df))
# info about the above table notes
nrow(r4.df)# num of payments
nrow(r4.df[unique(r4.df$id), ])# num respondents

#End: random forest variable importance, Table 2####

#+++++++++++++++

#Begin: random forest confusion matrix, Table 3####
# split training and testing data for prediction tables
nrow(r4.df)# going back to the un-imputed data (to be imputed below for training and testing separately)

set.seed(1955)# may not be needed if the previous one is still in effect
r4_train_index = sample(nrow(r4.df), 0.7*nrow(r4.df))
length(r4_train_index)
#
r4_train.df = r4.df[r4_train_index, ]
r4_test.df = r4.df[-r4_train_index, ]
dim(r4_train.df)
dim(r4_test.df)
dim(r4_train.df) + dim(r4_test.df)# must equal nrow r4.df

#Both the training and testing data must be imputed for NAs
str(r4.df)
sapply(r4.df, class)
# impute the training data
r4_imp_accept_cash_train = missRanger(data = r4_train.df, formula =  accept_cash ~ pi + amnt + merch)
#
r4_imp_accept_card_train = missRanger(data = r4_train.df, formula = pi + accept_card ~ pi + amnt + merch)
#
r4_imp_cc_surcharge_train = missRanger(data = r4_train.df, formula = cc_surcharge ~ pi + amnt + merch)
#
r4_imp_discount_train = missRanger(data = r4_train.df, formula = discount ~ pi + amnt + merch)
#
# Merge the 4 training imputations into a single data frame (in sequence)
names(r4_imp_accept_cash_train)
r4_train_imputed.df = r4_imp_accept_cash_train
r4_train_imputed.df$accept_card = r4_imp_accept_card_train$accept_card
r4_train_imputed.df$cc_surcharge = r4_imp_cc_surcharge_train$cc_surcharge
r4_train_imputed.df$discount = r4_imp_accept_card_train$discount
#
head(r4_train_imputed.df)
sum(is.na(r4_train_imputed.df))# remaining NA?
nrow(r4_train_imputed.df)

# impute the test data
r4_imp_accept_cash_test = missRanger(data = r4_test.df, formula = accept_cash ~ pi + amnt + merch)
#
r4_imp_accept_card_test = missRanger(data = r4_test.df, formula = accept_card ~ pi + amnt + merch)
#
r4_imp_cc_surcharge_test = missRanger(data = r4_test.df, formula = cc_surcharge ~ pi + amnt + merch)
#
r4_imp_discount_test = missRanger(data = r4_test.df, formula = discount ~ pi + amnt + merch)
#
# Merge the 4 test imputations into a single data frame (in sequence)
names(r4_imp_accept_cash_test)
r4_test_imputed.df = r4_imp_accept_cash_test
r4_test_imputed.df$accept_card = r4_imp_accept_card_test$accept_card
r4_test_imputed.df$cc_surcharge = r4_imp_cc_surcharge_test$cc_surcharge
r4_test_imputed.df$discount = r4_imp_discount_test$discount
#
head(r4_test_imputed.df)
sum(is.na(r4_test_imputed.df))# remaining NA?
nrow(r4_test_imputed.df)

dim(r4_train_imputed.df) +dim(r4_test_imputed.df)#=> num payments
sum(is.na(r4_test_imputed.df))# NA remained?

#Now, use only training data to prepare prediction
forest_model1
(forest_output_train=randomForest(forest_model1, data=r4_train_imputed.df, importance=T, na.action=na.roughfix)) # impute NAs
#importance(forest_output_train) # Table of variable importance
#varImpPlot(method_on_demog_rf) # Plot of variable importance (not displayed in paper)

#Remove NAs from the imputed testing data (NAs remain under "discount")
sum(is.na(r4_test_imputed.df))
dim(r4_test_imputed.df)

# Predict using the test data
forest_pred=predict(forest_output_train, newdata = r4_test_imputed.df)# predict test data

#Start predictions and building confusion matrix (bottom of Table 3)
# confusion matrix: preferred versus actual
# Note caret (also randomForest) flips (transposes) the orientation =>
# columns: Actually used PI (actual = reference)
# rows: Preferred payment method (predictor = data)
# Random forest confusion table: Note orientation: (predict,actual)
(forest_confusion.t = confusionMatrix(forest_pred, r4_test_imputed.df$pi))
# convert it to dataframe
(forest_confusion2.df = as.data.frame.matrix(forest_confusion.t$table))
# adding a total column
(forest_total_rowsum.vec = rowSums(forest_confusion2.df[,1:4]))
forest_confusion3.df = forest_confusion2.df
sum(forest_confusion2.df)# num of payments in-person (equals num of rows:)
dim(r4_test_imputed.df)

# adding a total column (horizontal sum)
(forest_confusion3.df$Total = forest_total_rowsum.vec)
forest_confusion3.df

# Converting to % by row
(forest_confusion_perc.t = as.data.frame.matrix(round(100*prop.table(forest_confusion.t$table, margin=1),0)))
# constructing a column of horizontal sums.
(forest_total_rowsum_perc.vec = rowSums(forest_confusion_perc.t[,1:4]))
(forest_confusion_perc2.t = forest_confusion_perc.t)
str(forest_confusion_perc2.t)
(forest_confusion_perc2.t$Total = forest_total_rowsum_perc.vec)# adding total column
forest_confusion_perc2.t
str(forest_confusion_perc2.t)
# adding % sign
(forest_confusion_perc3.t =  forest_confusion_perc2.t %>% mutate(across(where(is.numeric), ~paste0(., "%"))))

# Combine the 2 confusion tables into one
(forest_confusion_bind1.df = rbind(forest_confusion3.df, forest_confusion_perc3.t))

# adding stats at the bottom to the confusion table
str(forest_confusion.t)
(forest_confusion_stat.df = as.data.frame.matrix(forest_confusion.t$byClass))
(forest_confusion_stat2.df = t(forest_confusion_stat.df))# transpose
dim(forest_confusion_stat2.df)
# delete rows with unneeded stats
(forest_confusion_stat3.df = forest_confusion_stat2.df[c(1:4, 11), ])
# modify some row names
rownames(forest_confusion_stat3.df)
forest_confusion_stat4.df =forest_confusion_stat3.df
(rownames(forest_confusion_stat4.df) =c("Sensitivity (SEN)", "Specificity (SPE)", "Pos Pred Value (PPV)", "Neg Pred Value (NPV)", "Balanced Accuracy (BA)"))

dim(forest_confusion_bind1.df)
dim(forest_confusion_stat4.df)
# the above is a column short. Add a column
(forest_confusion_stat5.df = as.data.frame(forest_confusion_stat4.df ))
forest_confusion_stat5.df$Total = NA
forest_confusion_stat5.df
(forest_confusion_stat6.df = round(forest_confusion_stat5.df, digits = 2))

# Rename columns to match those of confusion_bind1.df
names(forest_confusion_bind1.df)
names(forest_confusion_stat6.df) = names(forest_confusion_bind1.df)

# Constructing TP, FP, FN, and TN table (to be appended to the above)
forest_confusion3.df
str(forest_confusion3.df)
dim(forest_confusion3.df)
#
# cash
(forest_tp_cash = forest_confusion3.df["Cash", "Cash"])# diagonal
#(fn_cash = sum(confusion3.df["Cash", c("Credit", "Debit", "Other") ]))# row sum
(forest_fn_cash = sum(forest_confusion3.df[c("Credit", "Debit", "Other"), "Cash" ]))# column sum
#(fp_cash = sum(confusion3.df[c("Credit", "Debit", "Other"), "Cash" ]))# column sum
(forest_fp_cash = sum(forest_confusion3.df["Cash", c("Credit", "Debit", "Other")]))# row sum
(forest_tn_cash = sum(forest_confusion3.df[c("Credit", "Debit", "Other"), c("Credit", "Debit", "Other")]))#sum table excluding cash row and cash column
forest_tp_cash+forest_fn_cash+forest_fp_cash+forest_tn_cash# verify sum up to num payments
sum(forest_confusion3.df[, 1:4])# num payments
#
# credit
(forest_tp_credit = forest_confusion3.df["Credit", "Credit"])
#(fn_credit = sum(confusion3.df["Credit", c("Cash", "Debit", "Other") ]))
(forest_fn_credit = sum(forest_confusion3.df[c("Cash", "Debit", "Other"), "Credit"]))
#(fp_credit = sum(confusion3.df[c("Cash", "Debit", "Other"), "Credit" ]))
(forest_fp_credit = sum(forest_confusion3.df["Credit", c("Cash", "Debit", "Other")]))
(forest_tn_credit = sum(forest_confusion3.df[c("Cash", "Debit", "Other"), c("Cash", "Debit", "Other")]))
forest_tp_credit+forest_fn_credit+forest_fp_credit+forest_tn_credit# verify sum up to num payments
#
# debit
(forest_tp_debit = forest_confusion3.df["Debit", "Debit"])
#(fn_debit = sum(confusion3.df["Debit", c("Cash", "Credit", "Other") ]))
(forest_fn_debit = sum(forest_confusion3.df[c("Cash", "Credit", "Other"), "Debit"]))
#(fp_debit = sum(confusion3.df[c("Cash", "Credit", "Other"), "Debit" ]))
(forest_fp_debit = sum(forest_confusion3.df["Debit", c("Cash", "Credit", "Other")]))
(forest_tn_debit = sum(forest_confusion3.df[c("Cash", "Credit", "Other"), c("Cash", "Credit", "Other")]))
forest_tp_debit+forest_fn_debit+forest_fp_debit+forest_tn_debit# verify sum up to num payments
#
(forest_tp_other = forest_confusion3.df["Other", "Other"])
#(fn_other = sum(confusion3.df["Other", c("Cash", "Credit", "Debit") ]))
(forest_fn_other = sum(forest_confusion3.df[c("Cash", "Credit", "Debit"), "Other"]))
#(fp_other = sum(confusion3.df[c("Cash", "Credit", "Debit"), "Other" ]))
(forest_fp_other = sum(forest_confusion3.df["Other", c("Cash", "Credit", "Debit")]))
(forest_tn_other = sum(forest_confusion3.df[c("Cash", "Credit", "Debit"), c("Cash", "Credit", "Debit")]))
forest_tp_other+forest_fn_other+forest_fp_other+forest_tn_other# verify sum up to num payments

# place TP, FN, FP, TN into a data frame as columns
(forest_tf.df = data.frame(Cash = c(forest_tp_cash, forest_fn_cash, forest_fp_cash, forest_tn_cash), Credit = c(forest_tp_credit, forest_fn_credit, forest_fp_credit, forest_tn_credit), Debit = c(forest_tp_debit, forest_fn_debit, forest_fp_debit, forest_tn_debit), Other = c(forest_tp_other, forest_fn_other, forest_fp_other, forest_tn_other) ))
# Adding row names
rownames(forest_tf.df) = c("True Positives (TP)", "False Negatives (FN)", "False Positives (FP)", "True Negatives (TN)")
forest_tf.df
# verify columns sum up to num payments
forest_tf.df %>% summarise(across(c("Cash", "Credit", "Debit", "Other"), sum))

# the above is a column short. Add a column
(forest_tf2.df = forest_tf.df)
(forest_tf2.df$Total = NA)
forest_tf2.df

# combine 3 tables 
(forest_confusion_bind2.df = rbind(forest_confusion_bind1.df, forest_tf2.df, forest_confusion_stat6.df))

# export to LaTeX
nrow(forest_confusion_bind2.df)
ncol(forest_confusion_bind2.df)
# matrix with digits (1 extra column)
#(digit_m = matrix(2, nrow(confusion_bind2.df), ncol(confusion_bind2.df)+1))
#
#digit_m[1:8, ] = 0# zero digits
#digit_m[9:nrow(confusion_bind2.df), ] =2

print(xtable(forest_confusion_bind2.df), hline.after =c(4,8,12))

# Notes for this table
nrow(r4.df)# number of payments
nrow(r4.df[unique(r4.df$id), ])# num respondents
#
nrow(r4_train_imputed.df)# num payments in the training subsample
nrow(r4_test_imputed.df)# num payments in the testing subsample

#End: random forest confusion matrix, Table 3####

#++++++++++++

#Begin: table listing merchant types ####
### Full merchant names (not used)
## Vector of 21 merchant names (Table 1)
(merch1_name = "Grocery stores, convenience stores without gas stations, pharmacies")
(merch2_name = "Gas stations")
(merch3_name = "Sit-down restaurants and bars")
(merch4_name = "Fast food restaurants, coffee shops, cafeterias, food trucks")
(merch5_name = "General merchandise stores, department stores, other stores, online shopping")
(merch6_name = "General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.")
(merch7_name = "Arts, entertainment, recreation")
(merch8_name = "Utilities not paid to the government: electricity, natural gas, water, sewer, trash, heating oil")
(merch9_name = "Taxis, airplanes, delivery")
(merch10_name = "Telephone, internet, cable or satellite TV, video or music streaming services, movie theaters")
(merch11_name = "Building contractors, plumbers, electricians, HVAC, etc.")
(merch12_name = "Professional services: legal, accounting, architectural services; veterinarians, photographers or photo
processers")
(merch13_name = "Hotels, motels, RV parks, campsites")
(merch14_name = "Rent for apartments, homes, or other buildings, real estate companies, property managers, etc.")
(merch15_name = "Mortgage companies, credit card companies, banks, insurance companies, stock brokers, IRA funds, mutual funds, credit unions, sending remittances")
(merch16_name = "Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.")
(merch17_name = "Charitable or religious donations")
(merch18_name = "Hospital, doctor, dentist, nursing homes, etc.")
(merch19_name = "Government taxes or fees")
(merch20_name = "Schools, colleges, childcare centers")
(merch21_name = "Public transportation and tolls")
# Make names a vector
(merch_name.vec = c(merch1_name, merch2_name, merch3_name, merch4_name, merch5_name, merch6_name, merch7_name, merch8_name, merch9_name, merch10_name, merch11_name, merch12_name, merch13_name, merch14_name, merch15_name, merch16_name, merch17_name, merch18_name, merch19_name, merch20_name, merch21_name))
# Finalizing merchant name table
merch_num.vec = 1:21
(merch_name.df = data.frame(merch_num.vec, merch_name.vec))
dim(merch_name.df)

### Abbreviated 21 merchant description (
# to be used in tables (see below further merchX_abv_fig for figures)
(merch1_abv = "1. Grocery store")
(merch2_abv = "2. Gas station")
(merch3_abv = "3. Restaurant/bar")
(merch4_abv = "4. Fast food/coffee shop")
(merch5_abv = "5. General merchandise store")
(merch6_abv = "6. General service")
(merch7_abv = "7. Art/entertainment")
(merch8_abv = "8. Non-government utility")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable")
(merch11_abv = "11. Contractor/plumber/ electrician")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent")
(merch15_abv = "15. Mortgage/insurance/credit card")
(merch16_abv = "16. Person-to-person")
(merch17_abv = "17. Charitable/religious donation")
(merch18_abv = "18. Hospital/doctor/dentist")
(merch19_abv = "19. Government taxes")
(merch20_abv = "20. School/college/childcare centers")
(merch21_abv = "21. Public transport/tolls")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))

# constructing merch abv (shorter for figures) description (not used)
(merch1_abv_fig = "1. Grocery store")
(merch2_abv_fig = "2. Gas station")
(merch3_abv_fig = "3. Restaurant/bar")
(merch4_abv_fig = "4. Fast food/coffee shop")
(merch5_abv_fig = "5. General merchandise")
(merch6_abv_fig = "6. General service")
(merch7_abv_fig = "7. Art/entertainment")
(merch8_abv_fig = "8. Non-gov't utility")
(merch9_abv_fig = "9. Taxi/airplane/delivery")
(merch10_abv_fig = "10. Phone/internet/cable")
(merch11_abv_fig = "11. Contractor")
(merch12_abv_fig = "12. Professional service")
(merch13_abv_fig = "13. Hotel/motel/campsite")
(merch14_abv_fig = "14. Rent")
(merch15_abv_fig = "15. Mortg/insur/credit c")
(merch16_abv_fig = "16. Person-to-person")
(merch17_abv_fig = "17. Charitable/religious")
(merch18_abv_fig = "18. Hospital/doctor/dentist")
(merch19_abv_fig = "19. Government taxes")
(merch20_abv_fig = "20. Education/childcare")
(merch21_abv_fig = "21. Public transport/tolls")
# Make names a vector
(merch_abv_fig.vec = c(merch1_abv_fig, merch2_abv_fig, merch3_abv_fig, merch4_abv_fig, merch5_abv_fig, merch6_abv_fig, merch7_abv_fig, merch8_abv_fig, merch9_abv_fig, merch10_abv_fig, merch11_abv_fig, merch12_abv_fig, merch13_abv_fig, merch14_abv_fig, merch15_abv_fig, merch16_abv_fig, merch17_abv_fig, merch18_abv_fig, merch19_abv_fig, merch20_abv_fig, merch21_abv_fig))
#End: List of merchant types ####

#+++++++++++++++

#Begin: Table of 10 merchant types (Appendix A)####
merch_abv.vec# descriptions of 21 merch types

# Select ony the 10 merchants used in this paper
names(m9.df)
table(m9.df$merch)
nrow(m9.df)# num payments
nrow(m9.df[unique(m9.df$id), ])# num respondents
#
# merch codes for the 10 merch types
(merch_selected.vec = as.numeric(names(table(m9.df$merch))))
(merch_abv2.vec = merch_abv.vec[merch_selected.vec])
str(merch_abv2.vec)

# number of payments by merch category
(merch_num.vec = m9.df %>% group_by(merch) %>% summarise(n()))
str(merch_num.vec)
(merch_num2.vec = merch_num.vec$`n()`)

# avg payment $ value by merch type
(merch_avg_val.vec = m9.df %>% group_by(merch) %>% summarise(mean(amnt)))
(merch_avg_val2.vec = merch_avg_val.vec$`mean(amnt)`)

# median payment $ value by merch type
(merch_med_val.vec = m9.df %>% group_by(merch) %>% summarise(median(amnt)))
(merch_med_val2.vec = merch_med_val.vec$`median(amnt)`)

# max payment $ value by merch type
(merch_max_val.vec = m9.df %>% group_by(merch) %>% summarise(max(amnt)))
(merch_max_val2.vec = merch_max_val.vec$`max(amnt)`)

# min payment $ value by merch type
(merch_min_val.vec = m9.df %>% group_by(merch) %>% summarise(min(amnt)))
(merch_min_val2.vec = merch_min_val.vec$`min(amnt)`)


# construct the table
(merch.df = data.frame(Category = merch_abv2.vec, Payments = merch_num2.vec, Median = format(round(merch_med_val2.vec,2), nsmall=2), Average = format(round(merch_avg_val2.vec,2), nsmall = 2), Min = format(round(merch_min_val2.vec,2), nsmall = 2), Max = format(round(merch_max_val2.vec,2), nsmall = 2)))

# Adding a bottom row with total and stats
(merch_total_num = nrow(m9.df))
sum((merch_total_num = nrow(m9.df)))# verify sums up to total num
#
(merch_total_avg_val = format(round(mean(m9.df$amnt),2), nsmall = 2))
#
(merch_total_med_val = format(round(median(m9.df$amnt),2), nsmall = 2))
#
(merch_total_min_val = format(round(min(m9.df$amnt),2), nsmall = 2))
#
(merch_total_max_val = format(round(max(m9.df$amnt),2), nsmall = 2))
# Total row to be appended from the bottom
(merch_total.vec = c("Total", merch_total_num, merch_total_avg_val, merch_total_med_val, merch_total_min_val, merch_total_max_val))

#Append
(merch2.df = rbind(merch.df, merch_total.vec))

# LaTeX table
print(xtable (merch2.df), hline.after = c(0, 10,11), include.rownames = F)

#End: Table of 10 merch categories (Appendix A)####

#+++++++++++++++

#Begin: Classification trees, Figure 3####
names(r4_imputed.df)
dim(r4_imputed.df)# num payments

# renaming variables for a better visual display
tree_data.df = r4_imputed.df
names(tree_data.df)
tree_data2.df = tree_data.df %>% rename("Preference" = "paypref_inperson", "Merchant" = "merch", "Amount" = "amnt", "Cash_accepted" = "accept_cash", "Card_accepted" = "accept_card", "Discount" = "discount")
names(tree_data2.df)

# Model (recall)
forest_model1
# classification tree uses the same model as random forest (except change in predictor names)
(tree_model1 = pi ~ Preference + Merchant + Amount + Card_accepted + Cash_accepted +  cc_surcharge + Discount)

# Tree on entire sample (not just training) to generate Fig.2 in paper, & Not tuning to Optimal tree cp, just to demonstrate. For confusion table, see below
set.seed(1955)# to be able to reproduce the rpart CV below
tree1 = rpart(tree_model1, data = tree_data2.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
prp(tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(tree1)# plot cp: 
#names(tree1)
tree1$cptable # List cp, number of splits and errors
# Below, I choose cp to use for prunning. There are 2 methods:
#Method 1: Use the plotcp and pick the highest relative error below the dashed line => may lead to a larger tree than needed.
#Method 2 (1-SE rule, preferred): Pick the lowest xerror in the cptable. Add the corresponding xstd. Then, pick the cp in the cptable with xerror < or = to this sum.
(cp.choice = tree1$cptable[10, "CP"]) # Corresponds to 9 splits (just for demonstration)
prune1 = prune.rpart(tree1, cp=cp.choice)
# plot prunned tree
prp(prune1, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, cex = 1.1)#faclet=0 avoids abbreviations, tweak for char size
#faclet=0 avoids abbreviations, tweak for char size
#tweak also controls the fontsize (in addition or replacement for cex = 1.x, but it looks good this way)

# Information for Fig 3 (tree)
nrow(r4_imputed.df)
length(unique(r4_imputed.df$id))

#End: Classification trees, Figure 3####

#+++++++++++++++

#Begin: Appendix B transaction-specific predictors####
# Note: Some info in the text are computed at the beginning of Random Forest variable importance table
## Card accepted
# raw accept card
table(r4.df$accept_card, useNA = "always")# raw
frac_accept_card# fraction Yes
# by PI 
r4.df %>% group_by(pi) %>% summarise(count =sum(is.na(accept_card), na.rm = TRUE))
r4.df %>% group_by(pi) %>% summarise(count =sum(accept_card =="Yes", na.rm = TRUE))
r4.df %>% group_by(pi) %>% summarise(count =sum(accept_card =="No", na.rm = TRUE))
# by merchant
r4.df %>% group_by(merch) %>% summarise(count =sum(accept_card =="Yes", na.rm = TRUE))

# imputed accept card
table(r4_imputed.df$accept_card, useNA = "always")# imputed
frac_accept_card_imputed# fraction Yes
# by PI
r4_imputed.df %>% group_by(pi) %>% summarise(count =sum(accept_card =="Yes", na.rm = TRUE))
# by merchant
r4_imputed.df %>% group_by(merch) %>% summarise(count =sum(accept_card =="Yes", na.rm = TRUE))

## Cash accepted
# raw accept card
table(r4.df$accept_cash, useNA = "always")# raw
frac_accept_cash# fraction Yes
# by PI 
r4.df %>% group_by(pi) %>% summarise(count =sum(accept_cash =="Yes", na.rm = TRUE))
r4.df %>% group_by(pi) %>% summarise(count =sum(accept_cash =="No", na.rm = TRUE))
# by merchant
r4.df %>% group_by(merch) %>% summarise(count =sum(accept_card =="Yes", na.rm = TRUE))

# imputed accept cash
table(r4_imputed.df$accept_cash, useNA = "always")# imputed
frac_accept_cash_imputed# fraction Yes
# by PI
r4_imputed.df %>% group_by(pi) %>% summarise(count =sum(accept_cash =="Yes", na.rm = TRUE))
# by merchant
r4_imputed.df %>% group_by(merch) %>% summarise(count =sum(accept_cash =="Yes", na.rm = TRUE))

## Credit card surcharge
# raw cc_surcharge
table(r4.df$cc_surcharge, useNA = "always")# raw
frac_cc_surcharge# fraction Yes # by PI 
r4.df %>% group_by(pi) %>% summarise(count =sum(cc_surcharge =="Yes", na.rm = TRUE))
r4.df %>% group_by(pi) %>% summarise(count =sum(cc_surcharge =="No", na.rm = TRUE))
# by merchant
r4.df %>% group_by(merch) %>% summarise(count =sum(cc_surcharge =="Yes", na.rm = TRUE))
# NA by PI
r4.df %>% group_by(pi) %>% summarise(count =sum(is.na(cc_surcharge), na.rm = TRUE))

# imputed cc_surcharge
table(r4_imputed.df$cc_surcharge, useNA = "always")# imputed
frac_cc_surcharge_imputed# fraction Yes
# by PI
r4_imputed.df %>% group_by(pi) %>% summarise(count =sum(accept_cash =="Yes", na.rm = TRUE))
# by merchant
r4_imputed.df %>% group_by(merch) %>% summarise(count =sum(accept_cash =="Yes", na.rm = TRUE))

## Discount
# raw discount
table(r4.df$discount, useNA = "always")# raw
frac_discount# fraction Yes # by PI 
r4.df %>% group_by(pi) %>% summarise(count =sum(discount =="Yes", na.rm = TRUE))
r4.df %>% group_by(pi) %>% summarise(count =sum(discount =="No", na.rm = TRUE))
# by merchant
r4.df %>% group_by(merch) %>% summarise(count =sum(discount =="Yes", na.rm = TRUE))
# NA by PI
r4.df %>% group_by(pi) %>% summarise(count =sum(is.na(discount), na.rm = TRUE))

# imputed discount
table(r4_imputed.df$discount, useNA = "always")# imputed
frac_discount_imputed# fraction Yes
# by PI
r4_imputed.df %>% group_by(pi) %>% summarise(count =sum(discount =="Yes", na.rm = TRUE))
# by merchant
r4_imputed.df %>% group_by(merch) %>% summarise(count =sum(discount =="Yes", na.rm = TRUE))

#End: Appendix B transaction-specific predictors####

#+++++++++++++++

#Begin: Misc calculations####
# Footnote to the model equation: I explain why cc_adopt and dc_adopt are not in the model. Here is how:
#
# num respo who have credit card and prefer credit card
nrow(subset(i2.df, paypref_inperson == 3 & cc_adopt==1))
#
# num respo who have credit card and do not prefer credit card
nrow(subset(i2.df, paypref_inperson != 3 & cc_adopt==1))
# Below, reported in the model footnote
# num respo who don't have credit card and prefer credit card
nrow(subset(i2.df, paypref_inperson == 3 & cc_adopt!=1))
#
# num respo who have debit card and prefer debit card
nrow(subset(i2.df, paypref_inperson == 4 & dc_adopt==1))
#
# num respo who have debit card and do not prefer debit card
nrow(subset(i2.df, paypref_inperson != 4 & dc_adopt==1))
# Below, reported in the model footnote
# num respo who don't have debit card and debit credit card
nrow(subset(i2.df, paypref_inperson == 4 & dc_adopt!=1))

#End: Misc calculations####

