#reading in HR2020 SAS dataset.
install.packages("haven");library(haven)
setwd("/Users/kevinledezma/Downloads/h20sas")
#today (31 mar. 11:30 to 1:30) we will locate our variables (RB00 for life sats, our outcome; D101, 120, 151 and 157 for cognitive impairment, our exposures)  -of interest and so some exploratory data analysis.
outcome<-read_sas("h20b_r.sas7bdat") #here is our outcome.
life_sat_cat <- ifelse(outcome$RB000 %in% c(1,2), 0, 
                       ifelse(outcome$RB000 %in% 3, 1, 
                              ifelse(outcome$RB000 %in% c(4,5), 1,
                                     ifelse(outcome$RB000 %in% c(8,9), NA,)
# 1 o the life_sat_cat indicates completely satisfied, 2 indicates somewhat satisfied, 3 indicates not satisfied, 9 indicates unknown/refused.
table(life_sat_cat)
library(ggplot2)

# create a data frame with the counts of each category
life_sat_counts <- data.frame(table(life_sat_cat))

# create a bar chart with a logarithmic y-axis
ggplot(data = outcome, aes(x = factor(life_sat_cat))) +
  geom_bar() +
  labs(title = "Life Satisfaction Categories", x = "Life Satisfaction", y = "Frequency") 
hist(life_sat_cat)
#now we will combine the D block of exposures, we will combine them into a COG-IMP score, like. tics score (Questions of orientation, repetition, naming, and calculations are some of the items covered, and for the HRS2020 dataset it is repersented as.... continuous NS D124, D129, & D151
#THROUGH D158. A TICS scores instead- D170. A SCORE IS TABULATED BY BLAISE BASED ON RESPONSES TO QUESTIONS D124, D129, & D151
#THROUGH D158. A SCORE OF 0-10 IS GIVEN BASED ON THE NUMBER OF CORRECT ANSWERS AND
#ELAPSED TIME TO ANSWER. NOTE THAT NOT ALL RESPONDENTS ARE ASKED D151 THROUGH 158

exposure<-read_sas("/Users/kevinledezma/Downloads/h20sas/h20d_r.sas7bdat")
TICS_score <- ifelse(exposure$RD124 %in% 1, 0, 
                     ifelse(exposure$RD124 %in% 5, 1,
                            ifelse(exposure$RD124 %in% c(6,9), NA,
                                   ifelse(exposure$RD129 %in% 1, 0,
                                          ifelse(exposure$RD129 %in% 5, 1,
                                                 ifelse(exposure$RD129 %in% 9, NA,
                                                        ifelse(exposure$RD151 %in% 1, 0, 
                                                               ifelse(exposure$RD151 %in% 5, 1, 
                                                                      ifelse(exposure$RD151 %in% c(8,9), NA,
                                                                             ifelse(exposure$RD152 %in% 1, 0,
                                                                                    ifelse(exposure$RD152 %in% 5, 1,
                                                                                           ifelse (exposure$RD152 %in% c(8,9), NA,
                                                                                                   ifelse(exposure$RD153 %in% 1,0,
                                                                                                          ifelse(exposure$RD153 %in% 5,1,
                                                                                                                 ifelse(exposure$RD153 %in% c(8,9), NA,
                                                                                                                        ifelse(exposure$RD154 %in% 1, 0,
                                                                                                                               ifelse(exposure$RD155 %in% 5, 1,
                                                                                                                                      ifelse(exposure$RD156 %in% c(8,9), NA,
                                                                                                                                             ifelse(exposure$RD157 %in% 1, 0,
                                                                                                                                                    ifelse(exposure$RD157 %in% 5, 1,
                                                                                                                                                           ifelse(exposure$RD157 %in% c(8,9), NA,
                                                                                                                                                                  ifelse(exposure$RD158 %in% 1,0,
                                                                                                                                                                         ifelse(exposure$RD158 %in% 5,1,
                                                                                                                                                                                ifelse(exposure$RD158 %in% c(8,9),NA, NA))))))))))))))))))))))))
                                                 
#for the TICS score, 0 indicates no impairement, 1 indictes  impairment, 8 indicates unknown or refused.
print(TICS_score)


# start 3 april 8:40, we will do chi-sq test and adjust/control. 
#EDA of response (cog_imp as TICS_score)   
hist(TICS_score) #a lot of 1's which is good!
freq_table <- table(TICS_score)
barplot(freq_table, main = "TICS_score Distribution", xlab = "TICS_score Categories", ylab = "Frequency")
summary(TICS_score)
#exploratory data analysis of the response var

#crude model ; merge data!
new_data<-merge(outcome, exposure)

 #our covars are self-tested memory(RD101), drugs use(RB117), depression (RB116)symptoms
cont_table <- table(TICS_score, life_sat_cat)
chi_model<-chisq.test(cont_table)
print(chi_model)

#new model time! #stop here 12:20
self_tested_memory<- ifelse(new_data$RD101 %in% c(1,2), 0, 
                                  ifelse(new_data$RD101 %in% c(3,4), 1, 
                                         ifelse(new_data$RD101 %in% 5, 1,
                                                ifelse(new_data$RD101 %in% c(8,9),NA, NA))))
#re-construct the self-tested memory variable. 1, 2 now mean 0 or excellent memory, and 3,4,5 now means 1 or poor memory. 8 and 9 now mean unknown or refused. to test poor memeory 
drugs_use <- ifelse(new_data$RB117 %in% 5, 0, 
                                         ifelse(new_data$RB117 %in% 1, 1, 
                                                ifelse(new_data$RB117 %in% c(8,9), NA, NA)))
#re-construct the alcohol and drugs use before 16 variable. 5 now means no or 0, 1 means yes or 1, NA means unknown or refused.to test positive drugs and alcohol problems before 16.
depression_symp<- ifelse(new_data$RB116 %in% 5, 0, 
                         ifelse(new_data$RB116 %in% 1, 1, 
                                ifelse(new_data$RB116 %in% c(8,9), NA, NA)))
#re-construct the depression before 16 var. 5 now means no or 0, 1 now means yes (or 1), 8 and 9 now means unknown or refused.to test if yes to depression symptoms before 16.
new_data$life_sat_cat <- as.factor(life_sat_cat)
new_data$TICS_score<-as.factor(TICS_score)
new_data$depression_symp <- as.factor(depression_symp)
new_data$drugs_use <- as.factor(drugs_use)
new_data$self_tested_memory<-as.factor(self_tested_memory)
#new_crude <- glm(TICS_score ~  life_sat_cat, #crude model!
                 #data = new_data, family = "binomial"(link=logit), na.action = na.exclude)
new_crude <- glm(TICS_score ~  life_sat_cat, #crude model!
                 data = new_data, family = "binomial"(link=logit))
#adjusted model, adjust for depression symptoms and drugs use before 16, and includes self-tested memory.
#adj_model<-glm(TICS_score~life_sat_cat+depression_symp+drugs_use+self_tested_memory, data=new_data, family="binomial"(link=logit), na.action=na.exclude())

adj_model<-glm(TICS_score~life_sat_cat+depression_symp+drugs_use+self_tested_memory, data=new_data, family="binomial"(link=logit))
summary(new_crude)
summary(adj_model)
#now to create our table one by life satsifcation
library(dplyr)
new_data <- new_data %>% 
     mutate(drugs_use = case_when(
         drugs_use == 0 ~ "Yes",
         drugs_use == 1 ~ "No",
         drugs_use == 8~ NA,
        TRUE ~ NA_character_
       ),
       depression_symp = case_when(
           depression_symp == 0 ~ "Yes",
           depression_symp == 1 ~ "No",
           depression_symp == 8~NA,
           TRUE ~ NA_character_
         ),
       life_sat_cat = case_when(
           life_sat_cat == 0 ~ "Completely Satsified",
          life_sat_cat == 1 ~ "Not Satisified",
           life_sat_cat == 8 ~ NA,
           TRUE ~ NA_character_
         ),
      self_tested_memory=case_when(
           self_tested_memory==0~"Good Memory",
           self_tested_memory==1~"Poor Memory",
           self_tested_memory==8~NA,
           TRUE~ NA_character_
         ),
       TICS_score=case_when(
           TICS_score==0~"No Impairment",
           TICS_score==1~"Mild Impairment",
           TICS_score==8~NA,
           TRUE~NA_character_
        ))
vars<-c("depression_symp", "drugs_use", "self_tested_memory", "life_sat_cat")
tableOne <- CreateTableOne(vars = vars, strata = c("TICS_score"), smd= TRUE,data = new_data)
tableOne

confint(new_crude, level=0.95)
confint(adj_model, level=0.95)
exp(coef(new_crude))
exp(coef(adj_model))
new_crude_coef <- exp(coef(new_crude))
new_crude_ci <- exp(confint(new_crude))
adj_model_coef <- exp(coef(adj_model))
adj_model_ci <- exp(confint(adj_model))
coef_est <- exp(coef(adj_model))
conf_int <- confint(adj_model, level = 0.95)
table_df <- data.frame(variable = names(coef_est), Odds_Ratio = coef_est, 
                       lower_CI = conf_int[, 1], upper_CI = conf_int[, 2])
kable(table_df, format = "html", align = "c",
      caption = "Adjusted Odds Ratios and 95% Confidence Intervals") %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped", "hover", "condensed")) #table of odds ratios!


#adjusted model coefficent plot- graphic
coef_data <- data.frame(variable = c("Intercept", names(coef(adj_model))[-1]),
                                                estimate = coef(adj_model),
                                                 lower_ci = confint(adj_model)[, 1],
                                                 upper_ci = confint(adj_model)[, 2])

 ggplot(coef_data, aes(x = reorder(variable, estimate), y = estimate)) +
       geom_point() +
       geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
       coord_flip() +
       labs(x = "Variable", y = "Coefficient (log-odds)", 
                       title = "Coefficient Plot (Adjusted Model)") +
       theme_classic()
 #crude model coefficient plot-graphic 
coef_data <- data.frame(variable = c("Intercept", names(coef(new_crude))[-1]),
                                               estimate = coef(new_crude),
                                                lower_ci = confint(new_crude)[, 1],
                                                upper_ci = confint(new_crude)[, 2])
ggplot(coef_data, aes(x = reorder(variable, estimate), y = estimate)) +
      geom_point() +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
       coord_flip() +
       labs(x = "Variable", y = "Coefficient (log-odds)", 
                     title = "Coefficient Plot (Crude Model)") +
       theme_classic()
library(broom)
vif(new_crude)
library(report)
report(adj_model)
#assess for confounding 
#exp(coef(new_crude))=1.08

dep_model<-glm(TICS_score~life_sat_cat+depression_symp, data=new_data, family="binomial"(link=logit))
summary(dep_model)
dep_int <- exp(coef(dep_model)) #1.08-1.65/1.08=76%, confonding by depression
dru_model<-glm(TICS_score~life_sat_cat+depression_symp+drugs_use, data=new_data, family="binomial"(link=logit))
summary(dru_model)
dru_int <- exp(coef(dru_model)) #1.08-1.60/1.08=60%, confounding by drugs use
self_model<-glm(TICS_score~life_sat_cat+depression_symp+drugs_use+self_tested_memory, data=new_data, family="binomial"(link=logit))
summary(self_model)
self_int<-exp(coef(self_model)) #1.08-1.71=63% confounding 
plot(adj_model)