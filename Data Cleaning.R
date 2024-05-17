options(repos = c(CRAN = "https://cran.r-project.org"))


#Setting up the working directory
setwd("C:/Users/user/Desktop")


#Installing required packages
install.packages("foreign") 
install.packages("dplyr")
install.packages("readr") 
install.packages("rio")
install.packages("crayon") 
install.packages("haven")


#Loading required packages into the current session
require(foreign)
require(haven)
require(dplyr)
require(readxl)
require(readr)
require(rio)



# Importing synthetic depression missing data data-set
library(readr)
sdmd <- read_csv("C:/Users/user/Downloads/synthetic_depression_missing_data.csv")
View(sdmd)


#Attaching the data set to avoid typing out the name of the data set especially in complex code
attach(sdmd)

View(sdmd)



#Viewing data structure
str(sdmd)
table(stress_t1)
table(stress_t2)
table(stress_t3)
table(mh_score_t1)
table(mh_score_t2)
table(mh_score_t3)
table(t1)
table(t2)
table(t3)
table(depression_t1)
table(depression_t2)
table(depression_t3)
table(antidepressant_dt_EHR)
table(antidepressant_dt_self)
summary(antidepressant_dt_EHR)
summary(antidepressant_dt_self)




#Employement variable
#Checking for inconsistencies in employment_t1 variable
table(sdmd$employment_t1)


#Correcting the typo error in the employment t1 variable
sdmd$employment_t1[sdmd$employment_t1=="uneemployed"] <- "unemployed"
sdmd$employment_t1[sdmd$employment_t1=="eemployed"]<- "employed"

#To view the variable employment t1 after correcting the typo
table(sdmd$employment_t1, useNA = "ifany")

#To view the missing values in the employment_t1 variable
sum(is.na(sdmd$employment_t1))



#Checking for inconsistencies in employment_t2 variable
table(sdmd$employment_t2)


#Correcting the typo error in the employment t2 variable
sdmd$employment_t2[sdmd$employment_t2=="uneemployed"] <- "unemployed"
sdmd$employment_t2[sdmd$employment_t2=="eemployed"]<- "employed"

#To view the variable employment t2 after correcting the typo, and any missing data
table(sdmd$employment_t2,useNA = "ifany")#shows that 1038 of the entries are missing
et1pctmiss <- (1038/(8455+507+1038))*100
et1pctmiss#Finds the percentage of missing data. #Can therefore replace with the mode. 

#Checking for inconsistencies in employment_t3 variable
table(sdmd$employment_t3)



#Correcting the typo error in the employment t3 variable
sdmd$employment_t3[sdmd$employment_t3=="uneemployed"] <- "unemployed"
sdmd$employment_t3[sdmd$employment_t3=="eemployed"]<- "employed"

#To view the variable employment t3 after correcting the typo
table(sdmd$employment_t3, useNA = "ifany")


#Creating a anew variable, employement_t1_cat and recoding unemployed to 0, employed to 1
table(sdmd$employment_t1)
sdmd$employment_t1_cat<- NA
sdmd$employment_t1_cat[sdmd$employment_t1=="employed"]<-1
sdmd$employment_t1_cat[sdmd$employment_t1=="unemployed"]<-0
table(sdmd$employment_t1_cat)

#Creating a anew variable, employement_t2_cat and recoding unemployed to 0, employed to 1
table(sdmd$employment_t2)
sdmd$employment_t2_cat<- NA
sdmd$employment_t2_cat[sdmd$employment_t2=="employed"]<-1
sdmd$employment_t2_cat[sdmd$employment_t2=="unemployed"]<-0
table(sdmd$employment_t2_cat, useNA = "ifany")


#Creating a anew variable, employment_3_cat and re-coding unemployed to 0, employed to 1
table(sdmd$employment_t3)
sdmd$employment_t3_cat<- NA
sdmd$employment_t3_cat[sdmd$employment_t3=="employed"]<-1
sdmd$employment_t3_cat[sdmd$employment_t3=="unemployed"]<-0
table(sdmd$employment_t3_cat)

#Saving the new variables as categorical variables with two levels, 0 and 1
sdmd$employment_t1_cat = factor(sdmd$employment_t1_cat,levels = c(0,1))
sdmd$employment_t2_cat = factor(sdmd$employment_t2_cat,levels = c(0,1))
sdmd$employment_t3_cat = factor(sdmd$employment_t3_cat,levels = c(0,1))
View(sdmd)


#Replacing missing values in the employement variables

#employment_t2_cat
#To view the variable employment_t2_cat andany missing data
table(sdmd$employment_t2_cat, useNA = "ifany")#shows that 1038 of the entries are missing
et2pctmiss <- (1038/(8455+507+1038))*100
et2pctmiss#Finds the percentage of missing data-10.38% #Can therefore replace with the mode.  
#Replacing missing values with the mOde
sdmd$employment_t2_cat[is.na(sdmd$employment_t2_cat)] <- levels(sdmd$employment_t2_cat)[which.max(table(sdmd$employment_t2_cat))]
table(sdmd$employment_t2_cat, useNA = "ifany")

#employment_t3_cat
#To view the variable employment_t3_cat and any missing data
table(sdmd$employment_t3_cat, useNA = "ifany")#shows that 990 of the entries are missing
et3pctmiss <-(990/(514+8496+990))*100
et3pctmiss#Finds the percentage of missing data.-9.9% #Can therefore replace with the mode.  
#Replacing missing values with the mode
sdmd$employment_t3_cat[is.na(sdmd$employment_t3_cat)] <- levels(sdmd$employment_t3_cat)[which.max(table(sdmd$employment_t3_cat))]
table(sdmd$employment_t3_cat, useNA = "ifany")
View(sdmd)

#Replacing the missing values in the income variables
table(sdmd$employment_t1, useNA = "ifany")#income_t1 variable had no missing variables

#Income_t2 variable
summary(sdmd$income_t2)
table(sdmd$employment_t2, useNA = "ifany")#Shows that income_t2 has 1038 missing values
hist(sdmd$income_t2)#shows that the data is skewed to the right
#Therefore to replace missing data, will use median
sdmd$income_t2[is.na(sdmd$income_t2)] <- median(sdmd$income_t2, na.rm = TRUE)#replaces the missing data with the median

#Income_t3 variable
summary(sdmd$income_t3)#median of32000, mean of 35130
table(sdmd$employment_t3, useNA = "ifany")#Shows that income_t3 has 990 missing values
hist(sdmd$income_t3)#shows that the data is skewed to the right
#Therefore to replace missing data, will use median
sdmd$income_t3[is.na(sdmd$income_t3)] <- median(sdmd$income_t3, na.rm = TRUE)#replaces the missing data with the median
summary(sdmd$income_t3)#new mean of 34820
sdmd








#Stress variable
#Creating new variable stress_t1_cat

sdmd$stress_t1_cat<- NA#creates a new variable
sdmd$stress_t1_cat[sdmd$stress_t1=="stressed"]<-1#assignsa code for the stresses as 1
sdmd$stress_t1_cat[sdmd$stress_t1=="not_stressed"]<-0# assigns a code for the not stressed at 0
sdmd$stress_t1_cat#vieiwng the variable entries
table(sdmd$stress_t1_cat, useNA = "ifany")#checking for any missing valuea

#Creating new variable stress_t2_cat

sdmd$stress_t2_cat<- NA#creates a new variable
sdmd$stress_t2_cat[sdmd$stress_t2=="stressed"]<-1#assigns a code for the stresses as 1
sdmd$stress_t2_cat[sdmd$stress_t2=="not_stressed"]<-0# assigns a code for the not stressed at 0
sdmd$stress_t2_cat#viewing the variable entries
table(sdmd$stress_t2_cat, useNA = "ifany")#checking for any missing values, shows that there are 1038 missing values
#Replacing the missing values with the mode
sdmd$stress_t2_cat[is.na(sdmd$stress_t2_cat)] <- sdmd$stress_t2_cat[which.max(table(sdmd$stress_t2_cat))]
sdmd$stress_t2_cat

#Creating new variable stress_t3_cat

sdmd$stress_t3_cat<- NA#creates a new variable
sdmd$stress_t3_cat[sdmd$stress_t2=="stressed"]<-1#assigns a code for the stresses as 1
sdmd$stress_t3_cat[sdmd$stress_t2=="not_stressed"]<-0# assigns a code for the not stressed at 0
sdmd$stress_t3_cat#viewing the variable entries
table(sdmd$stress_t3_cat, useNA = "ifany")#checking for any missing values, shows that there are 1038 missing values
#Replacing the missing values with the mode
sdmd$stress_t3_cat[is.na(sdmd$stress_t3_cat)] <- sdmd$stress_t3_cat[which.max(table(sdmd$stress_t3_cat))]
sdmd$stress_t3_cat


##Depression
#Creating new variables fr the depression t1
unique(depression_t1)
sdmd$depression_t1_cat<- NA#creates a new variable
sdmd$depression_t1_cat[sdmd$depression_t1=="depressed"]<-1#assigns code for the depressed as 1
sdmd$depression_t1_cat[sdmd$depression_t1=="not_depressed"]<-0# assigns a code for the not depressed at 0
sdmd$depression_t1_cat#vieiwng the variable entries
table(sdmd$depression_t1_cat, useNA = "ifany")#checking for any missing values

#Creating new variable depression_t2_cat

#Creating new variables fr the depression t1
unique(depression_t2)
sdmd$depression_t2_cat<- NA#creates a new variable
sdmd$depression_t2_cat[sdmd$depression_t2=="depressed"]<-1#assignsa code for the depressed as 1
sdmd$depression_t2_cat[sdmd$depression_t2=="not_depressed"]<-0# assigns a code for the not depressed at 0
sdmd$depression_t2_cat#viewing the variable entries
table(sdmd$depression_t2_cat, useNA = "ifany")#checking for any missing values and they are 1038
#Replacing the missing values with the mode
sdmd$depression_t2_cat[is.na(sdmd$depression_t2_cat)] <- sdmd$depression_t2_cat[which.max(table(sdmd$depression_t2_cat))]
#Printing the sdmd$depression_t2_cat
table(sdmd$depression_t2_cat)

#Creating new variable stress_t3_cat

unique(depression_t3)
sdmd$depression_t3_cat<- NA#creates a new variable
sdmd$depression_t3_cat[sdmd$depression_t3=="depressed"]<-1#assigns a code for the depressed as 1
sdmd$depression_t3_cat[sdmd$depression_t3=="not_depressed"]<-0# assigns a code for the not depressed at 0
sdmd$depression_t3_cat#viewing the variable entries
table(sdmd$depression_t3_cat, useNA = "ifany")#checking for any missing values and they are 990
#Replacing the missing values with the mode
sdmd$depression_t3_cat[is.na(sdmd$depression_t3_cat)] <- sdmd$depression_t3_cat[which.max(table(sdmd$depression_t3_cat))]
sdmd$depression_t3_cat















##Time variable
#t1
table(sdmd$t1, useNA= "ifany")

#t2
hist(sdmd$t2)
summary(sdmd$t2)
is.na(sdmd$t2)
table(sdmd$t2, useNA = "ifany")#Shows that there are 1038 missing values.
#Because the variable is normally distributed, we replace with the median
sdmd$t2[is.na(sdmd$t2)] <- mean(sdmd$t2, na.rm = TRUE)#Replaces with the median value
sdmd$t2



#t3
hist(sdmd$t3)
summary(sdmd$t3)
is.na(sdmd$t3)
table(sdmd$t3, useNA = "ifany")#Shows that there are 990 missing values.
#Because the variable is normally distributed, we replace with the median
sdmd$t3[is.na(sdmd$t3)] <- mean(sdmd$t3, na.rm = TRUE)#Replaces with the median value
sdmd$t3






##Mental health score
is.na(sdmd$mh_score_t1)
sum(is.na(sdmd$mh_score_t3))
hist(sdmd$mh_score_t1)
hist(sdmd$mh_score_t2)
hist(sdmd$mh_score_t3)

#Converts mental health score t1 to factor
sdmd$mh_score_t1<- as.factor(sdmd$mh_score_t1)
class(sdmd$mh_score_t1)
# Check the levels of the factor
levels(sdmd$mh_score_t1)

#Converts mental health score t2 to factor
sdmd$mh_score_t2<- as.factor(sdmd$mh_score_t2)
class(sdmd$mh_score_t2)
# Check the levels of the factor
levels(sdmd$mh_score_t2)
table(sdmd$mh_score_t2, useNA = "ifany")#Shows that the mode for this variable is "2", with 1038 missing values
#To replace the missing values with the mode
sdmd$mh_score_t2[is.na(sdmd$mh_score_t2)] <- 2
table(sdmd$mh_score_t2, useNA = "ifany")

#Converts mental health score t3 to factor
sdmd$mh_score_t3<- as.factor(sdmd$mh_score_t3)
class(sdmd$mh_score_t3)
# Check the levels of the factor
levels(sdmd$mh_score_t3)
table(sdmd$mh_score_t3, useNA = "ifany")#Shows that the mode for this variable is "1", with 990 missing values
#To replace the missing values with the mode
sdmd$mh_score_t3[is.na(sdmd$mh_score_t3)] <- 1
table(sdmd$mh_score_t3, useNA = "ifany")



table(sdmd$mh_score_t2, useNA = "ifany")

table(sdmd$mh_score_t1, useNA = "ifany")
table(sdmd$mh_score_t2, useNA = "ifany")
table(sdmd$mh_score_t3, useNA = "ifany")
mean(sdmd$mh_score_t1)






#####Rearranging the order of the variables while taking are of the redundancies. 
Completed_sdmd <- sdmd %>%
  select(ID, sexcat, age: income_t3, t1: t3, employment_t1_cat:depression_t3_cat)
print(Completed_sdmd)
View(Completed_sdmd)










