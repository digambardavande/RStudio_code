# RStudio_code
Data Wrangling

Do it Yourself-1

smoking data
{r}
library(dplyr)
library(tidyverse)
library(openintro)
smoking_data<-openintro::smoking
names(smoking_data)
select(smoking_data,gender,age,smoke)
select(smoking_data,gross_income,amt_weekends,amt_weekdays,age)
arrange(smoking_data,-age)
smoking_data_selected<-select(smoking_data,-ethnicity,-nationality)  

Do it Yourself-2

{r}
rename(smoking_data,education=highest_qualification)
mutate(smoking_data,total_smoke=amt_weekends+amt_weekdays)


Do it Yourself-3

{r}
filter(smoking_data,gender=="Male")
filter(smoking_data,age==35 & smoke=="No")
filter(smoking_data,marital_status!="Divorced",nationality!="English")
filter(smoking_data,age<=20 & smoke=="Yes")
filter(smoking_data,highest_qualification=="No Qualification" & smoke=="Yes")
filter(smoking_data,region=="London"|region=="Wales")


Do it Yourself-4

{r}
names(smoking_data)
smoking_data|>
   select(highest_qualification,amt_weekdays)|>
   rename(education=highest_qualification,weekdays=amt_weekdays)|>
   group_by(education)|>
   summarise(total_cigar=sum(weekdays,na.rm = T))

smoking_data|>
  filter(age<30,smoke=="Yes")|>
  arrange(-age)

smoking_data|>
   select(gross_income,amt_weekends)|>
   rename(income=gross_income)|>
   group_by(income)|>
   summarise(income=sum(amt_weekends,na.rm = T))

smoking_data|>
  group_by(gender)|>
  summarise(mean_age=mean(age))


Do it Yourself-5

{r}
smoking_data|>
  mutate(age_category=case_when(
    age>=15 & age<25 ~ "Lower age group ",
    age>=26 & age<40 ~ "Middle age group",
    age>=40 & age<59 ~ "Higher age group",
    age>=59 ~ "59 +more"))

smoking_data|>
  mutate(gender_category=case_when(
    gender==1 ~"Female",
    gender==0 ~"Male"))

smoking_data|>
  mutate(amt_weekends_category=case_when(
    amt_weekends>30 ~ "High",
    amt_weekends>=11 & amt_weekends>=30 ~"Medium",
    amt_weekends>=0  & amt_weekends<10 ~"Low"))


Thank You
