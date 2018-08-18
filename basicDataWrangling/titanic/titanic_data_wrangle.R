#load library
library(tidyr)
library(dplyr)

#load data into a data frame
titanic_df<-read.csv("titanic_original.csv")
titanic<-as_tibble(titanic_df)
glimpse(titanic)

#Fill in missing values for port of embarkation
titanic%>%
  select(embarked)%>%
  summary()
  
titanic<-titanic%>%
  mutate(embarked= gsub("^$", "S", embarked))

titanic%>%
  select(embarked)%>%
  unique()

#Fill in missing age values
titanic%>%
  select(age)%>%
  summary

titanic<-titanic%>%
  mutate(age= replace_na(titanic$age, mean(age, na.rm = T)))

titanic%>%
  select(age)%>%
  summary

#Fill in blank lifeboat values with "None"

titanic<-titanic %>%
  mutate(boat= gsub("^$", "none", boat))

titanic%>%
  select(boat)%>%
  unique()

#Fill in cabin numbers
titanic%>%
  select(cabin)%>%
  summary()

titanic<-titanic%>%
  mutate(has_cabin_number=if_else(grepl("^$",cabin),0, 1))

summary(titanic$has_cabin_number)

#write to new excel
write.csv(titanic,"titanic_clean.csv")
         