#upload data files 
library(readxl)
age_demo <- read_excel("age_demo.xlsx")
View(age_demo)
library(readxl)
education_demo <- read_excel("education_demo.xlsx")
View(education_demo)
library(readxl)
employment_demo <- read_excel("employment_demo.xlsx")
View(employment_demo)

#Extract columns from age_demo with occupation, total, and average
age <- age_demo[c(1, 2, 10)]

#rename columns in age_demo
names(age) <- c("Occupations", "Total (in thousands)", "Median Age")

#remane columns in education_demo

#rename columns in employment_demo 

#aggregate rows in employment_demo

#separate ethnicity and gender into two tables 
