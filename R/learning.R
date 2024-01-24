
# Loading packages --------------------------------------------------------

library(tidyverse)
#This will be used for testing out Git

#newchange
library(NHANES)
glimpse(NHANES)
str
str(NHANES)

# Select specific columns -------------------------------------------------

select(NHANES,age)
select(NHANES,Age,Weight,BMI)
select(NHANES,-HeadCirc)
select(NHANES,starts_with("BP"))
select(NHANES,ends_with("Day"))
select(NHANES,contains("Age"))


# Rename all columns ------------------------------------------------------

rename_with(NHANES,snakecase::to_snake_case)
NHANES_small <- rename_with(NHANES,snakecase::to_snake_case)

# Renaming specific columns -----------------------------------------------

rename(NHANES,sex = gender)
rename(NHANES,sex = Gender)
rename(NHANES,sex = gender)
rename(NHANES_small,sex = gender)
NHANES_small <- rename(NHANES_small, sex = gender)

# Chaining the function with the pipe -------------------------------------
colnames(NHANES_small)
NHANES_small %>%
    colnames()
NHANES_phys <- NHANES_small %>%
    select(phys_active) %>%
    rename(physically_active = phys_active)
NHANES_small %>%
    select(bp_sys_ave,education)
NHANES_small %>%
    rename(bp_sys = bp_sys_ave,
           bp_dia = bp_dia_ave)
select(nhanes_small, bmi, contains("age"))
NHANES_small %>%
    select(bmi, contains("age"))
blood_pressure <- select(nhanes_small, starts_with("bp_"))
rename(blood_pressure, bp_systolic = bp_sys_ave)

NHANES_small %>%
    select(starts_with("bp_")) %>%
    rename(bp_systolic = bp_sys_ave)


# Filtering data by row ---------------------------------------------------

filter(NHANES_small,phys_active=="No")

NHANES_small %>%
    filter(phys_active=="No")
NHANES_small %>%
    filter(phys_active !="No")

NHANES_small %>%
    filter(bmi==25)
NHANES_small %>%
    filter(bmi>=25)

TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

TRUE | TRUE
TRUE | FALSE
FALSE | FALSE
NHANES_small %>%
    filter(bmi == 25 & phys_active == "No") %>%
    select(bmi,phys_active)
NHANES_small %>%
    filter(bmi == 25 | phys_active == "No") %>%
    select(bmi, phys_active)


# Arranging the rows ------------------------------------------------------

NHANES_small %>%
    arrange(age)
NHANES_small %>%
    arrange(education) %>%
    select(education)

NHANES_small %>%
    arrange(desc(age)) %>%
    select(age)
NHANES_small %>%
    arrange(age,education)

# Transform or add columns ------------------------------------------------

NHANES_small %>%
    mutate(age = age * 12,
           logged_bmi = log(bmi)) %>%
    select(age, logged_bmi)

NHANES_small %>%
    mutate(
        old = if_else(age >= 30, "Yes", "No")
    ) %>%
    select(old)

# Code section ------------------------------------------------------------

    (DBP = bp_dia_ave and SBP = bp_sys_ave) to calculate Mean Arterial Pressure. Hint: In R, use + to add, * to multiply, and / to divide.
# 1. BMI between 20 and 40 with diabetes
NHANES_small %>%
    filter(bmi >= 20 & bmi <= 40 & diabetes == "Yes")

nhanes_modified <- NHANES_small %>%
    mutate(
        mean_arterial_pressure = ((2*bp_dia_ave) + bp_sys_ave / 3),
        young_child = if_else(age < 6, "Yes", "No")
    )

nhanes_modified
nhanes_modified

# Calculating summary statistics ------------------------------------------

result <- NHANES_small %>%
    summarise(max_bmi = max(bmi,na.rm=TRUE),
              min_bmi = min(bmi,na.rm = TRUE))

# Summary statistics by a group -------------------------------------------

NHANES_small %>%
    group_by(diabetes) %>%
    summarise(mean_age=mean(age, na.rm = TRUE),
              mean_bmi=mean(bmi, na.rm = TRUE))
NHANES_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes) %>%
    summarise(mean_age = mean(age, na.rm = TRUE),
              mean_bmi = mean(bmi, na.rm = TRUE))
NHANES_small %>%
    filter(!is.na(diabetes)) %>%
    group_by(diabetes,phys_active) %>%
    summarise(mean_age = mean(age, na.rm = TRUE),
              mean_bmi = mean(bmi, na.rm = TRUE)) %>%
    ungroup()

# Saving dataset as files -------------------------------------------------

readr::write_csv(NHANES_small,
                 here::here("data/NHANES_small.csv"))
