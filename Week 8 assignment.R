# week 8 assignment -------------------------------------------------------


# load library ------------------------------------------------------------

library(tidyverse)
library(janitor)


# load data ---------------------------------------------------------------

COVES_22<-read_csv(file = "FY22 COVES_values.csv")

colnames(COVES_22)

# create a long dataset containing respondents' racial demographic info and rating vars

COVES_22 %>%
  select("ResponseId", "NPS_GROUP", "OER", 
         "race_amind_alask", "race_asian", "race_afram", "race_hisp_lat" , 
         "race_haw_pacis", "race_white", "race_other", "race_notsay") %>%
  head() #view relevant vars before transformation

COVES_22_race <- COVES_22 %>%
  select("ResponseId", "NPS_GROUP", "OER", 
         "race_asian", "race_afram", "race_hisp_lat" , 
         "race_haw_pacis", "race_white", "race_other", "race_notsay") %>%
  pivot_longer(cols = race_asian:race_notsay,
               names_to = "race_ethnicity") %>%
  drop_na(value) %>%
  set_names("response_id", "net_promoter_group", "overall_experience_rating", "race_ethnicity", "value") %>%
  group_by(response_id) %>%
  summarize(
    net_promoter_group = net_promoter_group,
    overall_experience_rating = overall_experience_rating,
    race_ethnicity = race_ethnicity,
    value = value,
    value_sum = sum(value)) %>%
  ungroup() %>%
  mutate(race_ethnicity = recode(race_ethnicity,
                                 "race_amind_alask" = "American Indian/Alaska Native",
                                 "race_asian" = "Asian/Asian American",
                                 "race_afram" = "Black/African American",
                                 "race_hisp_lat" = "Hispanic/Latino",
                                 "race_haw_pacis" = "Native Hawaiian/Pacific Islander",
                                 "race_white" = "White",
                                 "race_other" = "Prefer to self-describe",
                                 "race_notsay" = "Prefer not to say")) %>%
  mutate("race_ethnicity" = if_else(value_sum > 1, 
                                    "Multiracial", 
                                    race_ethnicity)) %>%
  distinct(response_id, .keep_all = TRUE) %>%
  select(-c("value", "value_sum"))

View(COVES_22_race)

COVES_22_race$net_promoter_group <- factor(COVES_22_race$net_promoter_group, 
                                           levels = c(1,2,3),
                                           labels = c("Detractor", "Passive", "Promoter"),
                                           ordered = TRUE)

COVES_22_race$overall_experience_rating <- factor(COVES_22_race$overall_experience_rating, 
                                           levels = c(1,2,3,4,5),
                                           labels = c("Poor", "Fair", "Good", "Excellent", "Outstanding"),
                                           ordered = TRUE)        

head(COVES_22_race)
