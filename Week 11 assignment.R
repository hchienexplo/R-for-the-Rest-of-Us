# week 11 assignment -------------------------------------------------------


# load library ------------------------------------------------------------

library(tidyverse)
library(janitor)
library(ggtext)
library(scales)


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

COVES_22_race %>%
  tabyl(net_promoter_group, race_ethnicity) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, rounding = "half up") %>%
  adorn_ns()

nsp_race_df<-
  COVES_22_race %>%
  drop_na(race_ethnicity, net_promoter_group) %>%
  group_by(race_ethnicity, net_promoter_group) %>%
  summarize(net_promoter_group_race = n()) %>%
  ungroup() %>%
  group_by(race_ethnicity) %>%
  mutate(race_ethnicity_total = sum(net_promoter_group_race)) %>%
  mutate(nps_race_percentage = round(net_promoter_group_race/race_ethnicity_total,3)) %>%
  filter(net_promoter_group == "Promoter") %>%
  filter(!race_ethnicity %in% c("Prefer not to say", "Prefer to self-describe")) %>%
  mutate(percent_display = paste0(nps_race_percentage *100, "%")) %>%
  ungroup()

nsp_race_df_highlighted<-
  COVES_22_race %>%
  drop_na(race_ethnicity, net_promoter_group) %>%
  group_by(race_ethnicity, net_promoter_group) %>%
  summarize(net_promoter_group_race = n()) %>%
  ungroup() %>%
  group_by(race_ethnicity) %>%
  mutate(race_ethnicity_total = sum(net_promoter_group_race)) %>%
  mutate(nps_race_percentage = round(net_promoter_group_race/race_ethnicity_total,3)) %>%
  filter(net_promoter_group == "Promoter") %>%
  filter(race_ethnicity == "Black/African American") %>%
  ungroup() 
  
promoter_by_race_plot<-
nsp_race_df %>%  
  ggplot(aes(x = fct_reorder(race_ethnicity, nps_race_percentage),
             y = nps_race_percentage,)) +
  geom_col(fill="darkgrey")+
  coord_flip()+
  geom_col(data = nsp_race_df_highlighted,
           fill = "darkred") +
  geom_text(data = nsp_race_df[nsp_race_df$race_ethnicity!= "Black/African American", ],
            inherit.aes = TRUE,
            aes(label = percent_display),
            hjust = -0.1,
            col = "darkgrey",
            family = "Inter",
            size = 3) +
  geom_text(data = nsp_race_df_highlighted,
            inherit.aes = TRUE,
            aes(label = percent_display),
            hjust = -0.1,
            col = "darkred",
            family = "Inter",
            size = 3)+
  scale_y_continuous(label = percent_format(), 
                     limit = c(0, 1.1),
                     breaks = seq(0, 1, 0.2)) +
  labs(title = "The percentage of Promoter was lowest among <span style = 'color: darkred'>Black/African Americans</span>") +
  theme_minimal() + 
  theme(axis.title = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.title = element_markdown(face = "bold", hjust = 1.2, size = 12.5))

ggsave("promoter_by_race_plot.png", width = 6.5, height = 4.2)  




