# This will be a space to clean and perform some initial EDA of the survey data in order to use it 
# Engagement survey items were rated using a 5-point rating scale:
# • 5 = Strongly Agree
# • 4 = Agree
# • 3 = Neither Agree nor Disagree
# • 2 = Disagree
# • 1 = Strongly Disagree


library(tidyverse)
theme_set(theme_bw())


survey_response <- readxl::read_xlsx(here::here("data", "Senior HR Data & Insights Consultant - Work Sample - Engagement Data.xlsx"))

surv_long <-
  survey_response |> 
  pivot_longer(cols = 2:13, names_to = "Question", values_to = "Response") |> 
  filter(!is.na(Response)) |> 
  separate(Question, into = c("Question_Type", "Year"), sep = "_")


saveRDS(surv_long, file = here::here("data", "survey_long.RDS"))


surv_long |> 
  ggplot(aes(Response))+
  geom_bar(stat = "count", aes(fill = Year), position = "dodge")+
  facet_wrap(Question_Type ~ .)
