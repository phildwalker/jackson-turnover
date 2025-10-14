# looking at the differences between the characteristics of those who have exited vs the total population

# # Example of a two-way ANOVA
# anova_result <- aov(dependent_variable ~ factor1 * factor2, data = your_data)
# summary(anova_result)

library(tidyverse)

source(here::here("src", "_initial_settings.R"))
source(here::here("src", "monthly_employee.R"))

emp_clean <- readRDS(file = here::here("data", "employee.RDS"))
emp_dates <- readRDS(file = here::here("data", "employee_dates.RDS"))


# anova
# feature selection techniques (Techniques like recursive feature elimination (RFE), Lasso regression, or random forests (variable importance) can help rank the importance of individual factors and identify key combinations.)
# dominate analysis

active_pre <-
    emp_dates |> 
  rename(start_date = Hire_Date,
        end_date = CleanEnd) |> 
  select(employee_id = ID, start_date, end_date) |> 
  create_monthly_activity() |> 
  filter(year(month) %in% c(2018, 2019, 2020, 2021)) |>
  distinct(employee_id) |> 
  left_join(., y=emp_clean, 
              by = join_by(employee_id == ID)) |> 
  mutate(term_pre = case_when(year(Term_Date) %in% c(2018, 2019, 2020, 2021) ~ 1,
                              TRUE ~ 0)) |> 
  mutate(Term_Type_Clean = case_when(term_pre == 0 ~ 'Active',
                            TRUE ~ Term_Type))



anova_result <- aov(term_pre ~  Race_Ethnicity * Gender , 
  data = active_pre)
summary(anova_result)

# Department * Location * Job_Level * Associate_Type * Time_Type * FLSA * Race_Ethnicity * Gender * Term_Type_Clean

active_gr <-
    emp_dates |> 
  rename(start_date = Hire_Date,
        end_date = CleanEnd) |> 
  select(employee_id = ID, start_date, end_date) |> 
  create_monthly_activity() |> 
  filter(year(month) %in% c(2022, 2023)) |>
  distinct(employee_id) |> 
  left_join(., y=emp_clean, 
              by = join_by(employee_id == ID)) |> 
  mutate(term_gr = case_when(year(Term_Date) %in% c(2022, 2023) ~ 1,
                              TRUE ~ 0)) |> 
  mutate(Term_Type_Clean = case_when(term_gr == 0 ~ 'Active',
                            TRUE ~ Term_Type))


anova_result <- aov(term_gr ~  Race_Ethnicity * Gender, 
  data = active_gr)
summary(anova_result)
plot(anova_result)

tukey.plot.aov <-aov(term_gr ~  Race_Ethnicity * Gender, 
  data = active_gr)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


active_gr |> 
  mutate(term = ifelse(term_gr == 0, 'No', 'Yes')) |> 
  group_by(term, Race_Ethnicity) |> 
  summarise(occur = n()) |> 
  pivot_wider(names_from = term, values_from = occur) |> 
  mutate(PerTerm = Yes/ (Yes + No))


#----------------

term_comp <-
  emp_dates |> 
  filter(year(Term_Date) %in% c(2018:2023)) |> 
  mutate(TermGroup = case_when(year(Term_Date) %in% c(2022:2023) ~ 1,
                              year(Term_Date) %in% c(2018:2021) ~ 0))  |> 
  select(ID, TermGroup) |> 
    left_join(., y=emp_clean, 
              by = join_by(ID))


anova_result <- aov(TermGroup ~  Performance_Score, 
  data = term_comp)
summary(anova_result)
plot(anova_result)

tukey.plot.aov <-aov(TermGroup ~  Gender, 
  data = term_comp)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)


# Run the ANCOVA
ancova_model <- lm(post_score ~ pre_score + group, data = wide_data)
summary(ancova_model)





term_comp |> 
  mutate(term = ifelse(TermGroup == 0, '1_Pre', '2_Post')) |> 
  group_by(term, Term_Type) |> 
  summarise(occur = n()) |> 
  ungroup() |> 
  group_by(term) |> 
  mutate(PerGroup = occur / sum(occur)) |> 
  ungroup() |> 
  pivot_wider(names_from = term, values_from = c(occur, PerGroup))


term_comp |> 
  mutate(term = ifelse(TermGroup == 0, '1_Pre', '2_Post'),
PerfGroup = glue::glue("{Performance_Score}_{MostRecent_Performance}")) |>  
  group_by(term, PerfGroup) |> 
  summarise(occur = n()) |> 
  ungroup() |> 
  group_by(term) |> 
  mutate(PerGroup = occur / sum(occur)) |> 
  ungroup() |> 
  pivot_wider(names_from = term, values_from = c(occur, PerGroup))


comp_diff <- 
  term_comp |> 
  mutate(term = ifelse(TermGroup == 0, '1_Pre', '2_Post'),
PerfGroup = glue::glue("{Performance_Score}_{MostRecent_Performance}")) |>  
  group_by(term, PerfGroup) |> 
  summarise(occur = n()) |> 
  ungroup() |> 
  pivot_wider(names_from = term, values_from = c(occur))


# Perform the paired t-test
t.test(comp_diff$`1_Pre`, comp_diff$`2_Post`, paired = TRUE)

library(infer)

observed_f_statistic <- comp_diff |>
  specify(age ~ partyid) |>
  hypothesize(null = "independence") |>
  calculate(stat = "F")