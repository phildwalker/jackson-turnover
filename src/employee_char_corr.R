#building out the correlation analysis of employee characteristics to termination

library(tidyverse)
library(correlationfunnel)

source(here::here("src", "_initial_settings.R"))
source(here::here("src", "monthly_employee.R"))

emp_clean <- readRDS(file = here::here("data", "employee.RDS"))
emp_dates <- readRDS(file = here::here("data", "employee_dates.RDS"))


## getting employees active from 2018 to now
emp_recent <- emp_dates |> 
  # head(10) |> 
  rename(start_date = Hire_Date,
         end_date = CleanEnd) |> 
  select(employee_id = ID, start_date, end_date) |> 
  create_monthly_activity() |> 
  filter(month >= as.Date("2018-01-01")) |> 
  select(employee_id) |> 
  distinct() |> 
  pull(employee_id)


recent_employee <- 
  emp_clean |> 
  filter(ID %in% emp_recent) |> 
  mutate(Term = ifelse(is.na(Term_Type), 'Active', Term_Type),
         TYear = year(Term_Date),
         TYear_Group = case_when(TYear %in% c(2018:2021) ~ "2018-2021",
                                 TYear %in% c(2022:2023) ~ "2022-2023",
                                 TYear %in% c(2024:2025) ~ "2024-2025",
                                 TRUE ~ "Other")) |> 
  mutate(Term_Group = glue::glue("{Term}_{TYear_Group}"))

recent_employee |> 
  ggplot(aes(Compensation, fill = Term_Group))+
  geom_density(alpha= .1) +
  facet_grid(Location ~ Job_Level, scales = "free")

recent_employee |> 
  ggplot(aes(Merit, fill = Term_Group, color = Term_Group))+
  geom_density(alpha= .1) +
  facet_grid(Location ~ Job_Level, scales = "free")


recent_employee |> 
  group_by(Term_Group, Job_Level) |> 
  summarise(TotalEmployee = n(),
            AvgComp = mean(Compensation, na.rm=T),
            MedianComp = median(Compensation, na.rm=T),
            AvgMerit = mean(Merit, na.rm=T)) |> 
  ungroup()



#----- correlation funnel

recent_clean <-
  recent_employee %>% 
  mutate(exit = ifelse(is.na(Term_Date), 'no', 'yes'),
         Performance = glue::glue("{Performance_Score}_{MostRecent_Performance}"),
         Performance = as.character(Performance)) %>% 
  filter(!is.na(Merit)) %>%
  select(exit, Department, Location, Job_Level, FLSA, Race_Ethnicity, Gender, Merit, EffectiveSalary, Performance)


recent_bin <- 
  recent_clean %>% 
  binarize(n_bins = 4, thresh_infreq = 0.01)


recent_emp_corr <- 
  recent_bin %>% 
  correlate(target = exit__yes)


cf <- recent_emp_corr %>% 
  plot_correlation_funnel(interactive = FALSE)

ggsave(here::here('img', 'employee_characteristic_correlation_funnel.png') ,plot = cf)


save(cf, file = here::here("img","corr_funnel_term.Rdata"))


recent_clean %>% 
  group_by(Performance, exit) %>% 
  count()



#---- corr funnel for exits


exits <-
  emp_clean %>%
  filter(year(Term_Date) >= 2018) %>% 
  mutate(Performance = glue::glue("{Performance_Score}_{MostRecent_Performance}"),
         Performance = as.character(Performance)) %>% 
  filter(!is.na(Merit)) %>%
  select(Term_Type, Department, Location, Job_Level, FLSA, Race_Ethnicity, Gender, Merit, EffectiveSalary, Performance)


exits_bin <- 
  exits %>% 
  binarize(n_bins = 4, thresh_infreq = 0.01)


exits_corr <- 
  exits_bin %>% 
  correlate(target = Term_Type__Voluntary)


(cf_tt <- exits_corr %>% 
  plot_correlation_funnel(interactive = FALSE))

ggsave(here::here('img', 'term_type_correlation_funnel.png') ,plot = cf_tt)


save(cf_tt, file = here::here("img","corr_funnel_term_type.Rdata"))


