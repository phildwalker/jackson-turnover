# This will be a space to clean and perform some initial EDA of the survey data in order to use it 

options(scipen = 999)
library(tidyverse)
theme_set(theme_bw())
jcolor <- c("#5a2158", "#983560", "#cb575e", "#ee8757", "#febe57", "#f9f871")

source(here::here("src", "monthly_employee.R"))

employee <- readxl::read_xlsx(here::here("data", "Senior HR Data & Insights Consultant - Work Sample - HRIS Data.xlsx")) |> 
  filter(!Associate_Type %in% c("Temporary"))


#--- is ID unique? -- yes
employee |> 
  count(ID) |> 
  filter(n > 1)

# what's the distribution of the performance
employee |> count(MostRecent_Performance)

# what's the distribution of locations
employee |> count(Location) # 2/3 MI, 1/3 TN
employee |> count(FLSA)
employee |> count(Department) # no nulls here
employee |> count(Job_Level) # assuming that executive is above management


# cleaning the data to then have a clean version to use later
emp_clean <- 
  employee |> 
  mutate(EffectiveSalary = ifelse(FLSA == "Exempt", Compensation, Compensation *2080),
      Performance_Score = case_when(
        MostRecent_Performance %in% c("Consistently Exceeds Expectations") ~ 5,
        MostRecent_Performance %in% c("Exceeds Expectations") ~ 4,
        MostRecent_Performance %in% c("Meets Expectations") ~ 3,
        MostRecent_Performance %in% c("Below Expectations") ~ 2,
        MostRecent_Performance %in% c("Consistently Below Expectations") ~ 1,
        TRUE ~ -100 # some weird error, would want to flag
      ))


saveRDS(emp_clean, file = here::here("data", "employee.RDS"))




#--- will want to build out a view of how many employees are available in a month to then build to a view of how many turnovers were there in that month

emp_dates <- 
  emp_clean |> 
  select(ID, Hire_Date, Term_Date) |> 
  mutate(Term_Date = as.Date(Term_Date),
      Hire_Date = as.Date(Hire_Date)) |> 
  mutate(
    ActiveEmp = ifelse(is.na(Term_Date), 1,0),
    CleanEnd = ifelse(is.na(Term_Date), as.Date('2025-10-01'), Term_Date),
    CleanEnd = as.Date(CleanEnd),
    Term_Month = lubridate::floor_date(CleanEnd, 'month'),
    Tenure = (as.numeric(CleanEnd - Hire_Date, units = "days") / 365.25)
  )

saveRDS(emp_dates, file = here::here("data", "employee_dates.RDS"))

#---- exit by month

exit <-
  emp_dates |> 
  count(Term_Month)


#---------- 

month_trend <- emp_dates |> 
  # head(10) |> 
  rename(start_date = Hire_Date,
        end_date = CleanEnd) |> 
  select(employee_id = ID, start_date, end_date) |> 
  create_monthly_activity() |> 
  left_join( , y= emp_clean |> select(ID, Time_Type), 
      by = join_by(employee_id == ID)) |> 
  group_by(month, Time_Type) |> 
  summarise(active_employee = n()) |> 
  ungroup()



month_trend |> 
  filter(month >= as.Date("2018-01-01")) |> 
  ggplot(aes(month, active_employee, color = Time_Type))+
    geom_point()+
    geom_line()+
  scale_x_date(date_breaks = "6 month", date_labels = "%m-%Y") + 
  scale_color_manual(values = jcolor) +
  labs(title = "Count of Active Employees By Month",
    x = NULL, y= NULL)
