library(tidyverse)
library(flextable)
library(patchwork)
theme_set(theme_bw())
jcolor <- c("#5a2158", "#EF0029", "#983560", "#cb575e", "#ee8757", "#febe57", "#f9f871")

source(here::here("src", "monthly_employee.R"))

set_flextable_defaults(
  font.size = 10, theme_fun = theme_vanilla,
  padding = 6,
  background.color = "#F8F5F5")



emp_clean <- readRDS(file = here::here("data", "employee.RDS"))
emp_dates <- readRDS(file = here::here("data", "employee_dates.RDS"))

#---- exit by month

exit <-
  emp_dates |> 
  group_by(Term_Month) |> 
  summarise(exits = n()) |> 
  ungroup()


p1 <- exit |> 
  filter(Term_Month < as.Date("2025-10-01")) |> 
    filter(Term_Month >= as.Date("2018-01-01")) |> 
    ggplot(aes(Term_Month, exits))+
    geom_rect(aes(xmin = as.Date("2022-01-01"), xmax = as.Date("2023-01-01"), ymin = -Inf, ymax = Inf), alpha = 0.01, fill = jcolor[6])+
    geom_rect(aes(xmin = as.Date("2023-01-01"), xmax = as.Date("2024-01-01"), ymin = -Inf, ymax = Inf), alpha = 0.01, fill = jcolor[5])+
    geom_point()+
    geom_line()+

  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_color_manual(values = jcolor) +
  labs(title = "Count of Employees Exits By Month",
    x = NULL, y= NULL) + 
  theme(
    panel.background = element_rect(fill = "#F8F5F5"),
    plot.background = element_rect(fill = "#F8F5F5")
  )


turn_year <- 
  emp_dates |> 
  rename(start_date = Hire_Date,
        end_date = CleanEnd) |> 
  select(employee_id = ID, start_date, end_date) |> 
  create_monthly_activity() |> 
  # left_join( , y= emp_clean |> select(ID, Time_Type), 
  #     by = join_by(employee_id == ID)) |> 
  group_by(month) |> 
  summarise(active_employee = n()) |> 
  ungroup() |> 
  filter(month >= as.Date("2018-01-01"),
          month < as.Date("2025-10-01")) |> 
  left_join( , y= emp_dates |> 
                  # left_join(., y=emp_clean |> select(ID, Time_Type), by = join_by(ID)) |> 
                  group_by(Term_Month) |> 
                  summarise(exits = n()) |> 
                  ungroup(), 
        by = join_by(month == Term_Month)) |> 
  mutate(exits = ifelse(is.na(exits), 0, exits)) |> 
  mutate(turnover = exits/active_employee) |> 
  ungroup() |> 
  mutate(yr = lubridate::floor_date(month, unit = "year")) |> 
  group_by(yr) |> 
  summarise(AvgEmp = mean(active_employee, na.rm= T),
            exits_yr = sum(exits, na.rm=T)) |> 
  ungroup() |> 
  mutate(yr_turn = exits_yr/AvgEmp)

f1 <- turn_year |> 
  mutate(yr = year(yr)) |> 
  flextable() |> 
  colformat_num(j = "yr", big.mark = "", digits = 0) |> 
  colformat_double(j = "AvgEmp", digits = 1) |> 
  mk_par(j = "yr_turn", value = as_paragraph(as_chunk(yr_turn, formatter = fmt_pct))) |> 
  bg(i = ~yr == 2022, bg = jcolor[6]) |> 
  bg(i = ~yr == 2023, bg = jcolor[4]) |> 
  set_header_labels(yr = "Year",
                    AvgEmp= "Average Employees",
                    exits_yr = "Exits",
                    yr_turn = "Turnover")

table_grob <- gen_grob(f1, just = "center") #, fit = "fixed"

## showing both together
pw1 <- p1 | wrap_elements(full = table_grob) + 
  plot_layout(widths = c(3,1), heights = c(2,1))

pw2 <- pw1 & theme(plot.background = element_rect(fill = "#F8F5F5"))