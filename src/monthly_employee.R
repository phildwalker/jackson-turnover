# building out a function to create the monthly view of the employee dataset

library(tidyverse)
library(lubridate)

# Example employee data 
# for validation when building out the function
# employees <- tibble(
#   employee_id = c(1, 2, 3, 4),
#   name = c("Alice", "Bob", "Charlie", "Diana"),
#   start_date = as.Date(c("2023-01-15", "2023-03-01", "2023-02-10", "2023-01-01")),
#   end_date = as.Date(c("2023-06-30", "2023-12-31", NA, "2023-04-15"))
# )

# Function to create monthly activity dataset
create_monthly_activity <- function(employee_data, start_month = NULL, end_month = NULL) {
  
  # Determine date range if not specified
  if (is.null(start_month)) {
    start_month <- floor_date(min(employee_data$start_date, na.rm = TRUE), "month")
  }
  if (is.null(end_month)) {
    # Use today's date if no end dates or the max end date
    end_month <- floor_date(
      max(c(employee_data$end_date, Sys.Date()), na.rm = TRUE), 
      "month"
    )
  }
  
  # Create sequence of months
  months <- tibble(
    month = seq(start_month, end_month, by = "month")
  )
  
  # Cross join employees with months, then filter for active periods
  monthly_activity <- employee_data %>%
    crossing(months) %>%
    mutate(
      # Employee is active if month is >= start and <= end (or end is NA)
      is_active = month >= floor_date(start_date, "month") & 
                  (is.na(end_date) | month <= floor_date(end_date, "month"))
    ) %>%
    filter(is_active) %>%
    select(employee_id, month, start_date, end_date)
  
  return(monthly_activity)
}

# # Create the monthly activity dataset
# monthly_activity <- create_monthly_activity(employees)

# # View the result
# print(monthly_activity)