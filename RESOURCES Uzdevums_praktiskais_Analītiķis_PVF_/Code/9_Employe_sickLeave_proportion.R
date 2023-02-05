library(dplyr)
library(ggplot2)

employee_functions2021_df <-
  read.csv(
    "employee_functions2021_df_new.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
head(employee_functions2021_df)
nrow(employee_functions2021_df)

dati5 <-
  read.csv("Parvardota_tabula_dati2.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(dati5)
nrow(dati5)

# 1) Pievienot <sick_leave_total> aili <employee_functions2021_df> tabula.

i <- 1
while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  y <- sum(dati5$sick_leave_total[dati5$employeeID == x])
  employee_functions2021_df$sick_leave_total[employee_functions2021_df$employeeID == x] <-
    y
  i = i + 1
}
head(employee_functions2021_df)
tail(employee_functions2021_df)

sum(employee_functions2021_df$sick_leave_total != 0)
x <-
  employee_functions2021_df$sick_leave_total[employee_functions2021_df$sick_leave_total > 0]
round(mean(x), digits = 2)
round(median(x), digits = 2)

IQR(employee_functions2021_df$sick_leave_total)

# 2) Aprekini slimibas dienu ipatsvaru katram darbiniekam

i <- 1

while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  x
  y <-
    round((employee_functions2021_df$sick_leave_total[employee_functions2021_df$employeeID == x]) * 100 /
            employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == x],
          digits = 2
    )
  employee_functions2021_df$sick_leave_proportion2021_percentage[employee_functions2021_df$employeeID == x] <-
    y
  i = i + 1
}
head(employee_functions2021_df)


# 3) Izveido sarakstu ar slimibas dienu ipatsvaru
saraksts <- data.frame(
  employeeID = employee_functions2021_df$employeeID,
  sick_leave_proportion2021_percentage = employee_functions2021_df$sick_leave_proportion2021_percentage
)
head(saraksts)


write.csv(saraksts,
          "saraksts.csv",
          row.names = FALSE)

write.csv(
  employee_functions2021_df,
  "employee_functions2021_df_with_sickLeave",
  row.names = FALSE
)
