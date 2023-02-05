library(dplyr)
library(ggplot2)

employee_totals2021_df <-
  read.csv(
    "Employee_totals1_vacationProportions.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
head(employee_totals2021_df)
nrow(employee_totals2021_df)

# 1) Aprekini IQR

IQR <-
  IQR(employee_totals2021_df$vacation_proportion2021_percentage,
      na.rm = FALSE)
IQR
median(employee_totals2021_df$vacation_proportion2021_percentage)
quantile(employee_totals2021_df$vacation_proportion2021_percentage)

Q1
Q3

Q0 <- Q1 - 1.5 * IQR
round(Q0, digits = 2)

Q4 <- Q3 + 1.5 * IQR
round(Q4, digits = 2)

nrow(employee_totals2021_df)

# 2) Saskaitit, cik darbiniekiem nav bijusas nevienas atvalinajuma dienas 2021. gada.

sum(employee_totals2021_df$vacation_proportion2021_percentage == 0)
sum(
  employee_totals2021_df$vacation_proportion2021_percentage == 0 &
    employee_totals2021_df$months_worked2021 == 12
)

# Grafiski attelot vertibas
no_vacations_df <-
  subset(employee_totals2021_df,
         vacation_proportion2021_percentage == 0)
sum(no_vacations_df$vacation_proportion2021_percentage == 0) == nrow(no_vacations_df)
head(no_vacations_df)
nrow(no_vacations_df)

qplot(
  x = months_worked2021,
  data = no_vacations_df,
  geom = "bar",
  xlim = c(1, 13),
  xlab = "Nostradato menesu skaits",
  ylab = "Darbinieku skaits",
  main = "Kolonnu diagramma, kura uzrada darbinieku skaitu, kas nostradajusi noteiktu menesu skaitu 2021. gada bez atvalinajuma dienam."
)

# 2) Saskaitit, cik darbiniekiem atvalinajuma dienu ipatsvars ir virs maksimalas vertibas datu kopa.

# 3) Count how many employees have vacation day proportion above Q4.
sum(employee_totals2021_df$vacation_proportion2021_percentage > 15.88)
many_vacations_df <-
  subset(employee_totals2021_df,
         vacation_proportion2021_percentage > 15.88)
head(many_vacations_df)
nrow(many_vacations_df)
many_vacations_df


plot(
  many_vacations_df$months_worked2021,
  many_vacations_df$vacation_proportion2021_percentage,
  main = "Izkliedes (punktu) diagramma rada attiecibu starp nostradato menesu skaitu un atvalinajuma dienu ipatsvaru",
  xlab = "Nostradato menesu skaits 2021. gada",
  ylab = "Atvalinajuma dienu ipatsvars (%)",
  pch = 19
)


employee_totals2021_df_withoutOutliers <-
  subset(employee_totals2021_df,
         vacation_proportion2021_percentage < 25)
head(employee_totals2021_df_withoutOutliers)
nrow(employee_totals2021_df_withoutOutliers)



write.csv(
  employee_totals2021_df_withoutOutliers,
  "Employee_totals_df_noOutliers.csv",
  row.names = FALSE
)

write.csv(no_vacations_df,
          "Darbinieki_bez_atvalinajuma.csv",
          row.names = FALSE)

write.csv(many_vacations_df,
          "employees_with_manyVacations.csv",
          row.names = FALSE)
