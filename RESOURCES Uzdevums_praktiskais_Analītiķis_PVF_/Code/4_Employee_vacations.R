library(dplyr)
library(ggplot2)

dati4 <-
  read.csv("Dati.csv", header = TRUE, stringsAsFactors = TRUE)
head(dati4)
nrow(dati4)

employee_totals2021_df <-
  read.csv("employee_totals.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(employee_totals2021_df)
tail(employee_totals2021_df)
nrow(employee_totals2021_df)

# 1) Darbinieku tabulai pievieno aili ar katram darbiniekam atbilstoso antvalinajuma dienu ipatsvaru

i <- 1
while (i <= nrow(employee_totals2021_df)) {
  x <- employee_totals2021_df$employeeID[i]
  
  y <-
    (sum(dati4$vacation_days[dati4$employeeID == x]) * 100) / sum(dati4$planned_work_days[dati4$employeeID == x])
  employee_totals2021_df$vacation_proportion2021_percentage[employee_totals2021_df$employeeID == x] <-
    round(y, digits = 2)
  i = i + 1
}

head(employee_totals2021_df)
tail(employee_totals2021_df)
nrow(employee_totals2021_df)

# 2) Parbaudi datu izkliedi antvalinajuma dienu ipatsvara aile.

qplot(
  x = employee_totals2021_df$vacation_proportion2021_percentage,
  geom = "boxplot",
  xlab = "Atvalinajuma dienu ipatsvars (%)",
  main = "Kastu diagramma, kas demonstre atvalinajumu dienu proporcijas izkliedi starp darbiniekiem"
)

boxplot(
  employee_totals2021_df$vacation_proportion2021_percentage,
  data = employee_totals2021_df,
  xlab = "Latvenergo darbinieki",
  ylab = "Atvalinajuma dienu ipatsvars (%)",
  main = "Kastu diagramma, kas demonstre atvalinajumu dienu proporcijas izkliedi starp darbiniekiem"
)


write.csv(employee_totals2021_df,
          "Employee_totals1_vacationProportions.csv",
          row.names = FALSE)
