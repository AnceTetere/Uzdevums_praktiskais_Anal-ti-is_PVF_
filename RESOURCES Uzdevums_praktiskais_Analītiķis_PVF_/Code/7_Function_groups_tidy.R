library(dplyr)
library(ggplot2)

employee_functions2021_df <-
  read.csv("employee_totals.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(employee_functions2021_df)
nrow(employee_functions2021_df)

dati5 <-
  read.csv("Dati.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(dati5)
nrow(dati5)

function_groups2021_df <-
  read.csv("Funkciju_grupas.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(function_groups2021_df)
nrow(function_groups2021_df)


# 1) Ja tabula <employee_functions2021_df> pret darbinieku identifikatoriem vel nav atbilstosa funkciju grupa,
# tad te to var pievienot ka <employee_functions2021_df>.

i <- 1

while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  y <- dati5$function_group[dati5$employeeID == x]
  employee_functions2021_df$function_group[employee_functions2021_df$employeeID == x] <-
    y[1]
  i = i + 1
}
head(employee_functions2021_df)
nrow(employee_functions2021_df)

# 2) Tabula <employee_functions2021_df> pievienot funkciju grupam atbilstoso funkcijas grupas izmeru ka <function_group_size> aili.

i <- 1

while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$function_group[i]
  y <-
    function_groups2021_df$number_of_employees_per_group[function_groups2021_df$function_groups == x]
  employee_functions2021_df$function_group_size[employee_functions2021_df$function_group == x] <-
    y
  i = i + 1
}

head(employee_functions2021_df)
nrow(employee_functions2021_df)

# 3) Tabula <employee_functions2021_df> pievienot <function_group_number> aili ar funkcijai doto numuru, ko izmantot grafikos.

i <- 1
while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$function_group[i]
  y <-
    function_groups2021_df$function_group_number[function_groups2021_df$function_groups == x]
  employee_functions2021_df$function_group_number[employee_functions2021_df$function_group == x] <-
    as.character(y)
  i = i + 1
}

head(employee_functions2021_df)
nrow(employee_functions2021_df)

typeof(employee_functions2021_df$function_group_number)

# 4) Tabula <employee_functions2021_df> pievienot aili ar planoto darba dienu summu 2021-tajam gadam ka <planned_work_days2021>
#    un ailu ar atvalinajuma dienu kopsumam gada pret katru darbinieka numuru ka <vacation_days2021>.

i <- 1
while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  y <- sum(dati5$planned_work_days[dati5$employeeID == x])
  employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == x] <-
    y
  i = i + 1
}
head(employee_functions2021_df)

i <- 1
while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  y <- sum(dati5$vacation_days[dati5$employeeID == x])
  employee_functions2021_df$vacation_days2021[employee_functions2021_df$employeeID == x] <-
    y
  i = i + 1
}
head(employee_functions2021_df)


# 5) Pievienot aprekinato atvalinajuma dienu ipatsvaru katram darbiniekam aile <vacation_proportion2021_percentage>.

i <- 1
while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  y <-
    round(((employee_functions2021_df$vacation_days2021[employee_functions2021_df$employeeID == x]) * 100 /
             employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == x]
    ),
    digits = 2)
  employee_functions2021_df$vacation_proportion2021_percentage[employee_functions2021_df$employeeID == x] <-
    y
  i = i + 1
}
head(employee_functions2021_df)

# 6) Aprekinat atvalinajuma dienu ipatsvaru katrai no funkcijam, un
#    vertibas izkartot tabulas <function_groups2021_df> aile ar nosaukumu <vacation_proportion2021_percentage_per_function>.

head(function_groups2021_df)
function_groups2021_df1 <- function_groups2021_df[, 1:3]
head(function_groups2021_df1)

i = 1
while (i <= nrow(function_groups2021_df)) {
  x <- function_groups2021_df1$function_groups[i]
  y <-
    round(((
      sum(employee_functions2021_df$vacation_days2021[employee_functions2021_df$function_group == x])
    ) * 100) /
      sum(employee_functions2021_df$planned_work_days2021[employee_functions2021_df$function_group == x]),
    digits = 2)
  
  function_groups2021_df1$vacation_proportion2021_percentage_per_function[function_groups2021_df1$function_groups == x] <-
    y
  i = i + 1
}
head(function_groups2021_df1)
function_groups2021_df1$vacation_proportion2021_percentage_per_function


# Kastu diagramma - visi dati.

ggplot(data = employee_functions2021_df) +
  geom_boxplot(mapping =
                 aes(x = function_group,
                     y = vacation_proportion2021_percentage))


employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == 5555]
employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == 13202]
employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == 15203]
employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == 15232]
employee_functions2021_df$planned_work_days2021[employee_functions2021_df$employeeID == 15203]

# 7) Izveidot apakstabulu <employee_functions2021_df_noExtremes> bez izlecosam vertibam.
#    Parrekinat atvalinajuma dienu ipatsvaru katra no grupam un aizpildit aili <vacation_proportion2021>.
#    Pievienot so jauno aili ar nosaukumu < vacation_proportion2021_percentage_per_function_noExtremes>
#    jau esosaja funkciju grupu tabula <function_groups2021_df1>

employee_functions2021_df_noExtremes <-
  subset(
    employee_functions2021_df,
    employee_functions2021_df$vacation_proportion2021_percentage < 25
  )
head(employee_functions2021_df_noExtremes)
nrow(employee_functions2021_df_noExtremes)

i = 1
while (i <= nrow(function_groups2021_df)) {
  x <- function_groups2021_df1$function_groups[i]
  y <-
    round(((
      sum(employee_functions2021_df_noExtremes$vacation_days2021[employee_functions2021_df_noExtremes$function_group == x])
    ) * 100) /
      sum(
        employee_functions2021_df_noExtremes$planned_work_days2021[employee_functions2021_df_noExtremes$function_group == x]
      ),
    digits = 2
    )
  
  function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes[function_groups2021_df1$function_groups == x] <-
    y
  i = i + 1
}
head(function_groups2021_df1)
sum((
  function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes
  == function_groups2021_df1$vacation_proportion2021_percentage_per_function
) == FALSE
)

# Redzams, ka cetras grupas izlecoso vertibu nonemsana ir izmainijusi aprekinato ipatsvaru.
function_groups2021_df1$function_groups[function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes
                                        != function_groups2021_df1$vacation_proportion2021_percentage_per_function]

# Kastu diagramma funkciju grupam bez izlecosajam vertibam

ggplot(data = employee_functions2021_df_noExtremes) +
  geom_boxplot(mapping =
                 aes(x = function_group,
                     y = vacation_proportion2021_percentage))


write.csv(function_groups2021_df1,
          "function_groups2021_df1.csv",
          row.names = FALSE)
