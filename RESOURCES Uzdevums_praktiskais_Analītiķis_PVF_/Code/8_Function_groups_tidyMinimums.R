library(dplyr)
library(ggplot2)

function_groups2021_df1 <-
  read.csv("function_groups2021_df1.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(function_groups2021_df1)
nrow(function_groups2021_df1)


# 1) Redzams, ka cetras grupas izlecoso vertibu nonemsana ir izmainijusi aprekinato ipatsvaru.

sum((
  function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes
  == function_groups2021_df1$vacation_proportion2021_percentage_per_function
) == FALSE
)

# Tas ir:
function_groups2021_df1$function_groups[function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes
                                        != function_groups2021_df1$vacation_proportion2021_percentage_per_function]

# Kastu diagramma funkciju grupam bez izlecosajam vertibam

ggplot(data = employee_functions2021_df_noExtremes) +
  geom_boxplot(mapping =
                 aes(x = function_group,
                     y = vacation_proportion2021_percentage))
head(function_groups2021_df1)

# 2) Novertet kura grupa ir vismazakais atvalinajuma dienu ipatsvars.

function_groups2021_df1$function_groups[function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes == min(
  function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes
)]
function_groups2021_df1$number_of_employees_per_group[function_groups2021_df1$function_groups == "01EM00"]
subset(
  employee_functions2021_df_noExtremes,
  employee_functions2021_df_noExtremes$function_group == "01EM00"
)
function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes[function_groups2021_df1$function_groups == "01EM00"]

# 3) Aprekinat IQR darbinieku skaitam funkciju grupas bez izlecosajam vertibam.

IQR(function_groups2021_df1$number_of_employees_per_group)
summary(function_groups2021_df1$number_of_employees_per_group)

x <- subset(
  function_groups2021_df1,
  function_groups2021_df1$number_of_employees_per_group >= 4.5 &
    function_groups2021_df1$number_of_employees_per_group <= 29.5
)
nrow(x)
  function_groups2021_df1$function_groups[function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes
                                          == min(x$vacation_proportion2021_percentage_per_function_noExtremes)]

x <- subset(
  function_groups2021_df1,
  function_groups2021_df1$number_of_employees_per_group >= 29.5
)
nrow(x)
function_groups2021_df1$function_groups[function_groups2021_df1$vacation_proportion2021_percentage_per_function_noExtremes == min(x$vacation_proportion2021_percentage_per_function_noExtremes)]

function_groups2021_df1$number_of_employees_per_group[function_groups2021_df1$function_groups == "01TI80"]


write.csv(function_groups2021_df1,
          "function_groups2021_df_new.csv",
          row.names = FALSE)


write.csv(
  employee_functions2021_df_noExtremes,
  "employee_functions2021_df_noExtremes.csv",
  row.names = FALSE
)
