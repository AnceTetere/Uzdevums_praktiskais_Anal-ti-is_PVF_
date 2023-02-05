library(dplyr)
library(ggplot2)

dati4 <-
  read.csv("Dati.csv", header = TRUE, stringsAsFactors = TRUE)
head(dati4)
head(dati4)
nrow(dati4)

employee_functions2021_df <-
  read.csv("Months_worked2021_df2.csv",
           header = TRUE,
           stringsAsFactors = FALSE)
head(employee_functions2021_df)
nrow(employee_functions2021_df)

# 1) Katram darbiniekam <employee_functions2021_df> tabula pievieno atbilstoso funkciju grupu.

i <- 1
while (i <= nrow(employee_functions2021_df)) {
  x <- employee_functions2021_df$employeeID[i]
  y <- dati4$function_group[dati4$employeeID == x]
  employee_functions2021_df[i, "function_group"] <- y[1]
  i = i + 1
}

head(employee_functions2021_df)
nrow(employee_functions2021_df)

# 2) Sakarto tabulu ta, lai <function_groups> vertibas butu alfabetiska kartiba.

employee_functionsOrdered <-
  employee_functions2021_df[order(employee_functions2021_df$function_group), ]
head(employee_functionsOrdered)
tail(employee_functionsOrdered)
nrow(employee_functionsOrdered)

employee_functionsOrdered$function_group <-
  as.character(employee_functionsOrdered$function_group)
typeof(employee_functionsOrdered$function_group)


# 3) Saskaita darbiniekus katra funkciju grupa.

i <- 1
z <- 1
number_of_members_vec <- c(0L, 0L, 0L)
function_group_vec <- c("0", "0", "0")
group_number_vec <- c(0L, 0L, 0L)

while (i <= nrow(employee_functionsOrdered)) {
  x <- employee_functionsOrdered$function_group[i]
  x
  y <- sum(employee_functionsOrdered$function_group == x)
  y
  # cat("The function group Nr.", x, "has", y, "members \n")  # Var tikt aktivizets, lai redzetu tekstualu vestijumu konsole.
  
  function_group_vec[z] <- x
  number_of_members_vec[z] <- y
  group_number_vec[z] <- z
  
  z = z + 1
  i = i + y
}

length(function_group_vec) == length(number_of_members_vec) &
  length(function_group_vec) == group_number_vec[z - 1]

head(number_of_members_vec)  # uzrada darbinieku skaitu katra funkciju grupa
typeof(number_of_members_vec)
head(function_group_vec)     # rada funkciju grupu nosaukumus alfabetiska kartiba
typeof(function_group_vec)
group_number_vec <- as.character(group_number_vec)
head(group_number_vec)       # shows a number ascribed to the function group for plotting purposes.
typeof(group_number_vec)

# 3) Izveidot tabulu, kas uzrada funkciju grupas nosaukumu, tai nozimeto numuru prieks liksanas grafikos un darbinieku skaitu katra.

function_groups2021_df <-
  data.frame(
    function_groups = function_group_vec,
    number_of_employees_per_group = number_of_members_vec,
    function_group_number = group_number_vec
  )
function_groups2021_df

# 4) Grafiski ilustret vertibas.
ggplot(function_groups2021_df,
       aes(
         x = factor(function_group_number,
                    level = function_group_number),
         y = number_of_employees_per_group
       )) + geom_bar(stat = "identity")


# Ir grupas, kuras ir tikai viens darbinieks, kurpretim citas ir virs 200.

write.csv(function_groups2021_df, "Funkciju_grupas.csv", row.names = FALSE)
