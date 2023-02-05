library(dplyr)
library(ggplot2)

dati2 <-
  read.csv("Dati.csv", header = TRUE, stringsAsFactors = TRUE)
head(dati2)

# Datu parbaude
nrow(dati2)
dati3 <- dati2
nrow(dati3)

# Parbaudit vai visi darbinieki 2021. gada ir stradajusi vienadu menesu skaitu, vai tas atskiras.

# 1) Sakarto <employeeID> augosa seciba.

dati3ordered <- dati3[order(dati3$employeeID), ]
head(dati3ordered)
tail(dati3ordered)
nrow(dati3ordered)

# 2) Skaitot reizes, cik katrs <employeeID> uzradas orginalaja tabula (jo katra rinda atbilst vienam menesim),
#    pakartoti tiek izveidots jauns vektors 'character' formata,
#    lai kaut kadu nejausu kalkulaciju rezultata sie darbinieku identifikatori netiktu parveidoti.

i <- 1
z <- 1
occurrences_vec <- c(0L, 0L, 0L)
employee_numbers <- c("0", "0", "0")
typeof(employee_numbers)

while (i <= nrow(dati3ordered)) {
  x <- dati3ordered$employeeID[i]
  y <- sum(dati3ordered$employeeID == x)
  #  cat("Employee Nr.", x, "had", y, "occurrences in the data set \n")
  employee_numbers[z] <-
    x   # Izveido vektoru ar darbinieku identifikacijas numuriem.
  occurrences_vec[z] <-
    y    # Izveido vektoru ar darbinieku rindam tabula, attiecigi menesiem.
  
  z = z + 1      # 'z' define veribas poziciju jaunajos vektoros.
  # Pozicijai aizpildoties, tiek parvietots uz nakamo.
  i = i + y      # 'i' nem pa vienam <employeeID> no <dati3ordered$employeeID> kolonnas.
  # Par cik si kolonna ir kartota ta, ka vienadie darbinieku identifikatoru numuri seko cits citam,
  # nakosais atskirigais <employeeID> atrodams pec 'i + y' pozicijam kartotaja vektora.
  # 'i' ir pozicija, kas nak pec apaksvektora y kolonna <dati3ordered$employeeID>, kur koda cilpa atrod nakamo <employeeID> nummuru.
  
}

head(occurrences_vec)  # Vektors <occurrences_vec> rada menesu skaitu, ko katrs darbinieks nostradajis 2021. gada.
typeof(occurrences_vec)
length(occurrences_vec)

head(employee_numbers)  # Vektors <employee_numbers> rada darbinieku identifikacijas numurus <employeeIDs>
# teksta formata un augosa seciba, katram numuram paradoties tikai vienreiz.
typeof(employee_numbers)
length(employee_numbers) # Garumam jabut vienadam ar <occurrences_vec>.


# 3) Izveido vienkarsu tabulu, kas sast;av tikai no <employee_numbers> kolonnas un atbilstosiem <occurrences_vec> skaitliem.
#    Si tabula rada, cik tiesi menesus katrs no darbiniekiem ir bijis nodarbinats uznemuma 2021. gada.

if (length(occurrences_vec) == length(employee_numbers)) {
  # Tabula <employee_months2021_df> darbinieku identifikatori <employeeID> ir augosa seciba un blakus uzradas nostradato menesu skaits.
  employee_months2021_df <-
    data.frame(employeeID = employee_numbers, months_worked2021 = occurrences_vec)
}
head(employee_months2021_df, 20)
tail(employee_months2021_df)
nrow(employee_months2021_df)

# 4) Tiek zimeta kolonnu diagramma, kas ilustre nostradato menesu skaitu katram darbiniekam.

qplot(
  x = months_worked2021,
  data = employee_months2021_df,
  geom = "bar",
  xlim = c(1, 13),
  xlab = "Nostradato menesu skaits",
  ylab = "Darbinieku skaits",
  main = "Kolonnu diagramma, kura uzrada darbinieku skaitu, kas nostradajusi noteiktu menesu skaitu 2021. gada."
)

# saja dala tika parbaudits vai katrs no darbiniekiem stradajis vienadu menesu laiku 2021. gada, vai tie atskiras.
# SECINAJUMS: 2021. gada nostradato menesu skaits starp darbiniekiem atskiras,
#               lai gan lielaka dala ir bijusi nodarbinata uznemuma visu gadu.

write.csv(employee_months2021_df,
          "Months_worked2021_df2.csv",
          row.names = FALSE)
