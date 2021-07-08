library(readr)

source("src/function.R")

write_csv(data.frame(suma_1 = suma(1), suma_2 = suma(2)), "data_output/prueba_suma.csv")
