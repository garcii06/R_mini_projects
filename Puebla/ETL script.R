# Libraries -----------------------------------------------------------------------------------
library(skimr)
library(janitor)
library(lubridate)
library(tidyverse)

# Extract Data --------------------------------------------------------------------------------
# Although R can handle reading multiple files with read_csv(id = "") we cannot use it in here as
# not every file has the same number of columns. 
# Get a "clean" version of the month.
get_month_data <- function(name_file){
  month_data <- read_csv(str_c("Data/", name_file),
                         col_types = list(Fecha = col_character(),
                                          Monto = col_number())) |> 
    clean_names() |> 
    mutate(fecha = parse_date_time(fecha, orders = c("ymd", "dmy")),
           fecha = ymd(fecha)) |> 
    remove_empty("rows") |> 
    select(danos:monto)
  return(month_data)
}

# Loop through all the files in the folder, get the "clean" version, and append it.
for (i in 1:length(list.files("Data", pattern = ".csv"))) {
  if(i == 1){
    all_data <- tibble()
  }
  all_data <- bind_rows(all_data, get_month_data(list.files("Data", pattern = ".csv")[i]))
}

# Transform Data ------------------------------------------------------------------------------
# Add the column problema principal which encapsulates the main description of the problem.
# Rearrange the columns.
# I left the danos columns so I don't lose information about the problem.
all_data |> 
  skim()

all_data |> 
  distinct(descripcion)

all_data |> 
  distinct(danos) |> 
  view()

all_data <- all_data |> 
  mutate(problema_principal = case_when(str_detect(descripcion, "errib") ~ "Derribado",
                                        str_detect(descripcion, "eterio") ~ "Deteriorado",
                                        str_detect(descripcion, "aya") ~ "Rayado",
                                        str_detect(descripcion, "allad") ~ "Rayado",
                                        str_detect(descripcion, "añ") ~ "Dañado",
                                        str_detect(descripcion, "emol") ~ "Demolido",
                                        str_detect(descripcion, "obla") ~ "Doblado",
                                        str_detect(descripcion, "espre") ~ "Desprendido",
                                        str_detect(descripcion, "etir") ~ "Retiro",
                                        str_detect(descripcion, "aspad") ~ "Raspado",
                                        str_detect(descripcion, "obado") ~ "Robado",
                                        str_detect(descripcion, "bollad") ~ "Abollado")) |> 
  select(fecha, danos, problema_principal, descripcion, monto)

all_data |> 
  view()

## Entries Fixes ------------------------------------------------------------------------------
all_data <- all_data |> 
  mutate(descripcion = replace(descripcion, descripcion == "Demoición de banqueta", "Demolición de banqueta"))

# Load Data -----------------------------------------------------------------------------------
# "Load" the data into a new csv file.
write_csv(all_data, "Daño_Patrimonial.csv")
