
# Author: Hugo Tameirão Seixas

library(fs)
library(terra)
library(tidyverse)

reference <- aggregate(rast("data/reference.tif"), 2, "modal", na.rm = TRUE)

reference_wgs <- project(reference, "EPSG:4326")

carbon <- project(rast("data/carbon.tif"), reference)

op_forest <- aggregate(rast("data/final_layers/op_forest.tif"), 2, "sum", na.rm = TRUE)[[18]] # Optimistic
pe_forest <- aggregate(rast("data/final_layers/pe_forest.tif"), 2, "sum", na.rm = TRUE)[[18]] # Pessimistic

op_fi <- rast("data/fi/fogo_otimista.tif")
op_fi <- project(op_fi, reference)

pe_fi <- rast("data/fi/fogo_pessimista.tif")
pe_fi <- project(pe_fi, reference)

op_fi_area <- op_fi/op_fi * op_forest
pe_fi_area <- pe_fi/pe_fi * pe_forest

global(op_fi_area, sum, na.rm = TRUE)

op_fi_c <- op_fi * (op_forest * carbon * 0.0001)
pe_fi_c <- pe_fi * (pe_forest * carbon * 0.0001)

fire_table <-
  tibble(
    var = rep("fire", 2),
    metric = rep("abs", 2),
    scen = c("op", "pe"),
    carbon = c(
      global(op_fi_c, sum, na.rm = TRUE)[,1],
      global(pe_fi_c, sum, na.rm = TRUE)[,1]
    ),
    area = c(
      global(op_fi/op_fi * op_forest * carbon/carbon, sum, na.rm = TRUE)[,1],
      global(pe_fi/pe_fi * pe_forest * carbon/carbon, sum, na.rm = TRUE)[,1]
    )
  )

write_delim(fire_table, "data/final_layers/fire_table.txt", delim = ",")

op_fi_area[op_fi_area == 0] <- NA
pe_fi_area[pe_fi_area == 0] <- NA

writeRaster(op_fi_area, "data/final_layers/op_fire_area.tif", overwrite = TRUE)
writeRaster(pe_fi_area, "data/final_layers/pe_fire_area.tif", overwrite = TRUE)

writeRaster(op_fi_c, "data/final_layers/op_fire_c.tif", overwrite = TRUE)
writeRaster(pe_fi_c, "data/final_layers/pe_fire_c.tif", overwrite = TRUE)
