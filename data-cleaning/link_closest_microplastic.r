library(arrow)
library(FNN)  
library(dplyr)
library(ggplot2)
library(viridis)

currents <- read_parquet("plastic-drift-app/datasources/binned_currents.parquet")
microplastics <- read_parquet("plastic-drift-app/datasources/cleaned_microplastics.parquet")

nearest_mp <- get.knnx(data = microplastics[, c("Longitude", "Latitude")], 
                       query = currents[, c("lon", "lat")], 
                       k = 1)

currents$Concentration.Class <- microplastics$Concentration.Class[nearest_mp$nn.index]
currents$mp_lon <- microplastics$Longitude[nearest_mp$nn.index]
currents$mp_lat <- microplastics$Latitude[nearest_mp$nn.index]

write_parquet(currents, "plastic-drift-app/datasources/currents_with_microplastics.parquet")
