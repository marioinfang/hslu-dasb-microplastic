library(arrow)
library(hexbin)
library(dplyr)
library(FNN)


currents <- read.table("data-cleaning/buoydata_15001_jul24.dat",
           header=TRUE)
colnames(currents) <- c("id", "something", "time", "date", "lat", "lon", "t", "ve", "vn", "speed", "varlat", "varlon", "vart")
currents <- subset(currents, select = -c(something, varlat, varlon, vart) )

summary(currents$lon)
currents$lon <- ifelse(currents$lon > 180, currents$lon - 360, currents$lon)
summary(currents$lon)
currents <- currents %>%

  filter(lon >= -180 & lon <= 180, lat >= -90 & lat <= 90)
summary(currents$lon)
currents <- currents %>%
  filter(ve < 999, vn < 999, speed < 999)


num_bins <- 500

hex_bins <- hexbin(currents$lon, currents$lat, xbins = num_bins)

bin_centers <- data.frame(
  bin_id = 1:length(hex_bins@count),
  lon = hex_bins@xcm,
  lat = hex_bins@ycm
)

nearest_bins <- get.knnx(data = bin_centers[, c("lon", "lat")], 
                         query = currents[, c("lon", "lat")], 
                         k = 1)

currents$bin_id <- bin_centers$bin_id[nearest_bins$nn.index]

currents_binned <- currents %>%
  group_by(bin_id) %>%
  summarise(
    speed_sum = sum(speed, na.rm = TRUE), 
    speed_avg = mean(speed, na.rm = TRUE), 
    ve_avg = mean(ve, na.rm = TRUE), 
    vn_avg = mean(vn, na.rm = TRUE), 
    buoy_count = n_distinct(id),
    measurement_count = n()
  ) %>%
  ungroup()

currents_binned <- left_join(bin_centers, currents_binned, by = "bin_id")

# TODO: is this necessary? -> rather remove those
currents_binned[is.na(currents_binned)] <- 0

currents_binned_table <- arrow_table(
  currents_binned,
  schema = schema(
    bin_id = int32(),
    lon = double(),
    lat = double(),
    speed_sum = double(),
    speed_avg = double(),
    ve_avg = double(),
    vn_avg = double(),
    buoy_count = int32(),
    measurement_count = int32()
  )
)
write_parquet(currents_binned_table, "plastic-drift-app/datasources/binned_currents.parquet")
