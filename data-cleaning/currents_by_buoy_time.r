library(dplyr)

currents <- read.table("data-cleaning/buoydata_15001_jul24.dat", header=TRUE)
colnames(currents) <- c("id", "something", "time", "date", "lat", "lon", "t", "ve", "vn", "speed", "varlat", "varlon", "vart")
currents <- subset(currents, select = -c(something, varlat, varlon, vart) )

# Convert longitude (lon) to the -180 to 180 range
currents$lon <- ifelse(currents$lon > 180, currents$lon - 360, currents$lon)
currents <- currents %>% filter(lon >= -180 & lon <= 180, lat >= -90 & lat <= 90)
currents <- currents %>% filter(ve < 999, vn < 999, speed < 999)


length(unique(currents[["id"]]))
# 9557 different buoys
# Cluster by Buoy & Time (Daily Averages per Buoy)
# assuming time is hour
daily_currents <- currents %>%
  arrange(id, time) %>%  # Ensure ordered time per buoy
  mutate(month_id = cumsum(time == 1))

# Create a unique "day index" that combines month and time
daily_currents <- daily_currents %>%
  mutate(day = (month_id - 1) * 31 + floor(time))  # Offset each month by 31 days

# Group by unique day per buoy and average values
currents_daily <- daily_currents %>%
  group_by(id, day) %>%
  summarise(
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE),
    ve = mean(ve, na.rm = TRUE),
    vn = mean(vn, na.rm = TRUE),
    speed = mean(speed, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
head(currents_daily, 5)
nrow(currents_daily)
summary(currents_daily)
write.csv(currents_daily, "plastic-drift-app/datasources/currents_by_buoy_time.csv", row.names = FALSE)
