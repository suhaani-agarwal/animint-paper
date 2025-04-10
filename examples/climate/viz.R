library(animint2)
library(maps)
library(lubridate)
library(plyr)

# Data preparation
data(climate)
climate$time2 <- decimal_date(ymd(as.character(climate$date)))
countries <- map_data("world")
countries <- subset(countries, (lat < 38) & (lat > -24))
countries <- subset(countries, ((-long) > 54) & ((-long) < 118))
climate <- ddply(climate, .(id, month), transform,
               tempdev = temperature - mean(temperature),
               surfdev = surftemp - mean(surftemp))
climate <- climate[order(climate$date, climate$id), ]
dates <- ddply(climate, .(date), summarise, 
             month = month[1], year = year[1],
             time2 = time2[1], textdate = paste(month.name[month], year))
dates <- dates[order(dates$date),]

# Visualization parameters
long.names <- c(surftemp = "Surface temperature",
              temperature = "Temperature",
              surfdev = "Deviation from monthly norm")
lims <- list(surftemp = c(-10, 40),
           surfdev = c(-8, 8))
var.names <- c("surftemp", "surfdev")
dot.alpha <- 6/10
selected.color <- "#eb0deb"
getlab <- function(var.name){
  sprintf("%s (degrees Celsius)", long.names[[var.name]])
}

# Main visualization definition
viz <- list(
  surftempMap = ggplot() + 
    theme_bw() +
    geom_tile(aes(x = long, y = lat, fill = surftemp, key = id),
            clickSelects = "id", showSelected = "time2",
            data = climate) + 
    scale_fill_gradient2("deg. C", low = "blue", mid = "white", high = "#ff0000",
                       midpoint = 0, limits = lims$surftemp) + 
    ggtitle("Surface temperature") +
    geom_path(aes(long, lat, group = group), col = "#908f8f",
            data = countries) + 
    geom_text(aes(-86, 39, label = textdate,key = paste("date", time2)), showSelected = "time2",
            data = dates) + 
    theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()),
  
  # surfdevMap = ggplot() + 
  #   theme_bw() +
  #   geom_tile(aes(x = long, y = lat, fill = surfdev, key = id),
  #           clickSelects = "id", showSelected = "time2",
  #           data = climate) + 
  #   scale_fill_gradient2("deg. C", low = "blue", mid = "white", high = "#ff0000",
  #                      midpoint = 0, limits = lims$surfdev) + 
  #   ggtitle("Deviation from monthly norm") +
  #   geom_path(aes(long, lat, group = group), col = "#908f8f",
  #           data = countries) + 
  #   geom_text(aes(-86, 39, label = textdate,key = paste("date", time2)), showSelected = "time2",
  #           data = dates) + 
  #   theme(axis.line = element_blank(), 
  #       axis.text = element_blank(), 
  #       axis.ticks = element_blank(), 
  #       axis.title = element_blank()),
  surfdevMap = ggplot() + 
  theme_bw() +
  geom_tile(aes(
    x = long, 
    y = lat, 
    fill = surfdev,
    key = id  # Add key based on tile identifier
  ),
  clickSelects = "id", 
  showSelected = "time2",
  data = climate) + 
  scale_fill_gradient2("deg. C", 
    low = "blue", 
    mid = "white", 
    high = "#ff0000",
    midpoint = 0, 
    limits = lims$surfdev) + 
  ggtitle("Deviation from monthly norm") +
  geom_path(aes(long, lat, group = group), 
    col = "#908f8f",
    data = countries) + 
  geom_text(aes(-86, 39, label = textdate, key = paste("date", time2)), 
    showSelected = "time2",
    data = dates) + 
  theme(axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank()),
  
  scatterNow = ggplot() +
    geom_text(aes(20, -7, label = sprintf("all regions in %s", textdate), key = 1),
            showSelected = "time2",
            data = dates) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-10, 40) +
    ylim(-8, 8) +
    xlab(getlab(var.names[[1]])) +
    ylab(getlab(var.names[[2]])) +
    geom_point(aes_string(x = var.names[[1]], y = var.names[[2]]),
             clickSelects = "id", showSelected = "time2",
             data = climate, alpha = dot.alpha,
             selected.color = "black", selected.fill = selected.color),
  
  surftempTimeSeries = ggplot() +
    theme_animint(width = 463) +
    geom_hline(yintercept = 0) +
    make_tallrect(climate, "time2") +
    xlab("Year of measurement") +
    ylab(getlab(var.names[[1]])) +
    geom_line(aes_string(x = "time2", y = var.names[[1]],
                         group = "id"), clickSelects = "id", color = "violet", alpha = 0.53,
              data = climate,
              selected.color = "violet", selected.fill = selected.color, color_off = "black", z_off = 0.1),
  
  surfdevTimeSeries = ggplot() +
    theme_animint(width = 463) +
    geom_hline(yintercept = 0) +
    make_tallrect(climate, "time2") +
    xlab("Year of measurement") +
    ylab(getlab(var.names[[2]])) +
    geom_line(aes_string(x = "time2", y = var.names[[2]],
                         group = "id"), clickSelects = "id", color = "violet", alpha = 0.53,
              data = climate,
              selected.color = "violet", selected.fill = selected.color, color_off = "black", z_index = 100),
  
  scatterHere = ggplot() +
    make_text(climate, 20, -7, "id", "all times for region %s") +
    theme_bw() +
    xlab(getlab(var.names[[1]])) +
    ylab(getlab(var.names[[2]])) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlim(-10, 40) +
    ylim(-8, 8) +
    geom_point(aes_string(x = var.names[[1]], y = var.names[[2]]), clickSelects = "time2", showSelected = "id",
             data = climate, alpha = dot.alpha,
             selected.color = "black", selected.fill = selected.color),
  
  # Animation parameters
  duration = list(time2 = 1000, id = 1000),
  time = list(
    variable = "time2", 
    ms = 2000,
    smooth = TRUE
  ),
  selector.types = list(
    time2 = "single",
    id = "single"
  ),
  first = list(
    time2 = min(climate$time2),
    id = climate$id[1]
  ),
  title = "Central American Temperature Maps",
  source = 'http://github.com/suhaani-agarwal/animint-paper/blob/master/examples/climate/viz.R'
)

# Generate the visualization
animint2pages(viz, "central-american-temperature-maps")