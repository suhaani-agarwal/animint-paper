library(animint2)
library(maps)
library(lubridate)
library(plyr)
library(data.table)

# Data preparation
data(climate)
climate$time2 <- decimal_date(ymd(as.character(climate$date)))

# Prepare map data
countries <- map_data("world")
countries <- subset(countries, (lat < 38) & (lat > -24))
countries <- subset(countries, ((-long) > 54) & ((-long) < 118))

# Calculate monthly deviations
climate <- ddply(climate, .(id, month), transform,
               tempdev = temperature - mean(temperature),
               surfdev = surftemp - mean(surftemp))
climate <- climate[order(climate$date, climate$id), ]

# Prepare date labels
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

selected.color.region <- "#59d715"  # Green for selected region
selected.color.time <- "#263ff9"  # Blue for the selected time
getlab <- function(var.name){
  sprintf("%s (deg. Celsius)", long.names[[var.name]])
}

legend_data <- data.frame(
  label = c("region", "time"),
  color = c(selected.color.region, selected.color.time),
  # Using extreme x/y values that won't overlap with real data
  x = c(-Inf, -Inf),
  y = c(-Inf, -Inf)
)


# Main visualization definition
viz <- list(
  #Surface Temperature map
  surftempMap = ggplot() + 
    theme_bw() +
    theme_animint(width=420, height=450) +
    geom_tile(aes(x = long, y = lat, fill = surftemp, key = paste(id, time2)), color = selected.color.region ,
            clickSelects = "id", showSelected = "time2",
            data = climate,
            help = "Map showing surface temperature by region. Click to select a region.") + 
    scale_fill_gradient2("deg. C", low = "blue", mid = "white", high = "#ff0000",
                       midpoint = 0, limits = lims$surftemp) + 
    ggtitle("Surface temperature") +
    geom_path(aes(long, lat, group = group), col = "#908f8f",
            data = countries,
            help = "Country borders on the temperature map.") + 
    geom_text(aes(-86, 39, label = textdate,key = paste("date", time2)), showSelected = "time2",
            data = dates,
            help = "Display of current month and year being viewed.") + 
    theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()),

    #Surface Temperature deviation map (monthly)
    surfdevMap = ggplot() + 
    theme_bw() +
    theme_animint(width=420, height=450) +
    geom_tile(aes(
      x = long, 
      y = lat, 
      fill = surfdev,
      key = paste(time2,id),group = id
    ),color = selected.color.region,
    help = "Map showing deviation from monthly temperature norms. Click to select a region.",
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
      data = countries,
      help = "Country borders on the deviation map.") + 
    geom_text(aes(-86, 39, label = textdate, key = paste("date", time2)), 
      showSelected = "time2",
      data = dates,
      help = "Display of current month and year being viewed.") + 
    theme(axis.line = element_blank(), 
      axis.text = element_blank(), 
      axis.ticks = element_blank(), 
      axis.title = element_blank()),
    
  # Current time scatterplot
    scatterNow = ggplot() +
      geom_text(
        aes(20, -7, label = sprintf("all regions in %s", textdate), key = 1),
        showSelected = "time2",
        data = dates,
        help = "Title showing current time period for the scatterplot."
      ) +
      theme_bw() +
      geom_hline(yintercept = 0, help = "Horizontal zero line for reference.") +
      geom_vline(xintercept = 0, help = "Vertical zero line for reference.") +
      xlim(-10, 40) +
      ylim(-8, 8) +
      xlab(getlab(var.names[[1]])) +
      ylab(getlab(var.names[[2]])) +
      theme_animint(width=420, height=450) +
      # All points (unselected)
      geom_point(
        aes_string(x = var.names[[1]], y = var.names[[2]], key = "id"),
        data = climate,
        alpha = 0.7,
        color = "black",
        fill = "grey",
        size = 2.5,
        clickSelects = "id",
        showSelected = "time2",
        help = "All regions' temperature data points for the selected time."
      ) +
      # Selected points (only shown when id is selected)
      geom_point(
        aes_string(x = var.names[[1]], y = var.names[[2]]),
        data = climate,
        color = "#86ff86",
        fill = selected.color.region,
        size = 3.5,               # Slightly larger when selected
        alpha = 1,              # Opaque when selected
        showSelected = c("id", "time2") , # Only show for selected id and time2
        help = "Highlighted point showing selected region's temperature data."
      ),

    # Surface temperature time series plot
    surftempTimeSeries = ggplot() +
    theme_bw()+
      theme_animint(width=410, height=450) +
      geom_hline(yintercept = 0, help = "Horizontal zero line for reference.") +
      
      xlab("Year of measurement") +
      ylab(getlab(var.names[[1]])) +
      make_tallrect(climate, "time2", 
                    color = selected.color.time, 
                    help = "Interactive time selector - click or drag to change time.",
                    fill = selected.color.time) +
      # All lines
      geom_line(
        aes_string(x = "time2", y = var.names[[1]], group = "id"),
        data = climate,
        colour = "black",
        size = 1.5,
        alpha = 0.55,
        clickSelects = "id",
        help = "All regions' temperature time series."
      ) +

      # Selected region (green)
      geom_line(
        aes_string(x = "time2", y = var.names[[1]], key = 1),
        data = climate,
        colour = selected.color.region, # Green for selected
        size = 3,             # Slightly bolder when selected
        alpha = 2,              # Opaque when selected
        showSelected = "id" ,    # Only show for selected id
        help = "Highlighted line showing selected region's temperature over time."
      )+
      
      # Add legend using completely invisible and non-intrusive points
      geom_line(
        aes(x = x, y = y, color = label),
        data = legend_data,
        alpha = 0,
        size = 0
      ) +
      
      # Manual color scale
      scale_color_manual(
        name = "Selected",
        values = c("region" = selected.color.region, 
                  "time" = selected.color.time)
      ) +
      
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, clickSelects = FALSE))) ,


      # Surface temperature deviation time series plot
      surfdevTimeSeries = ggplot() +
      theme_bw()+
      theme_animint(width=410, height=450) +
      geom_hline(yintercept = 0, help = "Horizontal zero line for reference.") +
      make_tallrect(climate, "time2", 
                    color = selected.color.time, 
                    help = "Interactive time selector - click or drag to change time.",
                    fill = selected.color.time) +
      xlab("Year of measurement") +
      ylab(getlab(var.names[[2]])) +
      geom_line(
        aes_string(x = "time2", y = var.names[[2]], group = "id"),
        data = climate,
        colour = "black",
        size = 1.5,
        alpha = 0.55,
        clickSelects = "id",
        help = "All regions' temperature deviation time series."
      ) +
      geom_line(
        aes_string(x = "time2", y = var.names[[2]], key = 1),
        data = climate,
        colour = selected.color.region,
        size = 2,
        alpha = 1,
        showSelected = "id",
        help = "Highlighted line showing selected region's temperature deviation over time."
      )+
    
      # Add legend using completely invisible and non-intrusive points
      geom_line(
        aes(x = x, y = y, color = label),
        data = legend_data,
        alpha = 0,
        size = 0
      ) +
      
      # Manual color scale
      scale_color_manual(
        name = "Selected",
        values = c("region" = selected.color.region, 
                  "time" = selected.color.time)
      ) +
      
      guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, clickSelects = FALSE))) , 

    # ScatterHere plot
      scatterHere = ggplot() +
      make_text(climate, 20, -7, "id", "all times for region %s") +
      theme_bw() +
      theme_animint(width=430, height=450) +
      xlab(getlab(var.names[[1]])) +
      ylab(getlab(var.names[[2]])) +
      geom_hline(yintercept = 0, help = "Horizontal zero line for reference.") +
      geom_vline(xintercept = 0, help = "Vertical zero line for reference.") +
      xlim(-10, 40) +
      ylim(-8, 8) +
      # All points (unselected)
      geom_point(
        aes_string(x = var.names[[1]], y = var.names[[2]], key = "time2"),
        data = climate,
        alpha = 0.7,
        color = "black",
        fill = "grey",
        size = 2.5,
        clickSelects = "time2",
        showSelected = "id",
        help = "All time points' temperature data for the selected region."
      ) +
      # Selected points (only shown when time2 is selected)
      geom_point(
        aes_string(x = var.names[[1]], y = var.names[[2]]),
        data = climate,
        color = "#9595ff",  # Blue for selected time
        fill = selected.color.time,
        size = 3.5,               # Slightly larger when selected
        alpha = 1,              # Opaque when selected
        showSelected = c("time2", "id"),  # Only show for selected time2 and id
        help = "Highlighted point showing selected time's temperature data for the region."
      ),
      
      # Animation parameters
      duration = list(time2 = 2000, id = 1000),
      time = list(
        variable = "time2", 
        ms = 3000
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