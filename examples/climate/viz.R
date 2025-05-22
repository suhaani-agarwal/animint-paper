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

climate$formatted_time <- format(ymd(climate$date), "%Y-%m")
print(head(climate))

# Defining the viz list
plot_list <- list()

# Surface deviation map and temperature map
for(var.name in var.names){
  long.name <- long.names[[var.name]]
    plot_list[[paste0(var.name, "Map")]] <- 
    ggplot() + 
    theme_bw()+
    theme_animint(width=400, height=450) +
    geom_tile(aes_string(x="long", y="lat", fill=var.name, key = "id"
                  ), showSelected="time2", color = NA, alpha = 1,
              data=climate)+ 
    scale_fill_gradient2("deg. C", low="#0000ae", mid="white", high="#ff1919",
                         midpoint=0, limits=lims[[var.name]]) + 
    ggtitle(long.name)+
    geom_path(aes(long, lat, group=group), col="#757272",
              data=countries) + 
    geom_text(aes(-86, 39, label=textdate), showSelected="time2",
              data=dates)+ 
    theme(axis.line=element_blank(), axis.text=element_blank(), 
          axis.ticks=element_blank(), axis.title=element_blank())
    
  
}

# Main visualization definition
viz <- c(
  plot_list, # This includes both surftempMap and surfdevMap

  list(
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
    aes(
      x = surftemp,
      y = surfdev,
      key = id,
      tooltip = paste("Region:", id)
    ),
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
    aes(
      x = surftemp,
      y = surfdev,
      color = Selected
    ),
    data = transform(climate, Selected = "Region"),
    fill = selected.color.region,
    size = 3.5,
    alpha = 1,
    showSelected = c("id", "time2"),
    help = "Highlighted point showing selected region's temperature data."
  ) +
  scale_color_manual(
    name = "Selected",
    values = c("Region" = selected.color.region)
    
  ),


    
surftempTimeSeries = ggplot() +
        theme_bw() +
        theme_animint(width=450, height=450) +
        geom_hline(yintercept = 0) +
        xlab("Year of measurement") +
        ylab("Surface Temperature (deg. Celsius)") +
      
      geom_tallrect(
        aes(
          xmin = time2 - 0.05,
          xmax = time2 + 0.05,
          tooltip = formatted_time,
          key = time2 # Key matches selection variable
        ),
        color = selected.color.time,
        fill = selected.color.time,
        data = unique(climate[, c("time2", "formatted_time")]),
        alpha = 0.5,
        clickSelects = "time2"
      ) +
      
      # Visible selection indicator to get smooth transitions
      # geom_tallrect(
      #   aes(
      #     xmin = time2 - 0.05,
      #     xmax = time2 + 0.05,
      #     tooltip = formatted_time,
      #     key = 1
      #   ),
      #   data = unique(climate[, c("time2", "formatted_time")]),
      #   alpha = 0.5,
      #   color = selected.color.time,
      #   fill = selected.color.time,
      #   showSelected = "time2"  # Only shows for selected time
      # ) +
  
        # All lines (gray)
        geom_line(
          aes(x = time2, y = surftemp, group = id),
          data = climate,
          colour = "black",
          size = 1.5,
          alpha = 0.55,
          clickSelects = "id"
        ) +
        
        geom_line(
          aes(x = time2, y = surftemp),
          color = selected.color.region,
          data = climate,
          size = 3,
          alpha = 1,
          showSelected = "id",
          clickSelects = "id"
        ) ,


      # Surface temperature deviation time series plot
      surfdevTimeSeries = ggplot() +
      theme_bw()+
      theme_animint(width=450, height=450) +
      geom_hline(yintercept = 0, help = "Horizontal zero line for reference.") +
      xlab("Year of measurement") +
      ylab(getlab(var.names[[2]])) +
      # make_tallrect(climate, "time2", 
      #               color = selected.color.time, 
      #               fill = selected.color.time ) +
      geom_tallrect(
        aes(
          xmin = time2 - 0.05,
          xmax = time2 + 0.05,
          key = time2,
          tooltip = formatted_time
        ),
        color = selected.color.time,
        fill = selected.color.time,
        data = unique(climate[, c("time2", "formatted_time")]),
        alpha = 0.5,  
        clickSelects = "time2"
      ) +
      
      # Visible selection indicator to get smooth transitions
      # geom_tallrect(
      #   aes(
      #     xmin = time2 - 0.05,
      #     xmax = time2 + 0.05,
      #     tooltip = formatted_time,
      #     key = 1
      #   ),
      #   data = unique(climate[, c("time2", "formatted_time")]),
      #   alpha = 0.5,
      #   color = selected.color.time,
      #   fill = selected.color.time,
      #   showSelected = "time2"  # Only shows for selected time
      # ) +
  
      
      # All lines (gray)
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
          aes(x = time2, y = surfdev),
          color = selected.color.region,
          data = climate,
          size = 3,
          alpha = 1,
          showSelected = "id",
          clickSelects = "id"
        ) ,

    # ScatterHere plot
      scatterHere = ggplot() +
      make_text(climate, 20, -7, "id", "all times for region %s") +
      theme_bw() +
      theme_animint(width=420, height=450) +
      xlab(getlab(var.names[[1]])) +
      ylab(getlab(var.names[[2]])) +
      geom_hline(yintercept = 0, help = "Horizontal zero line for reference.") +
      geom_vline(xintercept = 0, help = "Vertical zero line for reference.") +
      xlim(-10, 40) +
      ylim(-8, 8) +
      # All points (unselected)
      geom_point(
    aes(
      x = surftemp,
      y = surfdev,
      key = time2,
      tooltip = formatted_time
    ),
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
    aes(
      x = surftemp,
      y = surfdev,
      color = Selected,
      tooltp = paste("Region:", id)
    ),
    data = transform(climate, Selected = "Time"),
    fill = selected.color.time,
    size = 3.5,
    alpha = 1,
    showSelected = c("time2", "id"),
    help = "Highlighted point showing selected time's temperature data for the region."
  ) +
  scale_color_manual(
    name = "Selected",
    values = c("Time" = selected.color.time)
  ),
      
      # Animation parameters
      duration = list(time2 = 3000, id = 2500),
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
)

# Generate the visualization
animint2pages(viz, "central-american-temperature-maps")

