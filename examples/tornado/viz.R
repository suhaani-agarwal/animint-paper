library(maps)
library(plyr)
library(animint2)
data(UStornadoes)
ord <- order(unique(UStornadoes$TornadoesSqMile), decreasing=TRUE)
stateOrder <- data.frame(state = unique(UStornadoes$state)[ord], rank = 1:49)
UStornadoes$state <- factor(UStornadoes$state, levels=stateOrder$state, ordered=TRUE)
UStornadoes$weight <- 1/UStornadoes$LandArea
USpolygons <- map_data("state")
USpolygons$state <- state.abb[match(USpolygons$region, tolower(state.name))]
UStornadoCounts <- ddply(UStornadoes, .(state, year), summarize, count=length(state))
seg.color <- "#55B1F7"
viz <- animint(
  title = 'US Tornado Visualization',
  source = 'https://github.com/suhaani-agarwal/animint-paper/blob/master/examples/tornado/viz.R',
  map = ggplot() +
    theme_bw() +
    theme_animint(width=750, height=500) +
    make_text(UStornadoes, -100, 50, "year", "Tornado paths and endpoints in %d") +
    geom_segment(aes(x=startLong, y=startLat, xend=endLong, yend=endLat),
                colour=seg.color, 
                showSelected="year",
                data=UStornadoes) +
    geom_polygon(aes(x=long, y=lat, group=group),
                 clickSelects="state",
                 data=USpolygons, 
                 fill="#000000",
                 colour="white", 
                 size=0.5, 
                 alpha=1) + 
    geom_point(aes(endLong, endLat),               
               colour=seg.color, 
               showSelected="year",
               data=UStornadoes) +
    theme(axis.line=element_blank(), 
    panel.background = element_rect(fill = "white", colour = "white"),
    panel.border = element_rect(colour = "white", fill = NA, size = 2),
          axis.text=element_blank(), 
          axis.ticks=element_blank(), 
          axis.title=element_blank()),
  
  ts = ggplot() +
    theme_animint(width=300, height=400) +
    xlab("year") +
    ylab("Number of tornadoes") +
    geom_bar(aes(year, count),
             clickSelects="year", 
             showSelected="state",
             data=UStornadoCounts, 
             stat="identity", 
             color = "black",
             fill = "grey",
             alpha = 2,
             position="identity") +
    make_text(UStornadoes, 1980, 200, "state") +
    geom_text(aes(year, count + 5, label=count),
              showSelected=c("state", "year"),
              data=UStornadoCounts, 
              size=20)
)
animint2pages(viz, "tornado-visualization")
