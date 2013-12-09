library(XML)
library(lubridate)
library(ggplot2)
library(stringr)
library(gtable)
library(grid)
#library(scales)

if(!file.exists("Observed")){
  system("unzip Observed.zip")
}

system(paste("mv ~/Downloads/fuelups.csv", getwd()))

fuel <- read.csv("fuelups.csv", stringsAsFactors=F)
fuel$date <- parse_date_time(fuel$fuelup_date, "ymd-hm")

start.date <- min(fuel$date)
end.date <- max(fuel$date)
n <- floor(as.numeric(end.date - start.date))
day.vec <- start.date + c(0:n) * days(1)

extract.max.temp <- function(date.field){
  # this function reads the html page for a given date, extracts and returns
  # the high temp for the day
  date.string <- format(date.field, "%Y-%m-%d")
  fn <- paste("Observed/data/", date.string, sep="")
  data <- try(readLines(fn), silent=T)
  temp <- NA
  if(!inherits(data, "try-error")){
    if(length(data) > 1){
      data <- data[which(data==" TODAY") + 1]
      temp <- str_extract(data, "[0-9]+")
    }
  }
  if(length(temp)==0){
    temp <- NA
  }
  return(temp)
}

# a loop, because *pply sucks
temps <- c()
for(i in 1:length(day.vec)){
  temps <- c(temps, extract.max.temp(day.vec[i]))
}

temp.df <- data.frame(cbind(date=format(day.vec, "%Y-%m-%d"), 
                            temp=temps), stringsAsFactors=F)

# get avg temps between fillups...
fuel$avg.temp <- rep(NA, nrow(fuel))
for(i in 1:(nrow(fuel)-1)){
  start.date <- format(fuel$date[i], "%Y-%m-%d")
  end.date <- format(fuel$date[i+1], "%Y-%m-%d")
  temps <- temp.df[which(temp.df$date==start.date) : 
                     which(temp.df$date==end.date),]
  avg.temp <- mean(as.numeric(temps$temp), na.rm=T)
  fuel$avg.temp[i+1] <- avg.temp  
}

# first row of data frame fuel is not needed...
fuel <- fuel[-1,]

# traditional way of showing pattern in two series...
p1 <- ggplot(fuel, aes(x=avg.temp, y=mpg)) + geom_point() + 
  geom_smooth(method = lm, se=FALSE) + xlab("AVG Temp") + ylab("MPG")
ggsave(plot=p1, "tempBYmpg.jpg")

# the taboo way of doing it, two series on same graph...
grid.newpage()

# two plots
p1 <- ggplot(fuel, aes(x=date, y=mpg)) + geom_point() + geom_line() + 
  theme_bw() + xlab("Date") + ylab("MPG (Black)")
p2 <- ggplot(fuel, aes(x=date, y=avg.temp)) + geom_point(colour="red") + 
  xlab("Date") + ylab("AVG Temp (Red)") +
  geom_line(colour="red") + theme_bw() + 
  theme(axis.title.y=element_text(angle = -90)) %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
alab <- g2$grobs[[which(g2$layout$name=="ylab")]]
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1 )
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1 )
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 2, pp$b)
g <- gtable_add_grob(g, alab, pp$t, length(g$widths) - 1, pp$b)

# draw it
jpeg(filename="tempANDmpg.jpg", width=800, height=600)
grid.draw(g)
dev.off()
