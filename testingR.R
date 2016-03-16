# Try various R scripts.

#########################################################################################
###############                         1                           #####################
###############     Avoid overlapping labels in ggplot2 charts      #####################
#########################################################################################
# http://blog.revolutionanalytics.com/2016/01/avoid-overlapping-labels-in-ggplot2-charts.html
# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
# If you've ever created a scatterplot with text labels using the text function in R, or the geom_text function in the ggplot2 package, you've probably found that the text labels can easily overlap, rendering some of them unreadable. Now, thanks to the new extensibility capabilities of the ggplot2 package, R user Kamil Slowikowski has created an R package ggrepel that adds alternative text labeling functions to ggplot2 that "repels" labels from data points and other labels to avoid overlapping. The new geom_text_repel replaces the standard geom_text for plain text lablels, and you can also use geom_label_repel instead of geom_label for these rounded and color-coded labels:
library(ggplot2)
library(ggrepel)
set.seed(42)
ggplot(mtcars) +
  geom_point(aes(wt, mpg), size = 5, color = 'grey') +
  geom_label_repel(
    aes(wt, mpg, fill = factor(cyl), label = rownames(mtcars)),
    fontface = 'bold', color = 'white',
    box.padding = unit(0.25, "lines"),
    point.padding = unit(0.5, "lines")
  ) +
  theme_classic(base_size = 16)

#########################################################################################
###############                         2                           #####################
###############               Create your own hexamaps              #####################
#########################################################################################
# http://www.56n.dk/create-your-own-hexamaps/
# http://www.r-bloggers.com/create-your-own-hexamaps/

# Hexamaps are gaining in popularity. Most notably has been the versions, where the map of the USA has been made into a hexamap. But people have also made maps of Europe using hexagons. The idea is that one unit is one hexagon. So in case of the US, each state is one hexagon. In the case of Europe, each country is a hexagon. This means that all units (states, countries, etc.) are the same size. This of course skews the hexamap in relation to the real geographic proportions. But it gives the advantage of giving all units equal size for displaying information – for instance a shade or color depending on some underlying values. - See more at: http://www.56n.dk/create-your-own-hexamaps/#sthash.QU2l31qk.dpuf

# To ease the process I’ve made the hexamapmaker package. It takes a set of points and turns them into hexagons. That means that you can quickly and easily design and produce hexamaps.

# Below I’ve included the example code from the package if you want to get started yourself. If you create a map of your own please share it with me on twitter @mikkelkrogsholm. I’d love to see your work!
# Install hexamapmaker
library(devtools)
install_github("56north/hexamapmaker")
library(hexamapmaker)

# Create data frame
# Notice the spacing of the points

x <- c(1,3,2,4,1,3,7,8)
y <- c(1,1,3,3,5,5,1,3)
id <- c("test1", "test2", "test3", "test4", "test5", "test6", "test7", "test8")
z <- data.frame(id,x,y)

# Plot points

library(ggplot2)
ggplot(z, aes(x, y, group = id)) +
  geom_point() +
  coord_fixed(ratio = 1) +
  ylim(0,max(y)) + xlim(0,max(x))

# Turn points into hexagons

library(hexamapmaker)

zz <- hexamap(z)

ggplot(zz, aes(x, y, group = id)) +
  geom_polygon(colour="black", fill = NA) +
  coord_fixed(ratio = 1)


#########################################################################################
###############                         3                           #####################
###############                 use emoji font in R                 #####################
#########################################################################################
# http://guangchuangyu.github.io/2015/12/use-emoji-font-in-r/
# http://www.r-bloggers.com/use-emoji-font-in-r/
# I implemented my solution of using emoji in the R package emojifont. The package is very simple, pack some emoji fonts (currently only OpenSansEmoji.ttf) and use showtext to render the fonts, then we can use the font in either base plot or ggplot2.
# Install
devtools::install_github("GuangchuangYu/emojifont")
# load emoji font
library(emojifont)
load.emojifont("OpenSansEmoji.ttf")
library(ggplot2)
# To use emoji, we need to use their corresponding unicode. Emoji unicode can be found in http://apps.timwhitlock.info/emoji/tables/unicode, or searched using remoji package.
devtools::install_github("richfitz/remoji")
library(remoji)
# Plotting
set.seed(123)
x <- rnorm(10) 
set.seed(321) 
y <- rnorm(10)
dd=data.frame(x=emoji(c("satisfied", "disapointed")), y=c(50, 10)) 
emoji_text=element_text(family="OpenSansEmoji", size=20) 
ggplot(dd, aes(x, y)) + geom_bar(stat='identity', aes(fill=x)) + 
  ggtitle(paste(emoji(c("+1", "-1")), collapse=" ")) + 
  theme(axis.text.x = emoji_text, legend.text=emoji_text, title=emoji_text) + 
  xlab(NULL) + ylab(NULL)

#########################################################################################
###############                         4                           #####################
###############            Some basics for base graphics            #####################
#########################################################################################
# http://blog.revolutionanalytics.com/2015/01/some-basics-for-base-graphics.html
# Some handy plotting parameters
attach(mtcars)

par(mfrow = c(1,2))                    # Put 2 plots on the same device
plot(disp,mpg)                        

plot(disp,mpg,
     main = "MPG vs. Displacement",    # Add a title
     type = "p",
     col = "grey",                     # Change the color of the points
     pch = 16,                         # Change the plotting symbol  see help(points)
     cex = 1,                          # Change size of plotting symbol     
     xlab = "Displacement (cu. in)",   # Add a label on the x-axis
     ylab = "Miles per Gallon",        # Add a label on the y-axis
     bty = "n",                        # Remove the box around the plot
     #asp = 1,                         # Change the y/x aspect ratio see help(plot)
     font.axis = 1,                    # Change axis font to bold italic
     col.axis = "black",               # Set the color of the axis
     xlim = c(85,500),                 # Set limits on x axis
     ylim = c(10,35),                  # Set limits on y axis
     las=1)                            # Make axis labels parallel to x-axis

abline(lm(mpg ~ disp),                 # Add regression line y ~ x
       col="red",                      # regression line color
       lty = 2,                        # use dashed line
       lwd = 2)                        # Set thickness of the line

lines(lowess(mpg ~ disp),              # Add lowess line y ~ x
      col="dark blue",                 # Set color of lowess line
      lwd= 2)                          # Set thickness of the lowess line

leg.txt <- c("red = lm", "blue = lowess") # Text for legend
legend(list(x = 180,y = 35),           # Set location of the legend
       legend = leg.txt,               # Specify text 
       col = c("red","dark blue"),     # Set colors for legend
       lty = c(2,1),                   # Set type of lines in legend
       merge = TRUE)                   # merge points and lines


#########################################################################################
###############                         5                           #####################
###############    How to Layout and Design an Infographic with R   #####################
#########################################################################################
# http://alstatr.blogspot.com.tr/2015/02/r-how-to-layout-and-design-infographic.html
# This article will serve as a template for more infographic design that I plan to share on future posts. 
# To start with, we need to setup our data first. And for illustration purposes, we will use a simulated data: 
y1 <- round(rnorm(n = 36, mean = 7, sd = 2)) # Simulate data from normal distribution
y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
x <- rep(LETTERS[1:12], 3)
grp <- rep(c("Grp 1", "Grp 2", "Grp 3"), each = 12)
dat <- data.frame(grp, x, y1, y2, y3)
# Use special fonts:
library(extrafont)
font_import()
loadfonts()
library(useful)
# To arrive on the design of the bar plot in the infographic we use the following theme,
# Configure Theme
kobe_theme <- function() {
  theme(
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.x = element_line(colour = "#E7A922"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}
x_id <- rep(12:1, 3) # use this index for reordering the x ticks
library(ggplot2)
p1 <- ggplot(data = dat, aes(x = reorder(x, x_id), y = y1)) + geom_bar(stat = "identity", fill = "#552683") +
  coord_flip() + ylab("Y LABEL") + xlab("X LABEL") + facet_grid(. ~ grp) +
  ggtitle("TITLE OF THE FIGURE")
p1 + kobe_theme()

p2 <- ggplot(data = dat, aes(x = x, y = y2, group = factor(grp))) +
  geom_line(stat = "identity", aes(linetype = factor(grp)), size = 0.7, colour = "#552683") +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
p2
p2 + kobe_theme()

kobe_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Impact", colour = "#552683", size = 10),
    legend.background = element_rect(fill = "#E2E2E3"),
    legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    legend.text = element_text(family = "Impact", colour = "#E7A922", size = 10),
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "#E7A922", family = "Impact"),
    plot.title = element_text(colour = "#552683", face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = "#552683", face = "bold", size = 13, family = "Impact"),
    panel.grid.major.y = element_line(colour = "#E7A922"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = "#E7A922"),
    axis.ticks = element_line(colour = "#E7A922")
  )
}

p2 + kobe_theme2()
p2 + kobe_theme2() + scale_linetype_discrete("GROUP")
p3 <- ggplot(data = dat, aes(x = reorder(x, rep(1:12, 3)), y = y3, group = factor(grp))) +
  geom_bar(stat = "identity", fill = "#552683") + coord_polar() + facet_grid(. ~ grp) +
  ylab("Y LABEL") + xlab("X LABEL") + ggtitle("TITLE OF THE FIGURE")
p3
p3 + kobe_theme2()
# Infographic layout:
library(grid)
pdf("~/Documents/Infographics1.pdf", width = 10, height = 20)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INFOGRAPHIC", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text("RProgramming", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Impact", col = "#E7A922", cex = 6.4))
grid.text("BY AL-AHMADGAID B. ASAAD", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text("ANALYSIS WITH PROGRAMMING", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text("alstatr.blogspot.com", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
print(p3, vp = vplayout(4, 1:3))
print(p1, vp = vplayout(3, 1:3))
print(p2, vp = vplayout(2, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#CA8B01", cex = 13, alpha = 0.3))
grid.text("A VERY VERY VERY VERY LONG TITLE", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 1.2))
grid.text("DATA INFO", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Impact", col = "white", cex = 1.2))
grid.text(paste(
  "Syndicated to",
  "Source",
  "Author",
  "Maintainer",
  "Frequency of Update",
  "Granularity",
  "Temporal Date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text(paste(
  "http://alstatr.blogspot.com",
  "http://alstatr.blogspot.com",
  "Analysis with Programming",
  "Al-Ahmadgaid B. Asaad",
  "Annually",
  "National",
  "2011-2013", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
dev.off()

# Generate Infographic in PNG Format
png("~/Documents/Infographics1.png", width = 10, height = 20, units = "in", res = 500)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INFOGRAPHIC", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text("RProgramming", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Impact", col = "#E7A922", cex = 6.4))
grid.text("BY AL-AHMADGAID B. ASAAD", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text("ANALYSIS WITH PROGRAMMING", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text("alstatr.blogspot.com", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
print(p3, vp = vplayout(4, 1:3))
print(p1, vp = vplayout(3, 1:3))
print(p2, vp = vplayout(2, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("CATEGORY", y = unit(0.82, "npc"), x = unit(0.5, "npc"), vjust = .5, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#CA8B01", cex = 13, alpha = 0.3))
grid.text("A VERY VERY VERY VERY LONG TITLE", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 1.2))
grid.text("DATA INFO", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Impact", col = "white", cex = 1.2))
grid.text(paste(
  "Syndicated to",
  "Source",
  "Author",
  "Maintainer",
  "Frequency of Update",
  "Granularity",
  "Temporal Date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
grid.text(paste(
  "http://alstatr.blogspot.com",
  "http://alstatr.blogspot.com",
  "Analysis with Programming",
  "Al-Ahmadgaid B. Asaad",
  "Annually",
  "National",
  "2011-2013", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.79, "npc"), gp = gpar(fontfamily = "Impact", col = "#552683", cex = 0.8))
dev.off()

#########################################################################################
###############                         6                           #####################
###############   Google Trends Visualization and Analysis with R   #####################
#########################################################################################
# http://dirk.eddelbuettel.com/blog/2015/11/29/#gtrendsr-1.3.0
# http://blog.revolutionanalytics.com/2015/12/download-and-plot-google-trends-data-with-r.html
# Google Trends is a useful way to compare changes in popularity of certain search terms over time, and Google Trends data can be used as a proxy for all sorts of difficult-to-measure quantities like economic activity and disease propagation. If you'd like to use Google Trends data in your own analyses, the gtrendsR package for R is now available on CRAN. This package by Philippe Massicotte and Dirk Eddelbuettel adds functions to connect with your Google account, and download Trends data for one or more search terms at daily or weekly resolution over a specified period of time.
library(gtrendsR)
# First you have to connect a google account:
usr <- "username@gmail.com"
psw <- "password"
gconnect(usr, psw)
trends <- gtrends(c("Nanotechnology", "Renewable energy", "Artificial intelligence", "Big data", "3D printing"))
plot(trends)


#########################################################################################
###############                         7                           #####################
###############             Tech Themes for ggplot2                 #####################
#########################################################################################

# https://github.com/ricardo-bion/ggtech

library(ggplot2)
library(ggtech)
d <- qplot(carat, data = diamonds[diamonds$color %in%LETTERS[4:7], ], geom = "histogram", bins=30, fill = color)
d + theme_tech(theme = "facebook")+ ggtitle("Facebook ggplot2 theme") + scale_fill_tech(theme = "facebook")

#########################################################################################
###############                         8                           #####################
###############             Basic ggplot2 examples                  #####################
#########################################################################################

# http://theanalyticalminds.blogspot.com.tr/2015/03/part-3a-plotting-with-ggplot2.html
# DATA PREPARATION
weather <- read.csv("weather_2014.csv",sep=";",stringsAsFactors=FALSE)
weather$season <- factor(weather$season, levels = c("Spring","Summer","Autumn","Winter"))
weather$day <- as.factor(weather$day)
weather$month <- as.factor(weather$month)
weather$dir.wind <- as.factor(weather$dir.wind)
rel <- round(prop.table(table(weather$dir.wind))*100,1)
sort(rel,decreasing = TRUE)
# Transforming wind direction variable: from 16 to 8 principal winds 
  
# Create a copy from the original variable...
weather$dir.wind.8 <- weather$dir.wind 

# ...and then simply recode some of the variables
weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"),
                                 "NE",as.character(weather$dir.wind.8)) 

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("NNW","WNW"),
                               "NW",as.character(weather$dir.wind.8)) 

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("WSW","SSW"),
                               "SW",as.character(weather$dir.wind.8)) 

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("ESE","SSE"),
                               "SE",as.character(weather$dir.wind.8)) 

# create factors, ordered by "levels" 
weather$dir.wind.8 <- factor(weather$dir.wind.8,
                                 levels = c("N","NE","E","SE","S","SW","W","NW"))
round(prop.table(table(weather$dir.wind.8,weather$season),margin = 2)*100,1)
first.day <- "2014-01-01"
first.day <- as.Date(first.day)
weather$date  <- first.day + weather$day.count - 1
# Store date and time as POSIXlt class
l.temp.time.date <- as.POSIXlt(paste(weather$date,weather$l.temp.time))
# Round to the nearest hour
l.temp.time.date <- round(l.temp.time.date,"hours")
weather$l.temp.hour <- l.temp.time.date [["hour"]]
# Lastly, the integer is converted to factor
weather$l.temp.hour <- as.factor(weather$l.temp.hour)

library(ggplot2)
# The R package ggplot2, created by Hadley Wickham, is an implementation of Leland Wilkinson's Grammar of Graphics, which is a systematic approach to describe the components of a graphic. In maintenance mode (i.e., no active development) since February 2014, ggplot2 it is the most downloaded R package of all time.
ggplot(weather, aes(x=date, y=ave.temp)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red", size = 1) +
  ggtitle("Daily average temperature") +
  xlab("Date") + ylab("Average Temperature")

# Same but with colour varying

ggplot(weather,aes(x = date,y = ave.temp)) + 
  geom_point(aes(colour = ave.temp)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 16) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")

# Distribution of the average temperature by season - density plot

ggplot(weather,aes(x = ave.temp, colour = season)) +
  geom_density() +
  scale_x_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Temperature distribution by season") +
  xlab("Average temperature ( ºC )") +  ylab ("Probability")

# Analysing the temperature by month - violin geom with jittered points overlaid

# Label the months - Jan...Dec is better than 1...12
weather$month = factor(weather$month,
                       labels = c("Jan","Fev","Mar","Apr",
                                  "May","Jun","Jul","Aug","Sep",
                                  "Oct","Nov","Dec"))

# Distribution of the average temperature by month - violin plot,
# with a jittered point layer on top, and with size mapped to amount of rain

ggplot(weather, aes(x = month, y = ave.temp)) +
  geom_violin(fill = "orange") +
  geom_point(aes(size=rain), colour = "blue", position = "jitter") +
  ggtitle("Temperature distribution by month") +
  xlab("Month") + ylab("Average temperature (C)")

#Analysing  the correlation between low and high temperatures


# Scatter plot of low vs high daily temperatures, with a smoother curve for each season

ggplot(weather,aes(x = l.temp, y = h.temp)) +
  geom_point(colour = "firebrick", alpha = 0.3) + 
  geom_smooth(aes(colour = season),se= F, size = 1.1) +
  ggtitle ("Daily low and high temperatures") +
  xlab("Daily low temperature ( ºC )") +  ylab ("Daily high temperature ( ºC )") 
