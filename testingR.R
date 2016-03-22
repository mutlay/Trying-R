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

#########################################################################################
###############                         9                           #####################
###############      Interpreting regression coefficient in R       #####################
#########################################################################################

# https://biologyforfun.wordpress.com/2014/11/23/interpreting-regression-coefficient-in-r/

# let's simulate the data the explanatory variables: temperature (x1),
# precipitation (x2) and the treatment (1=Control, 2= N addition)
set.seed(1)
x1 <- rnorm(100, 10, 2)
x2 <- rnorm(100, 100, 10)
x3 <- gl(n = 2, k = 50)
modmat <- model.matrix(~x1 + x2 + x3, data = data.frame(x1, x2, x3))
# vector of fixed effect
betas <- c(10, 2, 0.2, 3)
# generate data
y <- rnorm(n = 100, mean = modmat %*% betas, sd = 1)
# first model
m <- lm(y ~ x1 + x2 + x3)
summary(m)

# Now let’s make a figure of the effect of temperature on soil biomass

plot(y ~ x1, col = rep(c("red", "blue"), each = 50), pch = 16, xlab = "Temperature [°C]", ylab = "Soil biomass [mg]")
abline(a = coef(m)[1], b = coef(m)[2], lty = 2, lwd = 2, col = "red")

# What happened there? It seems as if our model is completely underestimating the y values … Well what we have been drawing is the estimated effect of temperature on soil biomass for the control group and for a precipitation of 0mm, this is not so interesting, instead we might be more interested to look at the effect for average precipitation values:

plot(y ~ x1, col = rep(c("red", "blue"), each = 50), pch = 16, xlab = "Temperature [°C]", ylab = "Soil biomass [mg]")
abline(a = coef(m)[1] + coef(m)[3] * mean(x2), b = coef(m)[2], lty=2, lwd = 2, col="red")
abline(a = coef(m)[1] + coef(m)[4] + coef(m)[3] * mean(x2), b = coef(m)[2], lty = 2, lwd = 2, col = "blue")
# averaging effect of the factor variable
abline(a = coef(m)[1] + mean(c(0, coef(m)[4])) + coef(m)[3] * mean(x2), b = coef(m)[2], lty = 1, lwd = 2)
legend("topleft", legend = c("Control", "N addition"), col = c("red", "blue"), pch = 16)

# now center the continuous variable to change interpretation of the intercept

data_center <- data.frame(x1 = x1 - mean(x1), x2 = x2 - mean(x2), x3 = x3)
modmat <- model.matrix(~x1 + x2 + x3, data = data.frame(x1=x1, x2=x2, x3=x3))
data_center$y_center <- rnorm(n = 100, mean = modmat %*% betas, sd = 1)

# second model
m_center <- lm(y_center ~ x1 + x2 + x3, data_center)
summary(m_center)

plot(y_center ~ x2, data_center, col = rep(c("red", "blue"), each = 50), pch = 16, 
     xlab = "Precipitation [mm]", ylab = "Biomass [mg]")
abline(a = coef(m_center)[1], b = coef(m_center)[3], lty = 2, lwd = 2, col = "red")
abline(a = coef(m_center)[1] + coef(m_center)[4], b = coef(m_center)[3], lty = 2, 
       lwd = 2, col = "blue")
# averaging effect of the factor variable
abline(a = coef(m_center)[1] + mean(c(0, coef(m_center)[4])), b = coef(m_center)[3], 
       lty = 1, lwd = 2)
legend("bottomright", legend = c("Control", "N addition"), col = c("red", "blue"), 
       pch = 16)

#########################################################################################
###############                         10                          #####################
###############            Composite image with ggplot2             #####################
#########################################################################################

# http://blog.mckuhn.de/2015/03/creating-composite-figures-with-ggplot.html
library(ggplot2)
library(gtable)
library(grid)

# create example data
set.seed(42)
dataset_names <- c("Human", "Mouse", "Fly", "Worm")
datasets <- data.frame(name = factor(dataset_names, levels = dataset_names), parity=factor(c(0,1,0,0)), v50 = runif(4, max=0.5), y=1:4)
data <- data.frame(dataset1 = rep(datasets$name, 4), dataset2 = rep(datasets$name, each=4), z=runif(16, min = 0, max = 0.5))
pal <- c("#dddddd", "#aaaaaa")

## set up individual plots

# heatmap
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$legend.position <- "none"
new_theme_empty$plot.margin <- structure(c(0, 0, 0, 0), unit = "lines", valid.unit = 3L, class = "unit")
new_theme_empty$axis.ticks <- element_blank()
new_theme_empty$axis.title.x <- element_blank()
new_theme_empty$axis.title.y <- element_blank()

p <- ggplot(data, aes(dataset1, dataset2, fill = z)) + geom_raster()
p <- p + coord_fixed()
p <- p + scale_fill_gradient2(midpoint = 0.5, mid="black", low = "#56B1F7", high="red", name="", breaks=c(0,0.25,0.5,0.75,1), limits=c(0,0.5))
p <- p + scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0))
plegend <- p + theme(legend.direction="horizontal")
pmiddle <- p + new_theme_empty

# top bar chart
theme_partly_empty <- theme_bw()
theme_partly_empty$rect <- element_blank()
theme_partly_empty$strip.text <- element_blank()
theme_partly_empty$axis.text.x <- element_blank()
theme_partly_empty$plot.title <- element_blank()
theme_partly_empty$legend.position <- "none"
theme_partly_empty$plot.margin <- structure(c(0, 0, 0, 0), unit = "lines", valid.unit = 3L, class = "unit")
theme_partly_empty$axis.ticks.x <- element_blank()
theme_partly_empty$axis.title.x <- element_blank()
theme_partly_empty$panel.grid.major.x <-element_blank()
theme_partly_empty$panel.grid.minor <-element_blank()

p <- ggplot( datasets, aes(name, v50, fill=parity)) + geom_bar(stat="identity")
p <- p + theme_partly_empty
p <- p + scale_fill_manual(values=pal)
p <- p + scale_y_continuous(expand=c(0,0), breaks=0.1*1:5) + scale_x_discrete(expand=c(0,0))
p <- p + ylab(expression(v[50]))
ptop <- p

# right bar chart
theme_partly_empty <- theme_bw()
theme_partly_empty$rect <- element_blank()
theme_partly_empty$strip.text <- element_blank()
theme_partly_empty$plot.title <- element_blank()
theme_partly_empty$legend.position <- "none"
theme_partly_empty$plot.margin <- structure(c(0, 0, 0, 0), unit = "lines", valid.unit = 3L, class = "unit")
theme_partly_empty$axis.text.y <- element_blank()
theme_partly_empty$axis.ticks.y <- element_blank()
theme_partly_empty$axis.title.y <- element_blank()
theme_partly_empty$panel.grid.major.y <-element_blank()
theme_partly_empty$panel.grid.minor <-element_blank()

p <- ggplot( datasets, aes(name, v50, fill=parity)) + geom_bar(stat="identity") + coord_flip()
p <- p + theme_partly_empty
p <- p + scale_fill_manual(values=pal)
p <- p + scale_y_continuous(expand=c(0,0), breaks=0.1*1:5) + scale_x_discrete(expand=c(0,0))
p <- p + ylab(expression(v[50]))
pright <- p

# left strip of labels
p <- ggplot( datasets, aes(xmin=0, xmax=1, ymin=y, ymax=y+1, fill=parity)) + geom_rect()
p <- p + new_theme_empty
p <- p + scale_fill_manual(values=pal)
p <- p + geom_text(aes(x=1, y=y+0.5, label=paste0(name, " ")), hjust=1, vjust=0.5)
p <- p + scale_y_discrete(expand = c(0,0)) + scale_x_continuous(expand=c(0,0))
pleft <- p

# bottom strip of labels
p <- ggplot( datasets, aes(xmin=y, xmax=y+1, ymin=0, ymax=1, fill=parity)) + geom_rect()
p <- p + new_theme_empty
p <- p + scale_fill_manual(values=pal)
p <- p + geom_text(aes(y=1, x=y+0.5, label=paste0(name, " ")), hjust=1, vjust=0.5, angle=90)
p <- p + scale_x_discrete(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
pbottom <- p

## create the layout, centered around the heatmap
g <- gtable_filter(ggplotGrob(pmiddle), pattern = "panel", trim = TRUE, fixed=TRUE)
g <- gtable_add_rows(g, unit(0.2, "null"), 0) # left labels
g <- gtable_add_rows(g, unit(0.2, "null"), 2) # right bar chart
g <- gtable_add_rows(g, unit(0.05, "null"), 2) # space for axis label 1/2
g <- gtable_add_rows(g, unit(0.05, "null"), 2) # space for axis label 2/2
g <- gtable_add_cols(g, unit(0.05, "null"), 0) # space for axis label 1/2
g <- gtable_add_cols(g, unit(0.05, "null"), 0) # space for axis label 2/2
g <- gtable_add_cols(g, unit(0.2, "null"), 0) # top bar chart
g <- gtable_add_cols(g, unit(0.2, "null"), 4) # bottom labels

# gtable_show_layout(g)

g <- gtable_add_grob(g, gtable_filter(ggplotGrob(ptop), pattern = "ylab", trim = TRUE, fixed=TRUE), 1, 2)
g <- gtable_add_grob(g, gtable_filter(ggplotGrob(ptop), pattern = "axis-l", trim = TRUE, fixed=TRUE), 1, 3)
g <- gtable_add_grob(g, gtable_filter(ggplotGrob(ptop), pattern = "panel", trim = TRUE, fixed=TRUE), 1, 4)

g <- gtable_add_grob(g, gtable_filter(ggplotGrob(pleft), pattern = "panel", trim = TRUE, fixed=TRUE), 2, 1, 2, 3)
g <- gtable_add_grob(g, gtable_filter(ggplotGrob(pbottom), pattern = "panel", trim = TRUE, fixed=TRUE), 3, 4, 5)

g <- gtable_add_grob(g, gtable_filter(ggplotGrob(pright), pattern = "panel", trim = TRUE, fixed=TRUE), 2, 5)
g <- gtable_add_grob(g, gtable_filter(ggplotGrob(pright), pattern = "axis-b", trim = TRUE, fixed=TRUE), 3, 5)
g <- gtable_add_grob(g, gtable_filter(ggplotGrob(pright), pattern = "xlab", trim = TRUE, fixed=TRUE), 4, 5)

g <- gtable_add_grob(g, gtable_filter(ggplotGrob(plegend), pattern = "guide-box", trim = TRUE, fixed=TRUE), 5, 1, 5, 3)
g <- gtable_add_grob(g, textGrob("\nMedian expression\ndistance of\n1:1 orthologs\n\n\n\n", gp = gpar(fontsize = 12)), 3, 1, 5, 3)


grid.newpage()
grid.draw(g)

png(paste0("test.png"), width = 10, height = 10, units = "in", res=300)
grid.draw(g)
dev.off()


#########################################################################################
###############                         11                          #####################
###############           VORONOI DIAGRAMS IN PLOTLY AND R          #####################
#########################################################################################

# http://moderndata.plot.ly/voronoi-diagrams-in-plotly-and-r/
# Here’s a function which uses plotly’s R Library to overlay a voronoi diagram on top of a 2-D K-Means visualization.
# Here’s a quick rundown on how this function works:
# 
# Accepts a data frame containing x and y coordinates of a bunch of data points
# The dataframe must have a cluster column specifying the k-means cluster to which point belongs to
# The function uses the deldir package to compute the Voronoi tesselation
# The function then uses some co-ordinate geometry to create the final visualization using plot_ly
# If interested, set the parameter print.ggplot to TRUE to see a diagnostic intermediary plot
# Try modifying the n.sd.x and n.sd.y parameters to adjust x-axis and y-axis limits

# FUNCTION DEFINITION
# Function Definition ---------------------------------------------------------
VoronoiPlotly <- function(fit,  # Fit object from K-Means
                          ds,  # Data frame containing original data and clusters from K-Means
                          n.sd.x = 3,  # Controls the width of the plot
                          n.sd.y = 3,  # Controls the height of the plot
                          print.ggplot = FALSE,  # Plots a diagnostic chart using ggplot2
                          point.opacity = 0.8,  
                          point.size = 7, 
                          point.symbol = "circle",
                          point.linewidth = 2,
                          point.lineopacity = 0.5,
                          plot_bgcolor = "#ffffff",
                          paper_bgcolor = "#ffffff",
                          center.size = 15,
                          shapes.opacity = 0.5,
                          shapes.linecolor = "#404040", 
                          center.color = "#000000"){
  
  # Options
  options(stringsAsFactors = F)
  graphics.off()
  
  # Load libraries ------------------------------------------------------------
  library(plotly)
  library(deldir)
  
  # Create convenience data frames ----------------------------------------------
  centers <- data.frame(fit$centers)
  vor <- deldir(centers)
  
  # Calculate slopes
  vor.df <- data.frame(vor$dirsgs, m = (vor$dirsgs$y2- vor$dirsgs$y1)/(vor$dirsgs$x2 - vor$dirsgs$x1))
  
  # Calculate constants
  vor.df$c <- with(vor.df, ((y2 - m*x2) + (y1 - m*x1))/2)
  
  # Covnert to strings for better matching later on
  vor.df.str <- data.frame(C1 = apply(vor.df[,1:2], 1, paste, collapse = ","),
                           C2 = apply(vor.df[,3:4], 1, paste, collapse = ","))
  
  # Combine the x and y coordinates for each segment
  coord.df <- rbind(as.matrix(vor.df[1:2]), as.matrix(vor.df[,3:4]))
  
  # Convert to string
  coord.df.str <- apply(coord.df, 1, paste, collapse = ",")
  
  # Find unique strings
  count <- sapply(coord.df.str, function(x){sum(coord.df.str == x)})
  coord.df.str <- data.frame(str = coord.df.str, count = count)
  coord.df.str <- subset(coord.df.str, count == 1)
  
  # Get outer boundary co-ordinates
  outer.bound <- matrix(as.numeric(unlist(strsplit(coord.df.str$str, ","))), ncol = 2, byrow = T)
  outer.bound <- data.frame(x = outer.bound[,1], y = outer.bound[,2])
  
  # Add respective slopes and constants
  for(i in 1:nrow(outer.bound)){
    str <- coord.df.str[i,1]
    idx <- ifelse(is.na(match(str, vor.df.str$C1)), match(str, vor.df.str$C2), match(str, vor.df.str$C1))
    
    # Slope
    outer.bound$m[i] <- vor.df$m[idx]
    
    # Constants
    outer.bound$c[i] <- vor.df$c[idx]
  }
  
  # Find enclosing rectangle boundaries -----------------------------------------
  x.min <- mean(ds$x) - n.sd.x*sd(ds$x)
  x.max <- mean(ds$x) + n.sd.x*sd(ds$x)
  y.min <- mean(ds$y) - n.sd.y*sd(ds$y)
  y.max <- mean(ds$y) + n.sd.y*sd(ds$y)
  
  # Create x-axsi and y-axis limits
  xlim <- c(x.min, x.max)
  ylim <- c(y.min, y.max)
  
  # Extend outer boundary points to above rectangle ------------------------------
  for(i in 1:nrow(outer.bound)){
    # Extract x-y coordinates
    x <- outer.bound$x[i]
    y <- outer.bound$y[i]
    
    # Get slope
    m <- outer.bound$m[i]
    
    # Get slope
    c <- outer.bound$c[i]
    
    # Extend to each edge of enclosing rectangle
    ext.coord <- mat.or.vec(4,3)
    
    # Extend to left edge
    ext.coord[1,1] <- x.min
    ext.coord[1,2] <- m*x.min + c
    ext.coord[1,3] <- sqrt((ext.coord[1,1] - x)^2 + (ext.coord[1,2] - y)^2)
    
    # Extend to right edge
    ext.coord[2,1] <- x.max
    ext.coord[2,2] <- m*x.max + c
    ext.coord[2,3] <- sqrt((ext.coord[2,1] - x)^2 + (ext.coord[2,2] - y)^2)
    
    # Extend to top edge
    ext.coord[3,2] <- y.max
    ext.coord[3,1] <- (y.max - c)/m
    ext.coord[3,3] <- sqrt((ext.coord[3,1] - x)^2 + (ext.coord[3,2] - y)^2)
    
    # Extend to bottom edge
    ext.coord[4,2] <- y.min
    ext.coord[4,1] <- (y.min - c)/m
    ext.coord[4,3] <- sqrt((ext.coord[4,1] - x)^2 + (ext.coord[4,2] - y)^2)
    
    # Find the closest edge
    idx <- which.min(ext.coord[,3])
    
    x <- ext.coord[idx,1]
    y <- ext.coord[idx,2]
    
    # Insert into outer bound 
    outer.bound$x.ext[i] <- x
    outer.bound$y.ext[i] <- y
  }
  
  # Convert to string for easier searcing later on
  outer.bound.str <- apply(outer.bound[,5:6], 1, paste, collapse = ",")
  
  # Augment vor.df with extended outer bound coordinates -------------------------
  for(i in 1:nrow(outer.bound)){
    # Convert to string to help matching
    str <- paste(outer.bound[i,1:2], collapse = ",")
    
    # Match with original vor.df
    if(is.na(match(str, vor.df.str$C1))){
      idx <- match(str, vor.df.str$C2)
      vor.df[idx, 3:4] <- outer.bound[i, 5:6]
    }else{
      idx <- match(str, vor.df.str$C1)
      vor.df[idx, 1:2] <- outer.bound[i, 5:6]
    }
  }
  
  # Plot Check ------------------------------------------------------------------
  p.ggplot <- ggplot() +
    geom_point(data = centers, aes(x, y), color= "red", size = 5) +
    geom_point(data = ds, aes(x, y, color = cluster)) +
    geom_segment(data = vor.df, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_point(data = as.data.frame(fit$centers), aes(x, y)) +
    geom_text(data = centers, aes(x,y, label = 1:nrow(centers)), size = 10) +
    geom_point(data = outer.bound, aes(x.ext, y.ext), color = "blue", size = 5) + 
    geom_point(data = outer.bound, aes(x, y), color = "red", size = 5) + 
    geom_hline(yintercept = y.min) + 
    geom_hline(yintercept = y.max) + 
    geom_vline(xintercept = x.min) +
    geom_vline(xintercept = x.max)
  p.ggplot <- ggplotly(p.ggplot)
  if(print.ggplot == T){print(p.ggplot)}
  # -----------------------------------------------------------------------------
  
  # Function to calculate which side of line is point on ------------------------
  sideFUNC <- function(x, y, x1, y1, x2, y2){
    d <- (x - x1)*(y2-y1) - (y - y1)*(x2 - x1)
    
    return(round(d,2))
  }
  
  # Figure out the path for each polygon ----------------------------------------
  path <- list()
  
  # Loop thorough each centroid and find corrosponding edges
  for(i in 1:nrow(centers)){
    # Find each row where centeroid is available
    mat <- subset(vor.df, ind1 == i | ind2 == i)
    
    # Find all unique coordinates associated with centroid
    mat <- cbind(matrix(c(mat$x1, mat$x2), ncol = 1), matrix(c(mat$y1, mat$y2), ncol = 1))
    mat <- unique(mat)
    mat.str <- apply(mat, 1, paste, collapse = ",")
    
    # print(mat)
    
    # Find all outer boundary points asociated with centroid
    # If an outer boundary point is found, there must be atleast two
    idx <- outer.bound.str %in% mat.str
    if(sum(idx) == 2){
      # Only if two outer boundary points are found
      # then need to modify matrix and add edge end points
      
      # Find the side where all other outer boundary points are
      # Assuming all other boundary points are on the same side
      # need only one point to find this out
      p <- as.numeric(unlist(strsplit(outer.bound.str[!idx][1], split = ",")))
      
      # Line segment is defined by the two identified outer boundary points 
      p1 <- as.numeric(unlist(strsplit(outer.bound.str[idx][1], split = ",")))
      p2 <- as.numeric(unlist(strsplit(outer.bound.str[idx][2], split = ",")))
      
      # Find side
      side <- sideFUNC(p[1], p[2], p1[1], p1[2], p2[1], p2[2])
      
      # Case when only two cluster and hence only one dividing segment
      if(is.na(side)){
        side <- sideFUNC(centers[i,1], centers[i,2], p1[1], p1[2], p2[1], p2[2])
      }
      
      if(side != 0){
        
        # Find the enclosing rectangle"s endpoints that are on the opposite side
        # Top - Left
        side.check <- sideFUNC(x.min, y.max, p1[1], p1[2], p2[1], p2[2])
        if(side.check != 0){if(sign(side.check) != sign(side)) {mat <- rbind(mat, c(x.min, y.max))}}
        
        # Bottom - Left
        side.check <- sideFUNC(x.min, y.min, p1[1], p1[2], p2[1], p2[2])
        if(side.check != 0){if(sign(side.check) != sign(side)) {mat <- rbind(mat, c(x.min, y.min))}}
        
        # Top - Right
        side.check <- sideFUNC(x.max, y.max, p1[1], p1[2], p2[1], p2[2])
        if(side.check != 0){if(sign(side.check) != sign(side)) {mat <- rbind(mat, c(x.max, y.max))}}
        
        # Bottom - Right
        side.check <- sideFUNC(x.max, y.min, p1[1], p1[2], p2[1], p2[2])
        if(side.check != 0){if(sign(side.check) != sign(side)) {mat <- rbind(mat, c(x.max, y.min))}}
      }
    }
    
    # print(mat)
    # readline("Enter:")
    
    # Re-order the points to ensure it makes a convex polygon
    mat <- mat[chull(mat),]
    
    #Paste together
    path[[i]] <- paste0("M", paste0(mat[1,], collapse = ","))
    
    path[[i]] <- paste(path[[i]],
                       paste(apply(matrix(mat[-1,], ncol = 2), 1, function(x){
                         vec <- paste0(x, collapse = ",")
                         vec <- paste0("L", vec)
                       }), collapse = " "),
                       "Z")
  }
  
  # Finally plot using Plotly ---------------------------------------------------
  # crate a "shapes" list for voronoi polygons to be passed to layout()
  shapes <- list()
  cols <- RColorBrewer::brewer.pal(nrow(centers), "Paired")
  
  # Loop through each path and add params like fill color, opacity etc
  for(i in 1:length(path)){
    shapes[[i]] <- list(type = "path",
                        path = path[[i]],
                        fillcolor = cols[i],
                        opacity = shapes.opacity,
                        line = list(color = shapes.linecolor))
  }
  
  # Change colors for each cluster to allow manual spec
  for(i in 1:nrow(centers)){
    ds$color[ds$cluster == i] <- cols[i]
  }
  
  # Create plot
  # base layer
  p <- plot_ly(ds, x = x, y = y , mode = "markers", name = "Clusters", opacity = point.opacity, 
               hoverinfo = "x+y+text",
               text = paste("Cluster:",cluster),
               marker = list(symbol = point.symbol, color = color, size = point.size, 
                             line = list(color = "#262626", width = point.linewidth, opacity = point.lineopacity)),
               showlegend = F)
  
  # Add centroids
  p <- add_trace(centers, x = x, y = y, mode = "markers", name = "Cluster Centers",
                 hoverinfo = "none",
                 marker = list(color = center.color, symbol = "cross", size = center.size))
  
  # Add polygons
  p <- layout(title = "Voronoi polygons and K- Means clustering",
              paper_bgcolor = paper_bgcolor,
              plot_bgcolor = plot_bgcolor,
              xaxis = list(range = xlim, zeroline = F),
              yaxis = list(range = ylim, zeroline = F),
              shapes = shapes)
  
  print(p)
}

# FUNCTION CALL

set.seed(12345)
nClust <- 12 # number of clusters
nPoints <- 2000 # number of data points
ds <- data.frame(x = rnorm(nPoints), y = rnorm(nPoints))
# Do K-Means Clustering
fit <- kmeans(ds, centers = nClust)
ds <- cbind(ds, cluster = as.factor(fit$cluster))
# Call function and print to internal viewer.
VoronoiPlotly(fit, ds, n.sd.x = 3, n.sd.y = 3, print.ggplot = F)


#########################################################################################
###############                         12                          #####################
###############           My Commonly Done ggplot2 graphs           #####################
#########################################################################################

# https://hopstat.wordpress.com/2014/12/18/my-commonly-done-ggplot2-graphs-part-2/

library(ggplot2)

# Data preparation
set.seed(20141106)
data = data.frame(x = rnorm(1000, mean = 6), batch = factor(rbinom(1000, size = 4, prob = 0.5)))
data$group1 = 1- rbeta(1000, 10, 2)
mat = model.matrix(~ batch, data=data)
mat = mat[, !colnames(mat) %in% "(Intercept)"]
betas = rbinom(ncol(mat), size = 20, prob = 0.5)
data$quality = rowSums(t(t(mat) * sample(-2:2)))
data$dec.quality = cut(data$quality, breaks = unique(quantile(data$quality, probs = seq(0, 1, by=0.1))), include.lowest = TRUE)
batch.effect = t(t(mat) * betas)
batch.effect = rowSums(batch.effect)
data$y = data$x * 5 + rnorm(1000) + batch.effect + data$quality * rnorm(1000, sd = 2)
data$group2 = runif(1000)

# I have added 2 important new variables, quality and batch. The motivation for these variables is akin to an RNAseq analysis set where you have a quality measure like read depth, and where the data were processed in different batches. The y variable is based both on the batch effect and the quality.

g = ggplot(data, aes(x = x, y = y)) + geom_point()
print(g)

# Coloring by a 3rd Variable (Discrete)
print({g + aes(colour=batch)})

# Coloring by a 3rd Variable (Continuous)
print({gcol = g + aes(colour=quality)})
# Let's change the gradient of low to high values using scale_colour_gradient:
print({gcol + scale_colour_gradient(low = "red", high = "blue")})
# This isn't much better. Let's call the middle quality gray and see if we can see better separation:
print({gcol_grad = gcol + scale_colour_gradient2(low = "red", mid = "gray", high = "blue")})

# Scatterplot with Coloring by a 3rd Variable (Continuous broken into Discrete)
print({gcol_dec = g + aes(colour = dec.quality)})

# Scatterplot with Coloring by 3rd Continuous Variable Faceted by a 4th Discrete Variable
print({gcol_grad + facet_wrap(~ batch)})



#########################################################################################
###############                         13                          #####################
###############           Scatter plot with special circles         #####################
#########################################################################################

# http://www.r-bloggers.com/the-world-we-live-in-4-marriage-ages/
# https://aschinchon.wordpress.com/2015/03/16/the-world-we-live-in-4-marriage-ages/
#Singulate mean age at marriage: http://data.un.org/Data.aspx?d=GenderStat&f=inID%3a20
#Population: http://data.un.org/Data.aspx?d=SOWC&f=inID%3a105
require("sqldf")
require("ggplot2")
mar=read.csv("Marriage.csv", nrows = 321, header = TRUE, row.names=NULL)
pop=read.csv("Population.csv", nrows = 999, header = TRUE, row.names = NULL)
colnames(mar)[1]="Country"
colnames(pop)[1]="Country"
data=sqldf("SELECT
  a.Country,
  a.Value as Pop,
  b.Value as Female,
  c.Value as Male
FROM
  pop a INNER JOIN mar b
  ON (a.Country=b.Country AND b.Subgroup='Female') INNER JOIN mar c
  ON (a.Country=c.Country AND c.Subgroup='Male')
WHERE a.Subgroup = 'Total'")
opts= theme(
  panel.background = element_rect(fill = "gray98"),
  panel.border = element_rect(colour = "black", fill = NA),
  axis.line = element_line(size = 0.5, colour = "black"),
  axis.ticks = element_line(colour = "black"),
  panel.grid.major = element_line(colour = "gray75", linetype = 2),
  panel.grid.minor = element_blank(),
  axis.text = element_text(colour = "gray25", size = 15),
  axis.title = element_text(size = 18, colour = "gray10"),
  legend.key = element_blank(),
  legend.position = "none",
  legend.background = element_blank(),
  plot.title = element_text(size = 40, colour = "gray10"))
ggplot(data, aes(x=Female, y=Male, size=log(Pop), label=Country), guide=FALSE)+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55)+
  scale_size_continuous(range=c(2,36))+
  scale_x_continuous(limits=c(16,36), breaks=seq(16, 36, by = 2), expand = c(0, 0))+
  scale_y_continuous(limits=c(16,36), breaks=seq(16, 36, by = 2), expand = c(0, 0))+
  geom_abline(intercept = 0, slope = 1, colour = "gray10", linetype=2)+
  labs(title="The World We Live In #4: Marriage Ages",
       x="Females mean age at marriage",
       y="Males mean age at marriage")+
  geom_text(data=subset(data, abs(Female-Male)>7), size=5.5, colour="gray25", hjust=0, vjust=0)+
  geom_text(data=subset(data, Female>=32|Female<=18), size=5.5, colour="gray25", hjust=0, vjust=0)+
  geom_text(aes(24, 17), colour="gray25", hjust=0, label="Source: United Nations (size of bubble depending on population)", size=5)+opts



#########################################################################################
###############                         14                          #####################
###############      Exploratory data analysis with ggplot2         #####################
#########################################################################################

# http://theanalyticalminds.blogspot.com.tr/2015/03/part-3a-plotting-with-ggplot2.html
# http://theanalyticalminds.blogspot.com.tr/2015/03/part-3b-eda-with-ggplot2.html
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

# EXPLORATORY DATA ANALYSIS
library(ggplot2)
# Exploring the dependent variable - daily rain amount
# Time series of the daily rain amount, with smoother curve
ggplot(weather, aes(date, rain)) +
  geom_point(aes(colour = rain)) +
  geom_smooth(colour = "blue", size = 1) +
  scale_colour_gradient2(low = "green", mid = "orange", high = "red", midpoint = 20) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Date") +
  ylab("Rain (mm)") +
  ggtitle("Daily rain amount")

# Histogram of the daily rain amount
ggplot(weather, aes(rain)) + 
  geom_histogram(binwidth = 1, colour = "blue", fill = "darkgrey") +
  scale_x_continuous(breaks = seq(0, 80, 5)) +
  scale_y_continuous(breaks = seq(0,225,25)) +
  xlab("Rain (mm)") +
  ylab("Frequency (days)") +
  ggtitle("Daily rain amount distribution")

# Rain amount (continuous) by season
# Jitter plot - Rain amount by season 

ggplot(weather, aes(season, rain)) +
  geom_jitter(aes(colour=rain), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red", high = "black", midpoint = 30) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Season") +
  ylab("Rain (mm)") +
  ggtitle("Daily rain amount by season")

# Amount of rain vs. wind, by season
ggplot(weather, aes(gust.wind, rain)) +
  geom_point(colour = "firebrick") +
  geom_smooth(size = 0.75, se = FALSE) +
  facet_wrap(~season) +
  xlab("Maximum wind speed (km/h)") +
  ylab("Rain (mm)") +
  ggtitle("Amount of rain vs. maximum wind speed, by season")
