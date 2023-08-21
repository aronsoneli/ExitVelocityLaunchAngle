yakker <- read.csv("~/Downloads/yakker23 (4).csv")
# This reads the main Yakkertech file with each event from each game
library(dplyr)
library(ggplot2)

#Select Boomers data only
boomers <- yakker %>%
  filter(BatterTeam == "Schaumburg Boomers")
boomers <- boomers %>%
  filter(PlayResult %in% c("Single", "Double", "Triple",
                            "HomeRun")) %>%
  select(Batter, ExitSpeed, Angle, Distance, PlayResult, Date)
#delete NA's
boomers <- na.omit(boomers)

# Turn Sac's into out
boomers$PlayResult <- ifelse(boomers$PlayResult == "Sacrifice", "Out", boomers$PlayResult)

# Select only hits over 50 mph exit velocity
boomers <- boomers[boomers$ExitSpeed >= 50, ]

#install.packages("svg")
library(png)
install.packages("gridExtra")
#library(magick)
library(gridExtra)


#image_path_modified <- "hitter2.png"  # Replace with the actual path to your modified image
#image <- readJPEG("~/Downloads/hitter3.jpeg")

# Create a b
lines_df <- data.frame(
  angle_degrees = seq(0, 350, by = 10),
  angle_radians = seq(0, 350, by = 10) * pi / 180,
  x_start = rep(25, 36),
  y_start = rep(0, 36),
  x_end = 25 + 100 * cos(seq(0, 350, by = 10) * pi / 180),
  y_end = 100 * sin(seq(0, 350, by = 10) * pi / 180)
)

# Create a plot with multiple lines
# line_plot <- ggplot(lines_df, aes(x = x_start, y = y_start, xend = x_end, yend = y_end)) +
#   geom_segment(color = "gray", alpha = 0.5) +
#   coord_cartesian(xlim = c(0, 120), ylim = c(-25, 70)) +
#   theme(panel.background = element_rect(fill = "transparent"))

# Launch Angle and Exit Velocity Graph without lines
p <- ggplot(boomers, aes(x = ExitSpeed, y = Angle, color = PlayResult)) +
  geom_point(size = 2, alpha = .7) +
  scale_color_manual(values = c("Out" = "black", "Single" = "orange", 
                                "Double" = "blue", "Triple" = "green", "HomeRun" = "pink"),
                     breaks = c("Single", "Double", "Triple", "HomeRun")) +
  labs(x = "Exit Velocity", y = "Launch Angle", title = "Schaumburg Boomers Exit Velocity v. Launch Angle") +
  coord_cartesian(xlim = c(35, 120), ylim = c(-25, 55)) +
  theme_minimal() +  # Use a minimal theme with no background gridlines
  theme(
    panel.background = element_rect(fill = "white"),  # Set a white background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )
#p
  #geom_raster(data = data.frame(x = 40, y = 10), aes(x = x, y = y, image = image_modified), interpolate = TRUE)

# Create a data frame for the lines
lines_df <- data.frame(
  angle_degrees = seq(0, 350, by = 10),
  angle_radians = seq(0, 350, by = 10) * pi / 180,
  x_start = rep(-5, 36),
  y_start = rep(0, 36),
  x_end = 25 + 100 * cos(seq(0, 350, by = 10) * pi / 180),
  y_end = 100 * sin(seq(0, 350, by = 10) * pi / 180)
)

# Add the lines to the scatterplot using geom_segment
p <- p + geom_segment(data = lines_df, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                 color = "gray", alpha = 0.5)
image_path <- "~/Downloads/Boomers.png"
image <- readPNG(image_path)
install.packages("grid")
library(grid)
# Create a graphical object for the image
image_grob <- rasterGrob(image, width = unit(1, "in"), height = unit(.5, "in"))

# Combine the scatterplot and image using annotation_custom
combined_plot <- p +
  annotation_custom(image_grob, xmin = 100, xmax = 125, ymin = 45, ymax = 60)

# Print the finished plot of exit velocity and launch
print(combined_plot)

#distance
# ggplot(boomers, aes(x = ExitSpeed, y = Distance, color = PlayResult)) +
#   geom_point() +
#   scale_color_manual(values = c("Out" = "black", "Single" = "orange", 
#                                 "Double" = "blue", "Triple" = "green", "HomeRun" = "pink")) +
#   labs(x = "Exit Speed", y = "Distance", title = "Schaumburg Boomers Exit Velocity vs. Projected Distance") +
#   coord_cartesian(xlim = c(30, 105), ylim = c(0, 450))
