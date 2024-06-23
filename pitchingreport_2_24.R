library(tidyverse)
library(gt)
library(cowplot)
library(patchwork)
library(ggplot2)
library(ggforce)
library(dplyr)
library(gridExtra)
library(GeomMLBStadiums)
library(qpdf)

setwd("/Users/Mason/Desktop/R project")

local_image_path <- "/Users/Mason/Desktop/R project/stjohns.png"

#local_image_path <- "/Users/Mason/Desktop/R project/orleanslogo.jpeg"

# Import the data
df <- read.csv("/Users/Mason/Downloads/St. John's Season 2024 - Combined.csv")

pitcher_data <- df %>% 
  filter(Pitcher == "Cunningham, Tim") %>%
  mutate(TaggedPitchType = ifelse(TaggedPitchType == "Sinker", "Fastball", TaggedPitchType)) %>%
  filter(TaggedPitchType != "Undefined")

output_pdf <- "Cunningham_2024_season.pdf"

pitcher_data <- pitcher_data %>%
  filter(!TaggedPitchType %in% c("0", "1", "2", "Other"))

# Get pitcher's name from the second row
pitcher_name <- pitcher_data$Pitcher[2]

# Get the date from the second row
date <- pitcher_data$Date[2]

no_reads_count <- sum(is.na(pitcher_data$RelSpeed))

# Calculate first pitch strike percentage
first_pitch_strikes <- pitcher_data %>%
  filter(PitchofPA == 1 & PitchCall %in% c("FoulBall", "InPlay", "StrikeCalled", "StrikeSwinging"))

first_pitch_strikes_count <- nrow(first_pitch_strikes)

total_first_pitches <- pitcher_data %>%
  filter(PitchofPA == 1)

total_first_pitches_count <- nrow(total_first_pitches)

first_pitch_strike_percentage <- round(first_pitch_strikes_count / total_first_pitches_count * 100, 1)

pitcherreporttable1 <- pitcher_data %>%
  summarise(
    H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
    BB = sum(KorBB == "Walk"),
    K = sum(KorBB == "Strikeout"),
    "1PS%" = first_pitch_strike_percentage,
    "No reads" = no_reads_count
  )

# Create a gt table
gt_table <- pitcherreporttable1 %>%
  gt() %>%
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#BA0C2F"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "#ffffff"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",  # You can change to "top", "bottom", "left", "right" as needed
      color = "lightgray",
      weight = 0.5
    ),
    locations = cells_body(columns = everything())
  ) %>%
  cols_width(
    "H" ~ px(50),
    "BB" ~ px(50),
    "K" ~ px(50),
    "1PS%" ~ px(75),
    "No reads" ~ px(85))

# Save the 1st table as an image
gt_table %>%
  gtsave("pitcher_report_table1.png", path = "~/Desktop/R project")

# Calculate the averages, maximums, and percentages per TaggedPitchType
# Include ZSwingPercentage
pitcherreporttable2 <- pitcher_data %>% 
  group_by(TaggedPitchType) %>% 
  summarise(AverageRelSpeed = round(mean(RelSpeed, na.rm = TRUE), 1),
            MaximumRelSpeed = round(max(RelSpeed, na.rm = TRUE), 1),
            AverageSpinRate = round(mean(SpinRate, na.rm = TRUE)), 
            MaximumSpinRate = round(max(SpinRate, na.rm = TRUE)),
            AverageExitSpeed = round(mean(ExitSpeed, na.rm = TRUE), 1),
            AverageInducedVertBreak = round(mean(InducedVertBreak, na.rm = TRUE), 1),
            AverageHorzBreak = round(mean(HorzBreak, na.rm = TRUE), 1),
            AverageExtension = round(mean(Extension, na.rm = TRUE), 2),
            AverageVertApprAngle = round(mean(VertApprAngle, na.rm = TRUE), 2), # Added line
            AverageRelHeight = round(mean(RelHeight, na.rm = TRUE), 2),
            AverageRelSide = round(mean(RelSide, na.rm = TRUE), 2),
            Count = n()
  ) %>%
  rename(AvgVelo = AverageRelSpeed,
         MaxVelo = MaximumRelSpeed,
         AvgSpin = AverageSpinRate,
         MaxSpin = MaximumSpinRate,
         AvgEV = AverageExitSpeed,
         Pitch = TaggedPitchType,
         IVB = AverageInducedVertBreak,
         HB = AverageHorzBreak,
         Ext = AverageExtension,
         VAA = AverageVertApprAngle,
         RelZ = AverageRelHeight,
         RelX = AverageRelSide)

totals <- data.frame(
  Pitch = "Total",
  AvgVelo = round(mean(pitcher_data$RelSpeed, na.rm = TRUE), 1),
  MaxVelo = round(max(pitcher_data$RelSpeed, na.rm = TRUE), 1),
  AvgSpin = round(mean(pitcher_data$SpinRate, na.rm = TRUE)),
  MaxSpin = round(max(pitcher_data$SpinRate, na.rm = TRUE)),
  AvgEV = round(mean(pitcher_data$ExitSpeed, na.rm = TRUE), 1),
  IVB = round(mean(pitcher_data$InducedVertBreak, na.rm = TRUE), 1),
  HB = round(mean(pitcher_data$HorzBreak, na.rm = TRUE), 1),
  Ext = round(mean(pitcher_data$Extension, na.rm = TRUE), 2),
  VAA = round(mean(pitcher_data$VertApprAngle, na.rm = TRUE), 2), # Added line
  RelZ = round(mean(pitcher_data$RelHeight, na.rm = TRUE), 2),
  RelX = round(mean(pitcher_data$RelSide, na.rm = TRUE), 2),
  Count = nrow(pitcher_data)
)

pitcherreporttable2 <- rbind(pitcherreporttable2, totals)

# The column order
col_order <- c("Pitch", "Count","AvgVelo", "MaxVelo","AvgSpin", "MaxSpin", "IVB", "HB", "RelZ", 
               "RelX", "Ext","VAA", "AvgEV")

# Get the table
table <- gt(pitcherreporttable2) %>%
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#BA0C2F"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "#ffffff"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      rows = nrow(pitcherreporttable2),
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",  # You can change to "top", "bottom", "left", "right" as needed
      color = "lightgray",
      weight = 0.5
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_header(
    title = md(paste0("<div style='display: flex; align-items: center; justify-content: center;'><img src='", local_image_path, "' style='height:35px; margin-right: 10px;'> <div style='text-align: center;'>", pitcher_name, "</div></div>")),
    subtitle = md(paste0("Trackman report Spring 2024"))
    #subtitle = md(paste0("Trackman season report"))
  ) %>%
  cols_width(
    "Pitch" ~ px(100),
    "Count" ~ px(50),
    "AvgVelo" ~ px(75),
    "MaxVelo" ~ px(75),
    "AvgSpin" ~ px(75),
    "MaxSpin" ~ px(75),
    "IVB" ~ px(75),
    "HB" ~ px(75),
    "RelZ" ~ px(75),
    "RelX" ~ px(75),
    "Ext" ~ px(75),
    "VAA" ~ px(75),
    "AvgEV" ~ px(75)
  ) %>%
  cols_move_to_start(columns = col_order) 

# Save the 2nd table as an image
table_image_path <- "~/Desktop/R project/pitcher_report_table2.png"
table %>%
  gtsave(table_image_path)

pitcher_data3 <- pitcher_data %>% 
  mutate(
    Strike = ifelse(PitchCall %in% c("FoulBall", "StrikeSwinging", "StrikeCalled", "InPlay"), 1, 0),
    Whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0),
    Swing = ifelse(PitchCall %in% c("FoulBall", "StrikeSwinging", "InPlay"), 1, 0),
    GroundBall = ifelse(TaggedHitType == "GroundBall", 1, 0),
    TotalBallsInPlay = ifelse(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive"), 1, 0),
    CSW = ifelse(PitchCall %in% c("StrikeSwinging", "StrikeCalled"), 1, 0),  
    SpinRate = SpinRate, 
    InStrikeZone = ifelse(PlateLocSide >= -1.30 & PlateLocSide <= 1.30 & PlateLocHeight >= 1.20 & PlateLocHeight <= 3.2, 1, 0),
    ZSwing = ifelse(InStrikeZone == 1 & Swing == 1, 1, 0),
    Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0)
  ) %>%
  select(TaggedPitchType, RelSpeed, ExitSpeed, InducedVertBreak, HorzBreak, Extension, RelHeight, RelSide, Swing, Whiff, Strike, GroundBall, TotalBallsInPlay, SpinRate, CSW, InStrikeZone, ZSwing, Chase)

print(head(pitcher_data3))

# Calculate the averages, maximums, and percentages per TaggedPitchType
# Include ZSwingPercentage
pitcherreporttable3 <- pitcher_data3 %>% 
  group_by(TaggedPitchType) %>% 
  summarise(SwingPercentage = round(mean(Swing)*100, 1),
            WhiffPercentage = round(sum(Whiff) / sum(Swing, na.rm = TRUE)*100, 1),
            StrikePercentage = round(mean(Strike)*100, 1),
            CSWPercentage = round(sum(CSW) / n() * 100, 1),  
            GroundBallPercentage = round(sum(GroundBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE)*100, 1),
            StrikeZonePercentage = round(sum(InStrikeZone, na.rm = TRUE) / n() * 100, 1),
            ZSwingPercentage = round(sum(ZSwing, na.rm = TRUE) / sum(InStrikeZone, na.rm = TRUE)*100, 1),
            Percentage = round((n()/nrow(pitcher_data3))*100, 1),
            ChasePercentage = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE)*100, 1)) %>%
  rename(Pitch = TaggedPitchType)

totals <- data.frame(
  Pitch = "Total",
  SwingPercentage = round(mean(pitcher_data3$Swing, na.rm = TRUE) * 100, 1),
  WhiffPercentage = round(sum(pitcher_data3$Whiff, na.rm = TRUE) / sum(pitcher_data3$Swing, na.rm = TRUE) * 100, 1),
  StrikePercentage = round(mean(pitcher_data3$Strike, na.rm = TRUE) * 100, 1),
  CSWPercentage = round(sum(pitcher_data3$CSW) / nrow(pitcher_data3) * 100, 1),
  GroundBallPercentage = round(sum(pitcher_data3$GroundBall, na.rm = TRUE) / sum(pitcher_data3$TotalBallsInPlay, na.rm = TRUE) * 100, 1),
  StrikeZonePercentage = round(sum(pitcher_data3$InStrikeZone, na.rm = TRUE) / nrow(pitcher_data3) * 100, 1),
  ChasePercentage = round(sum(pitcher_data3$Chase, na.rm = TRUE) / sum(pitcher_data3$Swing, na.rm = TRUE) * 100, 1),
  ZSwingPercentage = round(sum(pitcher_data3$ZSwing, na.rm = TRUE) / sum(pitcher_data3$InStrikeZone, na.rm = TRUE) * 100, 1),
  Percentage = 100.0
)


pitcherreporttable3 <- rbind(pitcherreporttable3, totals)

# The column order
col_order <- c("Pitch", "Percentage", "StrikePercentage", "SwingPercentage", 
               "WhiffPercentage", "CSWPercentage", "GroundBallPercentage", "StrikeZonePercentage", "ZSwingPercentage", "ChasePercentage")

# Get the table
table <- gt(pitcherreporttable3) %>%
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(
      align = "center"
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#BA0C2F"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "#ffffff"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      rows = nrow(pitcherreporttable3),
      columns = everything()
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",  # You can change to "top", "bottom", "left", "right" as needed
      color = "lightgray",
      weight = 0.5
    ),
    locations = cells_body(columns = everything())
  ) %>%
  cols_label(
    SwingPercentage = "Swing%", 
    WhiffPercentage = "Whiff%",
    StrikePercentage = "Strike%",
    Percentage = "Usage%",
    GroundBallPercentage = "GB%",
    StrikeZonePercentage = "Zone%",
    ZSwingPercentage = "Z-Swing%",
    ChasePercentage = "Chase%",
    CSWPercentage = "CSW%"
  ) %>%
  cols_width(
    "Pitch" ~ px(100),
    "Percentage" ~ px(75),
    "StrikePercentage" ~ px(75),
    "SwingPercentage" ~ px(75),
    "WhiffPercentage" ~ px(75),
    "CSWPercentage" ~ px(75),
    "GroundBallPercentage" ~ px(75),
    "StrikeZonePercentage" ~ px(75),
    "ZSwingPercentage" ~ px(90),
    "ChasePercentage" ~ px(75)
  ) %>%
  cols_move_to_start(columns = col_order) 

# Save the 3rd table as an image
table %>%
  gtsave("pitcher_report_table3.png", path = "~/Desktop/R project")

setwd("/Users/Mason/Desktop/R project")

# Load the saved images
p1 <- ggdraw() + draw_image("pitcher_report_table1.png", scale = 0.25)
p2 <- ggdraw() + draw_image("pitcher_report_table2.png", scale = 1)
p3 <- ggdraw() + draw_image("pitcher_report_table3.png", scale = .8)

# Combine the images into a single plot
combined_plot <- plot_grid(p2 / p3 / p1, rel_heights = c(1, 0.8, 0))

# Print the combined plot
print(combined_plot)

ggsave("/Users/Mason/Desktop/rimages/combinedtable.pdf", combined_plot, width = 8.5, height = 11)

pitch_colors <- c('FourSeamFastBall' = rgb(210/255, 45/255, 73/255, alpha = .5),
                  'One-seam Fastball' = rgb(210/255, 45/255, 73/255, alpha = .5),
                  'Fastball' = rgb(210/255, 45/255, 73/255, alpha = .5),
                  'TwoSeamFastBall' = rgb(222/255, 106/255, 4/255, alpha = .5),
                  'Sinker' = rgb(254/255, 157/255, 0/255, alpha = .5),
                  'Cutter' = rgb(147/255, 63/255, 44/255, alpha = .5),
                  'Slider' = rgb(238/255, 231/255, 22/255, alpha = .5),
                  'Splitter' = rgb(136/255, 136/255, 136/255, alpha = .5),
                  'ChangeUp' = rgb(29/255, 190/255, 58/255, alpha = .5),
                  'Sweeper' = rgb(221/255, 179/255, 58/255, alpha = .5),
                  'Knuckle Curve' = rgb(98/255, 54/255, 205/255, alpha = .5),
                  'Curveball' = rgb(0/255, 209/255, 237/255, alpha = .5),
                  'Slurve' = rgb(147/255, 175/255, 212/255, alpha = .5),
                  'Screwball' = rgb(96/255, 219/255, 51/255, alpha = .5),
                  'Forkball' = rgb(85/255, 204/255, 171/255, alpha = .5),
                  'Eephus' = rgb(136/255, 136/255, 136/255, alpha = .5),
                  'Knuckleball' = rgb(60/255, 82/255, 205/255, alpha = .5),
                  'Other' = rgb(136/255, 136/255, 136/255, alpha = .5))

pitcher_data$PitchCount <- seq_along(pitcher_data$TaggedPitchType)

fastballs <- pitcher_data %>%
  filter(TaggedPitchType %in% c("Fastball", "Four-seam Fastball", "Two-seam Fastball", "One-seam Fastball", "Sinker"))

ofspd <- pitcher_data %>%
  filter(TaggedPitchType %in% c("ChangeUp", "Splitter", "Cutter", "Knuckleball"))

brkball <- pitcher_data %>%
  filter(TaggedPitchType %in% c("Sweeper", "Slider", "Curveball", "Slurve"))

lhhwhiffs <- pitcher_data %>% 
  filter(BatterSide == "Left", PitchCall == "StrikeSwinging")

rhhwhiffs <- pitcher_data %>% 
  filter(BatterSide == "Right", PitchCall == "StrikeSwinging")

lhhswings <- pitcher_data %>% 
  filter(BatterSide == "Left", PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))

rhhswings <- pitcher_data %>% 
  filter(BatterSide == "Right", PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))

lhhhits <- pitcher_data %>% 
  filter(BatterSide == "Left", PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))

rhhhits <- pitcher_data %>% 
  filter(BatterSide == "Right", PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))

convert_time_to_degrees <- function(time_string) {
  time_parts <- strsplit(time_string, ":")[[1]]
  hour <- as.numeric(time_parts[1])
  minute <- as.numeric(time_parts[2])
  # Convert the time to degrees, with 12:00 as 0/360 degrees and increase clockwise
  hour_degrees <- ((3 - hour) %% 12) * 30
  minute_degrees <- minute * 0.5
  degrees <- hour_degrees - minute_degrees
  return(degrees)
}

# Convert tilt to degrees
pitcher_data$TiltDegrees <- sapply(pitcher_data$Tilt, convert_time_to_degrees)

plot1 <- ggplot(data = pitcher_data, aes(x = HorzBreak, y = InducedVertBreak, color = factor(TaggedPitchType))) +
  geom_vline(xintercept = seq(-20, 20, 10), linetype = "dashed", color = "lightgray") +
  geom_hline(yintercept = seq(-20, 20, 10), linetype = "dashed", color = "lightgray") +
  geom_vline(xintercept = 0, linewidth = .5) +
  geom_hline(yintercept = 0, linewidth = .5) +
  geom_point(size = 1.5, stroke = 1) +
  stat_ellipse(linetype = "solid", show.legend = FALSE) +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  xlab("Horizontal Break (in)") +
  ylab("Induced Vertical Break (in)") +
  labs(color = "Pitch Type", title = "Pitch Movement") +
  scale_x_continuous(breaks = seq(-20, 20, 10), limits = c(-25, 25)) +
  scale_y_continuous(breaks = seq(-20, 20, 10), limits = c(-25, 25)) +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(data = pitcher_data, aes(x = RelSide, y = RelHeight, color = factor(TaggedPitchType))) +
  geom_point(size = 1.5, stroke = 1) +
  stat_ellipse(linetype = "solid", show.legend = FALSE) +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  xlab("Release Side (ft)") +
  ylab("Release Height (ft)") +
  labs(color = "Pitch Type", title = "Release Location") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)

# Plot 3
plot3 <- ggplot(data = pitcher_data, aes(x = RelSpeed, y = factor(TaggedPitchType), color = factor(TaggedPitchType))) +
  geom_boxplot(fill = "transparent", alpha = .75, size = .5) +
  #geom_jitter(width = 1, alpha = 0.5) +
  scale_color_manual(values = pitch_colors) +
  theme_minimal() +
  ylab("Pitch Type") +
  xlab("Pitch Speed (mph)") +
  labs(color = "Pitch Type", title = "Velocity Bands") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)  # Add this line to remove the legend

# Plot 4
plot4 <- ggplot(data = pitcher_data, aes(x = SpinRate, y = factor(TaggedPitchType), color = factor(TaggedPitchType))) +
  geom_boxplot(fill = "transparent", alpha = .75, size = .5) +
  scale_color_manual(values = pitch_colors) +
  theme_minimal() +
  ylab("Pitch Type") +
  xlab("Spin Rate (rpm)") +
  labs(color = "Pitch Type", title = "Spin Bands") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)  # Add this line to remove the legend

plot5 <- ggplot(data = pitcher_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(title = "All Pitches (Pitcher POV)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 2) +
  guides(color=FALSE)

plot6 <- ggplot(pitcher_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradientn(colors = c("#3e66cc", "#ffffff", "#c90000")) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  coord_fixed(ratio = 2) +
  xlim(-3,3) +
  ylim(0,4) +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(title = "All Pitches") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot7 <- ggplot(pitcher_data, aes(x = PitchCount, y = SpinRate, color = TaggedPitchType, group = TaggedPitchType)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pitch_colors) +
  theme_minimal() +
  labs(x = "Pitch Count", y = "Spin Rate (rpm)", title = "Spin Rate Pro/Regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)

plot8 <- ggplot(pitcher_data, aes(x = PitchCount, y = RelSpeed, color = TaggedPitchType, group = TaggedPitchType)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pitch_colors) +
  theme_minimal() +
  labs(x = "Pitch Count", y = "Pitch Speed (mph)", title = "Velocity Pro/Regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)

# Plot 9
plot9 <- ggplot(pitcher_data, aes(x = TiltDegrees, fill = TaggedPitchType, color = TaggedPitchType)) +
  geom_histogram(binwidth = 12.5, size = .75) +
  coord_polar() +
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 45), minor_breaks = seq(0, 360, by = 45)) +
  scale_fill_manual(values = pitch_colors) +
  scale_color_manual(values = pitch_colors) +
  guides(fill = FALSE, color = FALSE) +
  theme_minimal() +
  labs(title = "Spin Axis",
       x = "Degrees",
       y = "Pitch Count") +
  theme(plot.title = element_text(hjust = 0.5))

plot10 <- ggplot(fastballs, aes(x = PlateLocSide, y = PlateLocHeight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradientn(colors = c("#3e66cc", "#ffffff", "#c90000")) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  coord_fixed(ratio = 2) +
  xlim(-3,3) +
  ylim(0,4) +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(title = "Fastball Location") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot11 <- ggplot(ofspd, aes(x = PlateLocSide, y = PlateLocHeight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradientn(colors = c("#3e66cc", "#ffffff", "#c90000")) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  coord_fixed(ratio = 2) +
  xlim(-3,3) +
  ylim(0,4) +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(title = "Offspeed Location") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot12 <- ggplot(brkball, aes(x = PlateLocSide, y = PlateLocHeight)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_gradientn(colors = c("#3e66cc", "#ffffff", "#c90000")) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  coord_fixed(ratio = 2) +
  xlim(-3,3) +
  ylim(0,4) +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(title = "Breaking Ball Location") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot13 <- ggplot(data = lhhswings, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Type", title = "LHH Swings") +
  coord_fixed(ratio = 2)

plot14 <- ggplot(data = rhhswings, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Type", title = "RHH Swings") +
  coord_fixed(ratio = 2)

plot15 <- ggplot(data = lhhwhiffs, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Call", title = "LHH Whiffs") +
  coord_fixed(ratio = 2)

plot16 <- ggplot(data = rhhwhiffs, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Call", title = "RHH Whiffs") +
  coord_fixed(ratio = 2)

# make a condensed dataframe keeping relevant columns 
batted_ball <- pitcher_data %>% filter( PitchCall == 'InPlay', !TaggedHitType %in% c('Bunt',''), !is.na(TaggedHitType)) %>%
  dplyr::select(Date, HomeTeam, AwayTeam, `Top.Bottom`, PitchNo, Batter, ExitSpeed, TaggedPitchType, PitchCall ,PlayResult, TaggedHitType, Angle,
                HitSpinRate, Direction, PlateLocSide,  Distance, Bearing, pfxx, pfxz  )%>%
  # Yakkertech data provides Bearing, Distance, and Angle. To get the hit coordinates (landing point) of the batted ball, we create new columns called hc_x, and hc_y 
  mutate(hc_x = sin(Bearing * pi/180)*Distance ,
         hc_y = cos(Bearing * pi/180)*Distance ,
         # recode the play results for a cleaner look
         PlayResult = recode(PlayResult, Double = 'Double', HomeRun = 'Home Run', Single = 'Single', Triple = 'Triple', Sacrifice = 'Sacrifice', Error = 'Error',
                             FieldersChoice = 'FC', Out = 'Field Out'),
         # recode the play hit types for a cleaner look
         TaggedHitType = recode(TaggedHitType, FlyBall = 'FB', LineDrive = 'LD', GroundBall = 'GB', Popup = 'PU'),
         Matchup = ifelse(`Top.Bottom` == 'Bottom', 
                          paste(HomeTeam, 'vs.', AwayTeam), 
                          paste(AwayTeam, 'at', HomeTeam) )  )

plot17 <- ggplot(batted_ball, aes(x = hc_x, y = hc_y)) + 
  geom_mlb_stadium(stadium_ids = 'braves',
                   stadium_transform_coords = TRUE, 
                   stadium_segments = 'all', 
                   linewidth = .5, 
                   color = 'black') + 
  theme_void() + 
  geom_point(aes(fill = PlayResult), colour = 'black', shape = 21, stroke = 0.5, size = 2) + 
  scale_fill_manual(values = c("Field Out" = "grey", "Single" = "#FE6100", "Double" = "#785EF0", "Triple" = "#FFB000", "Home Run" = "#DC267F"), 
                    breaks = c("Field Out", "Single", "Double", "Triple", "Home Run"),
                    limits = c("Field Out", "Single", "Double", "Triple", "Home Run")) +
  theme(panel.background = element_rect(fill = 'white', colour = NA),  # Remove border by setting colour to NA
        plot.title = element_text(hjust = 0.5)) +
  coord_fixed() +
  labs(title = "Batted Ball Chart",
       fill = 'Hit Type')

plot18 <- ggplot(data = fastballs, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Call", title = "Fastball Location") +
  coord_fixed(ratio = 2)

plot19 <- ggplot(data = ofspd, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Call", title = "Offspeed Location") +
  coord_fixed(ratio = 2)

plot20 <- ggplot(data = brkball, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
  ylim(0,4) +
  xlim(-3,3) +
  geom_point(size = 3.5, stroke = 1) +
  geom_segment(aes(x = -0.4, xend = -0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = 0.4, xend = 0.4, y = 1.3, yend = 3.1), color = "lightgray", linetype = "dashed") +
  geom_segment(aes(x = -1.20, xend = 1.20, y = 1.9, yend = 1.9), color = "lightgray", linetype = "dashed") + 
  geom_segment(aes(x = -1.20, xend = 1.20, y = 2.5, yend = 2.5), color = "lightgray", linetype = "dashed") +
  geom_rect(xmin = -1.20, xmax = 1.20, ymin = 1.30, ymax = 3.1, show.legend = FALSE, fill = "transparent", color = "black") +
  scale_color_manual(values = pitch_colors) +
  guides(color = guide_legend(override.aes = list(shape = 19))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  xlab("Plate Side (ft)") +
  ylab("Plate Height (ft)") +
  labs(color = "Pitch Call", title = "Breaking Location") +
  coord_fixed(ratio = 2)

plot21 <- ggplot(pitcher_data, aes(x = PitchCount, y = InducedVertBreak, color = TaggedPitchType, group = TaggedPitchType)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pitch_colors) +
  theme_minimal() +
  labs(x = "Pitch Count", y = "Induced Vertical Break (inches)", title = "IVB Pro/Regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)

plot22 <- ggplot(pitcher_data, aes(x = PitchCount, y = HorzBreak, color = TaggedPitchType, group = TaggedPitchType)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = pitch_colors) +
  theme_minimal() +
  labs(x = "Pitch Count", y = "Horiziontal Break (inches)", title = "HB Pro/Regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=FALSE)

combined_page1 <- (plot17  | plot1  + theme(aspect.ratio = 1)) /
  (plot21  + theme(aspect.ratio = 1) | plot22  + theme(aspect.ratio = 1))

combined_page2 <-  (plot2 + theme(aspect.ratio = 1)  | plot9) /
  (plot3 + theme(aspect.ratio = 1)  | plot8)

combined_page3 <-  (plot4 + theme(aspect.ratio = 1)  | plot7) /
  (plot5  | plot6)

combined_page4 <-  (plot18  | plot10) /
  (plot19  | plot11)

combined_page5 <-  (plot20  | plot12) /
  (plot13  | plot14)

combined_page6 <- (plot15  | plot16)

# Assuming you've already loaded the necessary libraries and created your plots

# Create a list of the combined pages
plot_pages <- list(combined_page1, combined_page2, combined_page3, combined_page4, combined_page5, combined_page6)

# Start PDF device
pdf("/Users/Mason/Desktop/rimages/combinedreport.pdf", width = 8.5, height = 11)

# Loop through each page in plot_pages and print
for (page in plot_pages) {
  print(page)
}

# Turn off the device
dev.off()

setwd("/Users/Mason/Desktop/rimages")

qpdf::pdf_combine(input = c("combinedtable.pdf", "combinedreport.pdf"),
                  output = output_pdf)