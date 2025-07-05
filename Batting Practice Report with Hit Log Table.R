# Load Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gtable)
library(grid)
library(GeomMLBStadiums)
library(ggrepel)

# Set Working Directory and Read Data
setwd("~/Downloads/UConnCSV/UConn Fall 2024")
data <- read.csv("UConnBP7.csv")

# Get All Batters on Team
batters <- data %>%
  filter(BatterTeam == "UCO_HUS") %>%
  pull(Batter) %>%
  unique()

# Loop Through Each Batter
for (batter in batters) {
  name_split <- strsplit(batter, ", ")[[1]]
  formatted_name <- paste(name_split[2], name_split[1])
  hitter_name <- batter
  stats <- data %>% filter(Batter == hitter_name)
  if (nrow(stats) == 0) next
  
  # Exit Velo Zone Chart
  make_zone_exit_speed <- function(hitter) {
    result <- stats %>% filter(Batter == hitter & !is.na(ExitSpeed))
    x_breaks <- seq(-1.05, 1.05, length.out = 4)
    y_breaks <- seq(1.6, 3.3, length.out = 4)
    tiles <- expand.grid(
      x = x_breaks[-length(x_breaks)] + diff(x_breaks) / 2,
      y = y_breaks[-length(y_breaks)] + diff(y_breaks) / 2
    )
    exit_speed_by_zone <- result %>%
      mutate(
        x_bin = cut(-PlateLocSide, breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
        y_bin = cut(PlateLocHeight, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
      ) %>%
      filter(!is.na(x_bin) & !is.na(y_bin)) %>%
      group_by(x_bin, y_bin) %>%
      summarise(Avg_ExitSpeed = round(mean(ExitSpeed, na.rm = TRUE), 1), .groups = 'drop') %>%
      mutate(
        x = (x_breaks[x_bin] + x_breaks[x_bin + 1]) / 2,
        y = (y_breaks[y_bin] + y_breaks[y_bin + 1]) / 2
      )
    tiles <- merge(tiles, exit_speed_by_zone[, c("x", "y", "Avg_ExitSpeed")], by = c("x", "y"), all.x = TRUE)
    tiles$Avg_ExitSpeed[is.na(tiles$Avg_ExitSpeed)] <- 0
    ggplot() +
      geom_tile(data = tiles, aes(x = x, y = y, fill = Avg_ExitSpeed), color = "black") +
      scale_fill_gradient2(low = "#3661ad", mid = "white", high = "#d82129",
                           midpoint = mean(tiles$Avg_ExitSpeed, na.rm = TRUE), na.value = "white") +
      geom_point(data = result, aes(x = -PlateLocSide, y = PlateLocHeight), color = "gray", size = 1, alpha = 0.6) +
      geom_text(data = tiles, aes(x = x, y = y, label = Avg_ExitSpeed), color = "black") +
      geom_rect(xmin = -1.05, xmax = 1.05, ymin = 1.6, ymax = 3.3, fill = "transparent", color = "black") +
      ylim(0.75, 3.75) + xlim(-2, 2) +
      theme_classic() + xlab("") + ylab("") +
      labs(title = paste("Exit Velocity By Zone for", hitter)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      coord_fixed(ratio = 1.3)
  }
  
  # Launch Angle Zone Chart
  make_zone_angle <- function(hitter) {
    result <- stats %>% filter(Batter == hitter & !is.na(Angle))
    x_breaks <- seq(-1.05, 1.05, length.out = 4)
    y_breaks <- seq(1.6, 3.3, length.out = 4)
    tiles <- expand.grid(
      x = x_breaks[-length(x_breaks)] + diff(x_breaks) / 2,
      y = y_breaks[-length(y_breaks)] + diff(y_breaks) / 2
    )
    angle_by_zone <- result %>%
      mutate(
        x_bin = cut(-PlateLocSide, breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
        y_bin = cut(PlateLocHeight, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
      ) %>%
      filter(!is.na(x_bin) & !is.na(y_bin)) %>%
      group_by(x_bin, y_bin) %>%
      summarise(Avg_Angle = round(mean(Angle, na.rm = TRUE), 1), .groups = 'drop') %>%
      mutate(
        x = (x_breaks[x_bin] + x_breaks[x_bin + 1]) / 2,
        y = (y_breaks[y_bin] + y_breaks[y_bin + 1]) / 2
      )
    tiles <- merge(tiles, angle_by_zone[, c("x", "y", "Avg_Angle")], by = c("x", "y"), all.x = TRUE)
    tiles$Avg_Angle[is.na(tiles$Avg_Angle)] <- 0
    ggplot() +
      geom_tile(data = tiles, aes(x = x, y = y, fill = Avg_Angle), color = "black") +
      scale_fill_gradient2(low = "#3661ad", mid = "white", high = "#d82129",
                           midpoint = mean(tiles$Avg_Angle, na.rm = TRUE), na.value = "white") +
      geom_point(data = result, aes(x = -PlateLocSide, y = PlateLocHeight), color = "gray", size = 1, alpha = 0.6) +
      geom_text(data = tiles, aes(x = x, y = y, label = Avg_Angle), color = "black") +
      geom_rect(xmin = -1.05, xmax = 1.05, ymin = 1.6, ymax = 3.3, fill = "transparent", color = "black") +
      ylim(0.75, 3.75) + xlim(-2, 2) +
      theme_classic() + xlab("") + ylab("") +
      labs(title = paste("Launch Angle By Zone for", hitter)) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
      coord_fixed(ratio = 1.3)
  }
  
  
  # Fancy Themes
  fancy_plot_theme <- theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  )
  
  #Line Drives
  spray_line_drive <- ggplot(
    stats %>%
      filter(TaggedHitType == "LineDrive" & !is.na(Bearing) & !is.na(Distance)) %>%
      mutate(
        hc_x = sin(Bearing * pi / 180) * Distance,
        hc_y = cos(Bearing * pi / 180) * Distance
      ),
    aes(x = hc_x, y = hc_y)
  ) +
    geom_mlb_stadium(stadium_ids = 'dodgers', stadium_transform_coords = TRUE,
                     stadium_segments = 'all', linewidth = 0.5, color = 'black') +
    theme_void() +
    geom_point(fill = "#1E90FF", shape = 21, colour = 'black', stroke = 0.5,
               size = 3, alpha = 0.8) +
    geom_text_repel(aes(label = round(ExitSpeed, 1)), color = "black", size = 3) +
    coord_fixed() +
    labs(title = "Spray Chart - Line Drives") +
    fancy_plot_theme
  
  
  # Fly Balls
  spray_fly_ball <- ggplot(
    stats %>%
      filter(TaggedHitType == "FlyBall" & !is.na(Bearing) & !is.na(Distance)) %>%
      mutate(
        hc_x = sin(Bearing * pi / 180) * Distance,
        hc_y = cos(Bearing * pi / 180) * Distance
      ),
    aes(x = hc_x, y = hc_y)
  ) +
    geom_mlb_stadium(stadium_ids = 'dodgers', stadium_transform_coords = TRUE,
                     stadium_segments = 'all', linewidth = 0.5, color = 'black') +
    theme_void() +
    geom_point(fill = "#228B22", shape = 21, colour = 'black', stroke = 0.5,
               size = 3, alpha = 0.8) +
    geom_text_repel(aes(label = round(ExitSpeed, 1)), color = "black", size = 3) +
    coord_fixed() +
    labs(title = "Spray Chart - Fly Balls") +
    fancy_plot_theme
  
  
  # Ground Balls
  spray_ground_ball <- ggplot(
    stats %>%
      filter(TaggedHitType == "GroundBall" & !is.na(Bearing) & !is.na(Distance)) %>%
      mutate(
        hc_x = sin(Bearing * pi / 180) * Distance,
        hc_y = cos(Bearing * pi / 180) * Distance
      ),
    aes(x = hc_x, y = hc_y)
  ) +
    geom_mlb_stadium(stadium_ids = 'dodgers', stadium_transform_coords = TRUE,
                     stadium_segments = 'all', linewidth = 0.5, color = 'black') +
    theme_void() +
    geom_point(fill = "#8B4513", shape = 21, colour = 'black', stroke = 0.5,
               size = 3, alpha = 0.8) +
    geom_text_repel(aes(label = round(ExitSpeed, 1)), color = "black", size = 3) +
    coord_fixed() +
    labs(title = "Spray Chart - Ground Balls") +
    fancy_plot_theme
  
  
  spray_line_drive <- spray_line_drive + fancy_plot_theme
  spray_fly_ball <- spray_fly_ball + fancy_plot_theme
  spray_ground_ball <- spray_ground_ball + fancy_plot_theme
  
  spray_charts_combined <- arrangeGrob(
    spray_line_drive,
    spray_fly_ball,
    spray_ground_ball,
    ncol = 3
  )
  
  fancy_table_theme <- ttheme_minimal(
    core = list(fg_params = list(fontsize = 8.25, fontface = "plain"), bg_params = list(fill = "white", col = "black")),
    colhead = list(fg_params = list(fontsize = 8.25, fontface = "bold", col = "white"), bg_params = list(fill = "navyblue", col = "black"))
  )
  # Batted Ball Type Summary Table
  batted_ball_data <- stats %>%
    filter(TaggedHitType %in% c("GroundBall", "FlyBall", "LineDrive", "Popup"), !is.na(Angle))
  
  total_batted_events <- nrow(batted_ball_data)
  
  batted_ball_table <- batted_ball_data %>%
    group_by(TaggedHitType) %>%
    summarise(
      Count = n(),
      "%" = round(n() / total_batted_events * 100, 1),
      "Avg Exit Velocity" = round(mean(ExitSpeed, na.rm = TRUE), 1),
      "Exit Velocity 90" = round(quantile(ExitSpeed, 0.9, na.rm = TRUE), 1),
      "Max Exit Velocity" = round(max(ExitSpeed, na.rm = TRUE), 1),
      "HardHit%" = round(sum(ExitSpeed > 90, na.rm = TRUE) / n() * 100, 1),
      "Avg Launch Angle" = round(mean(Angle, na.rm = TRUE), 1),
      "Hard Hit Launch Angle" = round(mean(Angle[ExitSpeed >= 90], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    bind_rows(
      tibble(
        TaggedHitType = "All",
        Count = nrow(batted_ball_data),
        "%" = 100,
        "Avg Exit Velocity" = round(mean(batted_ball_data$ExitSpeed, na.rm = TRUE), 1),
        "Exit Velocity 90" = round(quantile(batted_ball_data$ExitSpeed, 0.9, na.rm = TRUE), 1),
        "Max Exit Velocity" = round(max(batted_ball_data$ExitSpeed, na.rm = TRUE), 1),
        "HardHit%" = round(sum(batted_ball_data$ExitSpeed > 90, na.rm = TRUE) / nrow(batted_ball_data) * 100, 1),
        "Avg Launch Angle" = round(mean(batted_ball_data$Angle, na.rm = TRUE), 1),
        "Hard Hit Launch Angle" = round(mean(batted_ball_data$Angle[batted_ball_data$ExitSpeed >= 90], na.rm = TRUE), 1)
      )
    )
  
  
  batted_title <- textGrob("Hitting Metrics by Batted Ball Type",
                           gp = gpar(fontsize = 14, fontface = "bold.italic", col = "black"),
                           just = "center")
  
  batted_grob <- tableGrob(batted_ball_table, rows = NULL, theme = fancy_table_theme)
  batted_grob_with_title <- arrangeGrob(batted_title, batted_grob, ncol = 1, heights = c(0.05, 0.95))
  
  #Hit Log for every Batted Ball
  detailed_hits_table <- stats %>%
    filter(TaggedHitType != "Undefined") %>%
    select(PitchNo, ExitSpeed, Angle, Distance, TaggedHitType) %>%
    filter(!is.na(ExitSpeed) | !is.na(Angle) | !is.na(Distance) | !is.na(TaggedHitType))
  
  detailed_hits_grob <- tableGrob(detailed_hits_table, rows = NULL, theme = fancy_table_theme)
  
  detailed_grob <- arrangeGrob( detailed_hits_grob, ncol = 1, heights = c(0.05, 0.95))
  
  
  # Generate Plots
  exit_velo_zone_chart <- make_zone_exit_speed(hitter_name)
  launch_angle_zone_chart <- make_zone_angle(hitter_name)
  
  # Title
  title_grob <- textGrob(
    paste0(formatted_name, " Batting Practice Report"),
    gp = gpar(fontsize = 20, fontface = "bold"),
    hjust = 0.5
  )
  
  # Combine Exit Velo + Launch Angle zone charts
  zone_charts_combined <- arrangeGrob(
    exit_velo_zone_chart,
    launch_angle_zone_chart,
    ncol = 2
  )
  
  # Combine Spray Chart with zone charts
  spray_and_zone_combined <- arrangeGrob(
    spray_charts_combined,
    zone_charts_combined,
    nrow = 2,
    heights = c(1.3, 1.4)
  )
  
  # Combine everything into left column (title excluded)
  left_column <- arrangeGrob(
    batted_grob_with_title,
    zone_charts_combined,
    spray_charts_combined,
    ncol = 1,
    heights = c(0.4, 1.2, 1)
  )
  
  # Two-column layout: left (charts/tables), right (detailed table)
  main_body <- arrangeGrob(
    arrangeGrob(left_column, top = NULL),  # raises the left column
    arrangeGrob(detailed_grob, top = NULL, vp = viewport(y = 0.6, just = "top")),  # lowers the right column
    ncol = 2,
    widths = c(2.5, 1)
  )
  
  # Full layout: title on top, body below
  report_layout <- arrangeGrob(
    title_grob,
    main_body,
    ncol = 1,
    heights = c(.1, 1)  # Adjust height ratio to balance title and body
  )
  
  
  # Save PDF
  pdf_filename <- paste0(formatted_name, " Batting Practice Report.pdf")
  pdf(pdf_filename, width = 13, height = 12)
  grid.draw(report_layout)
  dev.off()
  
  cat("PDF saved as '", pdf_filename, "'\n", sep = "")
}
