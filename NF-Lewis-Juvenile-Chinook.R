#===============================================================================
# NFL WILD JUVENILE CHINOOK SEINING PROJECT
#-------------------------------------------------------------------------------
# Prepared by : Chris Kopack (WDFW)
# Last updated: 3/26/2026
# Reporting year: 2025
#===============================================================================



#-------------------------------------------------------------------------------
# 1 — SETUP & PACKAGE LOADING
#-------------------------------------------------------------------------------

# Install packages if needed
# install.packages("dataRetrieval")
# install.packages("tidyverse")
# install.packages("lubridate")

library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(grid)      # for unit()
library(ggplot2)
library(dplyr)



#-------------------------------------------------------------------------------
# 2 — USER INPUTS
#-------------------------------------------------------------------------------

site_id <- "14220500"   # North Fork Lewis River gage
param_cd <- "00060"     # Discharge (cfs)
stat_cd  <- "00003"     # Daily mean

years <- 2016:2025      # Years of interest



#-------------------------------------------------------------------------------
# 3 — FUNCTION: RETRIEVE SEASONAL DISCHARGE DATA
#-------------------------------------------------------------------------------

get_seasonal_flow <- function(year) {
  
  # Define seasonal window
  start_date <- as.Date(paste0(year, "-05-15"))
  end_date   <- as.Date(paste0(year, "-07-15"))
  
  # Pull USGS data
  df <- readNWISdv(
    siteNumbers = site_id,
    parameterCd = param_cd,
    startDate   = start_date,
    endDate     = end_date,
    statCd      = stat_cd
  )
  
  # Clean and format
  df <- renameNWISColumns(df) %>%
    mutate(
      Date = as.Date(Date),
      year = year
    )
  
  return(df)
}



#-------------------------------------------------------------------------------
# 4 — DATA RETRIEVAL & CLEANING
#-------------------------------------------------------------------------------

# Pull all years and combine
flow_data <- map_dfr(years, get_seasonal_flow)

# Keep only relevant columns
flow_data <- flow_data %>%
  select(Date, year, Flow)



#-------------------------------------------------------------------------------
# 5 — DISCHARGE SUMMARY STATISTICS
#-------------------------------------------------------------------------------

flow_summary <- flow_data %>%
  group_by(year) %>%
  summarise(
    mean_flow   = mean(Flow, na.rm = TRUE),
    median_flow = median(Flow, na.rm = TRUE),
    max_flow    = max(Flow, na.rm = TRUE),
    min_flow    = min(Flow, na.rm = TRUE),
    .groups = "drop"
  )

flow_summary



#-------------------------------------------------------------------------------
# 6 — DISCHARGE TABLE (WIDE FORMAT FOR REPORTING)
#-------------------------------------------------------------------------------

flow_wide <- flow_data %>%
  
  # Create consistent day column (MM-DD)
  mutate(day = format(Date, "%m-%d")) %>%
  
  # Keep required fields
  select(year, day, Flow) %>%
  
  # Ensure all dates exist across all years
  complete(
    day = format(seq(as.Date("2000-05-15"),
                     as.Date("2000-07-15"),
                     by = "day"), "%m-%d"),
    year
  ) %>%
  
  # Pivot to wide format (years as columns)
  pivot_wider(
    names_from  = year,
    values_from = Flow
  )

# Sort rows chronologically
flow_wide <- flow_wide %>%
  arrange(day)

# Sort year columns numerically
flow_wide <- flow_wide %>%
  select(day, sort(names(.)[-1]))

flow_wide



#-------------------------------------------------------------------------------
# 7 — EXPORT NFL DISCHARGE DATA: Table B1
#-------------------------------------------------------------------------------

write.csv(flow_wide,
          "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Table_B1_NF_Lewis_Discharge_by_Date_2016_2025.csv",
          row.names = FALSE)  # Table B1 in Report Appendix B



#-------------------------------------------------------------------------------
# 8 — Figure B1: DISCHARGE BY YEAR (FACETED)
#-------------------------------------------------------------------------------

Figure_B1<-ggplot(flow_data, aes(x = Date, y = Flow)) +
  geom_line() +
  facet_wrap(~year, scales = "free_x") +
  labs(
    y = "Discharge (cfs)",
    x = "Date"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    
    # Add spacing between facet panels
    panel.spacing.x = unit(1, "lines"),
    
    # Improve x-axis readability
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x.bottom = element_text(margin = margin(t = 5))
  )

Figure_B1

ggsave(filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_B1_NFL_Discharge_by_Date_and_Year.png",
       plot = Figure_B1,
       width = 7.5,
       height = 5,
       dpi = 600)



#-------------------------------------------------------------------------------
# 9 — Figure 2: DISCHARGE COMPARISON (MULTI-YEAR + MEDIAN)
#-------------------------------------------------------------------------------

# Align years to common plotting year
plot_data <- flow_data %>%
  mutate(
    plot_date = as.Date(format(Date, "2000-%m-%d"))
  )

# Median flow by date
median_flow <- plot_data %>%
  group_by(plot_date) %>%
  summarise(
    median_flow = median(Flow, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
Figure_2<-ggplot() +
  
  # Historical years (grey)
  geom_line(
    data = plot_data %>% filter(year < 2025),
    aes(x = plot_date, y = Flow, group = year),
    color = "grey70",
    linewidth = 0.8
  ) +
  
  # Current year (black)
  geom_line(
    data = plot_data %>% filter(year == 2025),
    aes(x = plot_date, y = Flow),
    color = "black",
    linewidth = 1.2
  ) +
  
  # Median line (dashed)
  geom_line(
    data = median_flow,
    aes(x = plot_date, y = median_flow),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  scale_x_date(
    date_labels = "%b %d",
    date_breaks = "1 week",
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Date",
    y = "Discharge (cfs)"
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line("black")
  )

Figure_2

ggsave(filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_2_NFL_Daily_and_Median_Discharge_2016_2025.png",
       plot = Figure_2,
       width = 5.63,
       height = 3.72,
       dpi = 600)



#-------------------------------------------------------------------------------
# 10 — MERGE CATCH & DISCHARGE DATA
#-------------------------------------------------------------------------------

catch_data <- read.csv("C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/2025CatchData.csv")

# Ensure numeric year
catch_data <- catch_data %>%
  mutate(Year = as.numeric(Year))

flow_summary <- flow_summary %>%
  mutate(Year = as.numeric(year))

# Merge datasets
plot_data <- left_join(catch_data, flow_summary, by = "Year")

# Scaling factor for dual-axis plot
scale_factor <- max(plot_data$Tags.released, na.rm = TRUE) /
  max(plot_data$mean_flow, na.rm = TRUE)

# Remove duplicate year column and remove Est. total catch column
head(plot_data)
plot_data <- plot_data[, -c(2,4)]
plot_data



#-------------------------------------------------------------------------------
# 11 — Figure 3: TAGS RELEASED VS DISCHARGE
#-------------------------------------------------------------------------------

Figure_3<-ggplot(plot_data, aes(x = Year)) +
  
  # Tags released (bars)
  geom_col(aes(y = Tags.released),
           fill = "grey70", width = 1, color = "black") +
  
  # Mean flow (points)
  geom_point(aes(y = mean_flow * scale_factor),
             shape = 21,
             color = "black",
             fill = "white",
             size = 3,
             stroke = 1.2) +
  
  # Min/max flow (error bars)
  geom_errorbar(aes(ymin = min_flow * scale_factor,
                    ymax = max_flow * scale_factor),
                color = "black",
                width = 0.2) +
  
  # Dual y-axis
  scale_y_continuous(
    name = "Tags Released",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Discharge (cfs)"),
    expand = c(0, 0)
  ) +
  
  # Tagging goal reference line
  geom_hline(yintercept = 100000,
             linetype = "dashed",
             color = "black",
             linewidth = 0.8) +
  
  # X-axis formatting
  scale_x_continuous(breaks = 2016:2025,
                     expand = c(0, 0)) +
  
  # Theme
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.y.left  = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right  = element_text(color = "black"),
    panel.border = element_blank(),
    axis.line = element_line("black")
  ) +
  
  labs(x = "Year")

Figure_3

ggsave(filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_3_NFL_Tags_Released_and_Discharge_2016_2025.png",
       plot = Figure_3,
       width = 5.63,
       height = 3.72,
       dpi = 600)



#-------------------------------------------------------------------------------
# 12 — EXPORT FINAL DATASET: Table 1
#-------------------------------------------------------------------------------

write.csv(plot_data,
          "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Table_1_NFL_Juv_CK_Tags_Released_by_Discharge_2016_2025.csv",
          row.names = FALSE) # Table 1 in Report


citation()
#===============================================================================
# END OF SCRIPT
#===============================================================================

