#===============================================================================
# NFL WILD JUVENILE CHINOOK SEINING PROJECT
#-------------------------------------------------------------------------------
# Prepared by : Chris Kopack (WDFW)
# Last updated: 3/26/2026
# Reporting year: 2025
#-------------------------------------------------------------------------------
# Description:
# This script retrieves USGS discharge data, summarizes seasonal flow,
# integrates tagging and escapement data, and generates report-ready
# tables and figures (Table B1, Figures B1 & 2–5).
#-------------------------------------------------------------------------------
# WORKFLOW OVERVIEW:
#   1. Setup
#   2. Discharge Data Pipeline (USGS retrieval → summary → formatting → figures)
#   3. Catch Data Pipeline
#   4. Abundance Data Pipeline
#   5. Data Integration
#   6. Final Tables & Figures
#===============================================================================



#===============================================================================
# 1 — SETUP
#===============================================================================

# Install packages if needed
# install.packages("dataRetrieval")

library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(grid)
library(ggplot2)
library(dplyr)
library(stringi)
library(tidyr)
library(ggrepel)
library(scales)
library(glue)

#-----------------------------
# User Inputs
#-----------------------------
site_id <- "14220500"
param_cd <- "00060"
stat_cd  <- "00003"

years <- 2016:2025

#-----------------------------
# Import Data
#-----------------------------
ui_datafiles_wd <-c("/data") #glue("T:/DFW-Team FP R5 Chum - General/Analysis/Juvenile_estimates/data/{ui_trap_year}")
ui_dataname_catch <- c("2025CatchData.csv")
ui_dataname_esc <- c("NFLAdultChinookAbundance_2024.csv")



#===============================================================================
# 2 — FLOW DATA PIPELINE
#===============================================================================

#-----------------------------
# 2.1 — Function: Retrieve Discharge
#-----------------------------
get_seasonal_flow <- function(year) {
  
  start_date <- as.Date(paste0(year, "-05-15"))
  end_date   <- as.Date(paste0(year, "-07-15"))
  
  df <- readNWISdv(
    siteNumbers = site_id,
    parameterCd = param_cd,
    startDate   = start_date,
    endDate     = end_date,
    statCd      = stat_cd
  )
  
  df <- renameNWISColumns(df) %>%
    mutate(
      Date = as.Date(Date),
      year = year
    )
  
  return(df)
}

#-----------------------------
# 2.2 — Retrieve & Clean Discharge Data
#-----------------------------
flow_data <- map_dfr(years, get_seasonal_flow)

flow_data <- flow_data %>%
  select(Date, year, Flow)

#-----------------------------
# 2.3 — Discharge Summary
#-----------------------------
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

#-----------------------------
# 2.4 — Discharge Table (Wide Format)
#-----------------------------
flow_wide <- flow_data %>%
  mutate(day = format(Date, "%m-%d")) %>%
  select(year, day, Flow) %>%
  complete(
    day = format(seq(as.Date("2000-05-15"),
                     as.Date("2000-07-15"),
                     by = "day"), "%m-%d"),
    year
  ) %>%
  pivot_wider(names_from = year, values_from = Flow)

flow_wide <- flow_wide %>% arrange(day)
flow_wide <- flow_wide %>% select(day, sort(names(.)[-1]))

flow_wide

#-----------------------------
# 2.5 — Discharge Figures
#-----------------------------

# Figure B1 (faceted hydrographs)
Figure_B1 <- ggplot(flow_data, aes(x = Date, y = Flow)) +
  geom_line() +
  facet_wrap(~year, scales = "free_x") +
  labs(y = "Discharge (CFS)", x = "Date") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x.bottom = element_text(margin = margin(t = 5))
  )

Figure_B1

ggsave(
  filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_B1_NFL_Discharge_by_Date_and_Year.png",
  plot = Figure_B1,
  width = 7.5,
  height = 5,
  dpi = 600
)

# Figure 2 (multi-year comparison)
plot_data <- flow_data %>%
  mutate(plot_date = as.Date(format(Date, "2000-%m-%d")))

median_flow <- plot_data %>%
  group_by(plot_date) %>%
  summarise(median_flow = median(Flow, na.rm = TRUE), .groups = "drop")

Figure_2 <- ggplot() +
  geom_line(
    data = plot_data %>% filter(year < 2025),
    aes(x = plot_date, y = Flow, group = year),
    color = "grey70", linewidth = 0.8
  ) +
  geom_line(
    data = plot_data %>% filter(year == 2025),
    aes(x = plot_date, y = Flow),
    color = "black", linewidth = 1.2
  ) +
  geom_line(
    data = median_flow,
    aes(x = plot_date, y = median_flow),
    linetype = "dashed", linewidth = 1.2
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week", expand = c(0, 0)) +
  scale_y_continuous(labels = comma) +
  labs(x = "Date", y = "Discharge (CFS)") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line("black")
  )

Figure_2

ggsave(
  filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_2_NFL_Median_Daily_Discharge_2016_2025.png",
  plot = Figure_2,
  width = 7.5,
  height = 5,
  dpi = 600
)



#===============================================================================
# 3 — CATCH DATA PIPELINE
#===============================================================================

catch_data <- read.csv(file = glue("{here::here()}/{ui_datafiles_wd}/{ui_dataname_catch}")) #read.csv("C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/2025CatchData.csv")

catch_data <- catch_data %>%
  mutate(Year = as.numeric(Year))



#===============================================================================
# 4 — ABUNDANCE DATA PIPELINE
#===============================================================================

abundance_data <- read.csv(file = glue("{here::here()}/{ui_datafiles_wd}/{ui_dataname_esc}"),
  fileEncoding = "Latin1")

abundance_data <- abundance_data %>%
  mutate(across(where(is.character), ~stri_trim_both(.))) %>%
  mutate(
    Median = as.numeric(gsub("[^0-9\\.]", "", Median)),
    SD     = as.numeric(gsub("[^0-9\\.]", "", SD)),
    X.CV   = as.numeric(gsub("[^0-9\\.]", "", X.CV)),
    Year   = as.numeric(gsub("[^0-9]", "", Year))
  )

fall_bright_summary <- abundance_data %>%
  filter(
    str_detect(Run, regex("Fall[- ]?Bright", ignore_case = TRUE)),
    str_detect(Age_Composition, regex("^ij$", ignore_case = TRUE))
  ) %>%
  group_by(Year) %>%
  summarise(Total_Median = sum(Median, na.rm = TRUE)) %>%
  arrange(Year)

fall_bright_summary

fall_bright_summary_lag <- fall_bright_summary %>%
  mutate(Year = Year + 1, Spawners = Total_Median) %>%
  select(Year, Spawners)



#===============================================================================
# 5 — DATA INTEGRATION
#===============================================================================

flow_summary <- flow_summary %>%
  mutate(Year = as.numeric(year))

plot_data <- left_join(catch_data, flow_summary, by = "Year")

scale_factor <- max(plot_data$Tags.released, na.rm = TRUE) /
  max(plot_data$mean_flow, na.rm = TRUE)

plot_data <- plot_data[, -c(2,4,5)]

plot_data <- plot_data %>%
  left_join(fall_bright_summary_lag, by = "Year") %>%
  select(Year, Tags.released, Spawners, everything())

tag_geomean<-exp(mean(log(plot_data$Tags.released)))
tag_geomean

esc_geomean<-exp(mean(log(plot_data$Spawners)))
esc_geomean

dis_geomean<-exp(mean(log(plot_data$median_flow)))
dis_geomean



#===============================================================================
# 6 — FINAL OUTPUTS (TABLES & FIGURES)
#===============================================================================

#-----------------------------
# 6.1 - Table 1: Tagging Summary Data
#-----------------------------
write.csv(
  plot_data,
  "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Table_1_NF_Lewis_Tagging_Data_2016_2025.csv",
  row.names = FALSE)

#-----------------------------
# 6.2 - Figure 3: Tags vs Discharge
#-----------------------------
Figure_3 <- ggplot(plot_data, aes(x = median_flow, y = Tags.released)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = min_flow, xmax = max_flow), height = 0, color = "skyblue") +
  geom_text_repel(aes(label = Year), size = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(x = "Discharge (CFS, Median with Min/Max Range)", y = "Number of Tags Released") +
  theme_bw() +
  theme(panel.grid = element_blank())

Figure_3

ggsave(
  filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_3_NFL_Tags_vs_Discharge.png",
  plot = Figure_3,
  width = 7.5,
  height = 5,
  dpi = 600
)

# NOTES:
# Each point = median flow for that year
# Horizontal line = min → max flow over the seining season

#-----------------------------
# 6.3 - Figure 4: Tags vs Spawners
#-----------------------------
Figure_4 <- ggplot(plot_data, aes(x = Spawners, y = Tags.released)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text_repel(aes(label = Year), size = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(x = "Prior-Year Escapement Estimate", y = "Number of Tags Released") +
  theme_bw() +
  theme(panel.grid = element_blank())

Figure_4

ggsave(
  filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_4_NFL_Tags_vs_Spawners.png",
  plot = Figure_4,
  width = 7.5,
  height = 5,
  dpi = 600
)

# NOTES:
# Simple scatter plot shows independent effect of escapement

#-----------------------------
# 6.4 - Figure 5: Tags vs Discharge & Spawners
#-----------------------------
Figure_5 <- ggplot(plot_data, aes(x = Spawners, y = Tags.released, size = median_flow)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_text_repel(aes(label = Year), size = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  scale_size_continuous(name = "Median Flow (CFS):", labels = comma) +
  labs(x = "Prior-Year Escapement Estimate", y = "Number of Tags Released") +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "top")

Figure_5

ggsave(
  filename = "C:/Users/kopcj477/OneDrive - Washington State Executive Branch Agencies/Desktop/Figure_5_NFL_Tags_vs_Discharge_Spawners.png",
  plot = Figure_5,
  width = 7.5,
  height = 5,
  dpi = 600
)

# NOTES:
# Adds median flow as bubble size
# Shows combined effect of escapement and flow



#===============================================================================
# 7 — SESSION INFO
#===============================================================================

citation()



#===============================================================================
# END SCRIPT
#===============================================================================