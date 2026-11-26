rm(list=ls())
todays_date <- "2026.06.01"

####################################################################################################################################
# General information
####################################################################################################################################

# Author: Marie de Bakker
# Date: 01/10/2024
# Information: data cleaning

####################################################################################################################################
# Packages
####################################################################################################################################

library(readr)
library(readxl)
library(dplyr)
library(gridExtra)
library(tidyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(Hmisc)
library(scales)

####################################################################################################################################
# Additional information
####################################################################################################################################



####################################################################################################################################
# Load data into R
####################################################################################################################################

# load functions
source("I:/PROJECTS/iPREVENT - primary prevention/Scripts/Functions/helper_functions.R")
source("I:/PROJECTS/iPREVENT - primary prevention/Scripts/Functions/tax_year_plot.R")

####################################################################################################################################
# Figure 1
####################################################################################################################################

df_long <- read.csv("I:/PROJECTS/iPREVENT - primary prevention/Data generated during analysis/000_data disclosures/Disclosure IV - risk assessment/Datasets used to create final figures and tables/2026.04.08 table_figure 1.csv")

# ----------------------------------------
variables_of_interest <- c("assign_score_valid", "all_assign_components_valid")
legend_labels <- c("ASSIGN risk score", 
                   "Composite of ASSIGN risk factors")

figure_1 <- tax_year_plot(dataset = df_long,
                          variables_of_interest = variables_of_interest,
                          legend_labels = legend_labels,
                          limits_yaxis = c(0, 1), 
                          yaxis_breaks = 0.2,
                          title = "", 
                          line_types = c("solid", "dashed", "solid"),
                          colors = c("black", "black"),
                          activity_threshold = 3*30.437,
                        legend_position = "right",
                        label_yaxis = "Cardiovascular risk assessment recording")

plot_1 <- figure_1[[1]]
plot_1

pdf(file = paste0("I:/PROJECTS/iPREVENT - primary prevention/Data generated during analysis/002_time trend plots/", todays_date, " Figure 1.pdf"),
    width = 13, height = 6)
plot_1
dev.off()

pdf(file = paste0("I:/PROJECTS/iPREVENT - primary prevention/Data generated during analysis/002_time trend plots/", todays_date, " Figure 1_graphical abstract.pdf"),
    width = 15, height = 6)
plot_1
dev.off()


####################################################################################################################################
# Figure 2
####################################################################################################################################

variables_of_interest <- c("assign_score_valid")
legend_labels <- c("ASSIGN risk score")

# sex  ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure 2_sex.csv")
df_long$stratified_by <- as.factor(df_long$stratified_by)

figure_2a <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "sex",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.4), 
                           title = "", 
                           line_types = c("solid", "solid"),
                           colors = c("#DD3C51",  "#313657"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_sex <- figure_2a[[1]]
plot_sex

# age  ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure 2_age.csv")

df_long$stratified_by <- as.factor(df_long$stratified_by)
df_long$stratified_by <- relevel(df_long$stratified_by, ">70 years")
df_long$stratified_by <- relevel(df_long$stratified_by, "50 to 70 years")
df_long$stratified_by <- relevel(df_long$stratified_by, "<50 years")

figure_2b <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "age_groups",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.4), 
                           title = "", 
                           line_types = c("solid", "solid"),
                           colors = c("#DD3C51",  "#313657", "#1F6683"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_age <- figure_2b[[1]]
plot_age

# simd  ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure 2_simd.csv")

df_long$stratified_by <- as.factor(df_long$stratified_by)
df_long$stratified_by <- relevel(df_long$stratified_by, "Group 3 (Q5, least deprived)")
df_long$stratified_by <- relevel(df_long$stratified_by, "Group 2 (Q2 to Q4)")
df_long$stratified_by <- relevel(df_long$stratified_by, "Group 1 (Q1, most deprived)")

figure_2c <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "simd",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.4), 
                           title = "", 
                           line_types = c("solid", "solid"),
                           colors = c("#DD3C51",  "#313657", "#1F6683"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_simd <- figure_2c[[1]]
plot_simd

# ethnicity ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure 2_ethnicity.csv")

df_long$stratified_by <- as.factor(df_long$stratified_by)
df_long$stratified_by <- relevel(df_long$stratified_by, "Other")
df_long$stratified_by <- relevel(df_long$stratified_by, "White")
df_long$stratified_by <- relevel(df_long$stratified_by, "Mixed")
df_long$stratified_by <- relevel(df_long$stratified_by, "Black")
df_long$stratified_by <- relevel(df_long$stratified_by, "Asian")

figure_2d <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "ethnicity",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.4), 
                           title = "", 
                           line_types = c("solid", "solid", "solid", "solid"),
                           colors = c("#313657", "#DD3C51", "#1F6683","#CABEE9", "#7C7189"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_ethnicity <- figure_2d[[1]]
plot_ethnicity

# combined
combined_plot <- plot_grid(plot_age, plot_sex, plot_simd, plot_ethnicity, 
                           ncol = 2, nrow = 2, align = "hv",
                           labels = c("A. Age groups", "B. Sex", 
                                      "C. Socioeconomic status", "D. Ethnicity"),
                           
                           label_x = 0.15, hjust = 0)
combined_plot

pdf(file = paste0("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Tables and figures/", todays_date, " Figure 2.pdf"),
    width = 10, height = 8)
combined_plot
dev.off()


####################################################################################################################################
# Supplemental Figure SX
####################################################################################################################################

variables_of_interest <- c("all_assign_components_valid")
legend_labels <- c("Composite of ASSIGN risk factors")

# sex  ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure SX components_sex.csv")
df_long$stratified_by <- as.factor(df_long$stratified_by)

figure_SXa <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "sex",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.5), 
                           title = "", 
                           line_types = c("solid", "solid"),
                           colors = c("#DD3C51",  "#313657"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_sex <- figure_SXa[[1]]
plot_sex

# age  ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure SX components_age.csv")

df_long$stratified_by <- as.factor(df_long$stratified_by)
df_long$stratified_by <- relevel(df_long$stratified_by, ">70 years")
df_long$stratified_by <- relevel(df_long$stratified_by, "50 to 70 years")
df_long$stratified_by <- relevel(df_long$stratified_by, "<50 years")

figure_SXb <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "age_groups",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.5), 
                           title = "", 
                           line_types = c("solid", "solid"),
                           colors = c("#DD3C51",  "#313657", "#1F6683"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_age <- figure_SXb[[1]]
plot_age

# simd  ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure SX components_simd.csv")

df_long$stratified_by <- as.factor(df_long$stratified_by)
df_long$stratified_by <- relevel(df_long$stratified_by, "Group 3 (Q5, least deprived)")
df_long$stratified_by <- relevel(df_long$stratified_by, "Group 2 (Q2 to Q4)")
df_long$stratified_by <- relevel(df_long$stratified_by, "Group 1 (Q1, most deprived)")

figure_SXc <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "simd",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.5), 
                           title = "", 
                           line_types = c("solid", "solid"),
                           colors = c("#DD3C51",  "#313657", "#1F6683"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_simd <- figure_SXc[[1]]
plot_simd

# ethnicity ----------------------------------------
df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure SX components_ethnicity.csv")

df_long$stratified_by <- as.factor(df_long$stratified_by)
df_long$stratified_by <- relevel(df_long$stratified_by, "Other")
df_long$stratified_by <- relevel(df_long$stratified_by, "White")
df_long$stratified_by <- relevel(df_long$stratified_by, "Mixed")
df_long$stratified_by <- relevel(df_long$stratified_by, "Black")
df_long$stratified_by <- relevel(df_long$stratified_by, "Asian")


figure_SXd <- tax_year_plot(dataset = df_long,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "ethnicity",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.5), 
                           title = "", 
                           line_types = c("solid", "solid", "solid", "solid"),
                           colors = c("#313657", "#DD3C51", "#1F6683","#CABEE9", "#7C7189"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_ethnicity <- figure_SXd[[1]]
plot_ethnicity

# combined
combined_plot <- plot_grid(plot_age, plot_sex, plot_simd, plot_ethnicity, 
                           ncol = 2, nrow = 2, align = "hv",
                           labels = c("A. Age groups", "B. Sex", 
                                      "C. Socioeconomic status", "D. Ethnicity"),
                           
                           label_x = 0.15, hjust = 0)
combined_plot

pdf(file = paste0("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Tables and figures/", todays_date, " Figure SX components per subgroup.pdf"),
    width = 10, height = 8)
combined_plot
dev.off()

####################################################################################################################################
# Supplemental Figure S3
####################################################################################################################################

df_simd <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure S5_missing simd.csv")
df_eth <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure S5_missing ethnicity.csv")

df_eth$stratified_by <- as.factor(df_eth$stratified_by)

variables_of_interest <- c("assign_score_valid")
legend_labels <- c("ASSIGN risk score")

# simd
figure_simd_1 <- tax_year_plot(dataset = df_simd,
                            variables_of_interest = variables_of_interest,
                            stratified_by = "simd",
                            legend_labels = legend_labels,
                            limits_yaxis = c(0, 0.4),
                            title = "",
                            line_types = c("solid"),
                            colors = c("grey50"),
                            activity_threshold = 3*30.437,
                            legend_position = "bottom")

plot_simd_1 <- figure_simd_1[[1]]
plot_simd_1

figure_simd_2 <- tax_year_plot_eligibility(dataset = df_simd,
                                        variables_of_interest = variables_of_interest,
                                        stratified_by = "simd",
                                        legend_labels = legend_labels,
                                        limits_yaxis = c(0, 300000),
                                        yaxis_breaks = 50000,
                                        title = "",
                                        line_types = c("solid"),
                                        colors = c("grey50"),
                                        activity_threshold = 3*30.437,
                                        legend_position = "bottom")

plot_simd_2 <- figure_simd_2[[1]]
plot_simd_2

plot_simd <- plot_grid(plot_simd_1, plot_simd_2,
                            ncol = 2, nrow = 1, align = "hv")

# ethnicity
figure_eth_1 <- tax_year_plot(dataset = df_eth,
                           variables_of_interest = variables_of_interest,
                           stratified_by = "stratified_by",
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 0.4),
                           title = "",
                           line_types = c("solid", "solid"),
                           colors = c("grey80", "grey50"),
                           activity_threshold = 3*30.437,
                           legend_position = "bottom")

plot_ethnicity_1 <- figure_eth_1[[1]]
plot_ethnicity_1

figure_eth_2 <- tax_year_plot_eligibility(dataset = df_eth,
                                       variables_of_interest = variables_of_interest,
                                       stratified_by = "stratified_by",
                                       legend_labels = legend_labels,
                                       limits_yaxis = c(0, 300000),
                                       yaxis_breaks = 50000,
                                       title = "",
                                       line_types = c("solid", "solid"),
                                       colors = c("grey80", "grey50"),
                                       activity_threshold = 3*30.437,
                                       legend_position = "bottom")

plot_ethnicity_2 <- figure_eth_2[[1]]
plot_ethnicity_2

plot_ethnicity <- plot_grid(plot_ethnicity_1, plot_ethnicity_2,
                           ncol = 2, nrow = 1, align = "hv")

# combined
combined_plot <- plot_grid(plot_simd,
                           plot_ethnicity,
                           ncol = 1, nrow = 2, align = "hv",
                           labels = c("A. Socioeconomic status",
                                      "B. Ethnicity"),
                           label_x = 0.085, hjust = 0)
combined_plot

pdf(file = paste0("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Tables and figures/", todays_date, " Figure SX_missing simd and ethnicity.pdf"),
    width = 10, height = 10)
combined_plot
dev.off()

####################################################################################################################################
# Supplemental Figure S4
####################################################################################################################################

df_long <- read.csv("I:/PROJECTS/iPREVENT - primary prevention/Data generated during analysis/000_data disclosures/Disclosure V - Alice/2026.05.01 table_figure S4.csv")

df_long$assessment_type <- as.factor(df_long$assessment_type)
df_long$assessment_type <- relevel(df_long$assessment_type, "hdl_valid")
df_long$assessment_type <- relevel(df_long$assessment_type, "cholesterol_valid")
df_long$assessment_type <- relevel(df_long$assessment_type, "smoking_status_valid")
df_long$assessment_type <- relevel(df_long$assessment_type, "sbp_valid")

variables_of_interest <- c("sbp_valid",
                           "smoking_status_valid",
                           "cholesterol_valid",
                           "hdl_valid")

legend_labels <- c("Systolic blood pressure",
                   "Smoking status",
                   "Total cholesterol",
                   "High-density lipoprotein cholesterol")

figure_S4 <- tax_year_plot(dataset = df_long[df_long$assessment_type %in% variables_of_interest, ],
                           variables_of_interest = variables_of_interest,
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 1), 
                           title = "", 
                           line_types = c("solid", "solid", "solid", "solid", "solid"),
                           activity_threshold = 3*30.437,
                           legend_position = "right")

plot_S4 <- figure_S4[[1]]

pdf(file = paste0("I:/PROJECTS/iPREVENT - primary prevention/Data generated during analysis/002_time trend plots/", todays_date, " Figure SX_secondary outcomes.pdf"),
    width = 10, height = 5)
plot_S4
dev.off()

####################################################################################################################################
# Supplemental Figure S4
####################################################################################################################################

df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure SX_never smoker consistent.csv")

variables_of_interest <- c("sbp_valid",
                           "new_c_smoking_status_valid",
                           "cholesterol_hdl_valid")

legend_labels <- c("Systolic blood pressure",
                   "Smoking status",
                   "Cholesterol and/or high-density lipoprotein")

figure_SX <- tax_year_plot(dataset = df_long[df_long$assessment_type %in% variables_of_interest, ],
                           variables_of_interest = variables_of_interest,
                           legend_labels = legend_labels,
                           limits_yaxis = c(0, 1), 
                           title = "", 
                           line_types = c("solid", "solid", "solid", "solid", "solid"),
                           activity_threshold = 3*30.437,
                           legend_position = "right")

plot_SX <- figure_SX[[1]]

pdf(file = paste0("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Tables and figures/", todays_date, " Figure SX_secondary outcomes_never smoker consistent.pdf"),
    width = 10, height = 5)
plot_SX
dev.off()

####################################################################################################################################
# Supplemental Figure SX
####################################################################################################################################

df_long_1 <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure SX_never smoker consistent.csv")
df_long_2 <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure 1X_never smoker consistent.csv")

df_long_1 <- df_long_1 %>% filter(assessment_type == "new_c_smoking_status_valid")

df_long <- rbind(df_long_1, df_long_2)
df_long$assessment_type <- as.factor(df_long$assessment_type)

df_long$assessment_type <- relevel(df_long$assessment_type, "new_c_smoking_status_valid")

# version 1 ----------------------------------------
variables_of_interest <- c("new_c_smoking_status_valid", "new_c_all_assign_components_valid")
legend_labels <- c("Smoking status", "Composite of ASSIGN risk factors")

figure_SX <- tax_year_plot(dataset = df_long,
                          variables_of_interest = variables_of_interest,
                          legend_labels = legend_labels,
                          limits_yaxis = c(0, 1),
                          title = "",
                          line_types = c("solid", "solid", "solid"),
                          colors = c("#DD3C51", "#313657"),
                          activity_threshold = 3*30.437,
                          legend_position = "right")

plot_SX <- figure_SX[[1]]
plot_SX

pdf(file = paste0("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Tables and figures/", todays_date, " Figure SX_never smoker consistent.pdf"),
    width = 9, height = 5)
plot_SX
dev.off()


####################################################################################################################################
# Supplemental Figure SX
####################################################################################################################################

# df_long <- read.csv("C:/Users/marie/OneDrive - University of Edinburgh/Temporary documents_when working form home/004_iPREVENT-PP/Datasets/2026.04.08 table_figure S4.csv")

# variables_of_interest <- c("any_glucose_hba1c_valid",
#                            "hba1c_valid",
#                            "fasting_glucose_valid",
#                            "random_glucose_valid",
#                            "glucose_valid")
#
# legend_labels <- c("Any glucose and/or HbA1c",
#                    "HbA1c",
#                    "Fasting glucose",
#                    "Random glucose",
#                    "Glucose (not specified)")
#
# figure_SX <- tax_year_plot(dataset = df_long,
#                            variables_of_interest = variables_of_interest,
#                            legend_labels = legend_labels,
#                            limits_yaxis = c(0, 1),
#                            title = "",
#                            line_types = c("solid", "solid", "solid", "solid", "solid"),
#                            activity_threshold = 3*30.437,
#                            legend_position = "right")
#
# plot_SX <- figure_SX[[1]]
#
# pdf(file = paste0("figure SX_glucose.pdf"),
#     width = 10, height = 5)
# plot_SX
# dev.off()
#

