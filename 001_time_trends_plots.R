rm(list=ls())

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
# Load data into R
####################################################################################################################################

# load functions
source("I:/PROJECTS/iPREVENT - primary prevention/Scripts/Functions/helper_functions.R")
source("I:/PROJECTS/iPREVENT - primary prevention/Scripts/Functions/tax_year_plot.R")

####################################################################################################################################
# Figure 1
####################################################################################################################################

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

####################################################################################################################################
# Figure 2
####################################################################################################################################

variables_of_interest <- c("assign_score_valid")
legend_labels <- c("ASSIGN risk score")

# sex  ----------------------------------------
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



