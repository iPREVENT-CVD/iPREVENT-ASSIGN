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
# helper_functions
# function_gp_interaction_plot

####################################################################################################################################
# Figure 1
####################################################################################################################################

variable_of_interest <- c("assignvalid_appt_group")
df_gp$assignvalid_appt_group <- as.factor(df_gp$assignvalid_appt_group)

df_gp$assignvalid_appt_group <- relevel(df_gp$assignvalid_appt_group, "ASSIGN Y, Appt N")
df_gp$assignvalid_appt_group <- relevel(df_gp$assignvalid_appt_group, "ASSIGN Y, Appt Y")
df_gp$assignvalid_appt_group <- relevel(df_gp$assignvalid_appt_group, "ASSIGN N, Appt N")
df_gp$assignvalid_appt_group <- relevel(df_gp$assignvalid_appt_group, "ASSIGN N, Appt Y")

# total population -------------------------------------------------------------------------
df_1 <- df_gp[, c("year", "assignvalid_appt_group", "n")]
plot_1 <- gp_interaction_plot(dataset = df_1,
                              title = "", 
                              variable_of_interest = variable_of_interest)
plot_1

# age groups --------------------------------------------------------------------------------------
# age 1
df_age_1 <- df_gp[, c("year", "assignvalid_appt_group", "n_under50")] %>% rename(n = n_under50)
plot_age_1 <- gp_interaction_plot(dataset = df_age_1,
                              title = "A. <50 years", 
                              legend_position = "None",
                              variable_of_interest = variable_of_interest)

# age 2
df_age_2 <- df_gp[, c("year", "assignvalid_appt_group", "n_50_70")] %>% rename(n = n_50_70)
plot_age_2 <- gp_interaction_plot(dataset = df_age_2,
                              title = "B. 50 to 70 years", 
                              legend_position = "None",
                              variable_of_interest = variable_of_interest)

# age 3
df_age_3 <- df_gp[, c("year", "assignvalid_appt_group", "n_over70")] %>% rename(n = n_over70)
plot_age_3 <- gp_interaction_plot(dataset = df_age_3,
                                  title = "C. >70 years", 
                                  legend_position = "None",
                                  variable_of_interest = variable_of_interest)

plot_age <- plot_grid(plot_age_1, plot_age_2, plot_age_3, ncol = 2, nrow = 2)
plot_age

# sex --------------------------------------------------------------------------------------
# men
df_m <- df_gp[, c("year", "assignvalid_appt_group", "n_male")] %>% rename(n = n_male)
plot_m <- gp_interaction_plot(dataset = df_m,
                              title = "B. Males", 
                              legend_position = "None",
                              variable_of_interest = variable_of_interest)
plot_m

# men
df_f <- df_gp[, c("year", "assignvalid_appt_group", "n_female")] %>% rename(n = n_female)
plot_f <- gp_interaction_plot(dataset = df_f,
                              title = "A. Females", 
                              legend_position = "None",
                              variable_of_interest = variable_of_interest)
plot_f

plot_sex <- plot_grid(plot_f, plot_m, ncol = 2)
plot_sex

# simd --------------------------------------------------------------------------------------
# simd 1
df_simd_1 <- df_gp[, c("year", "assignvalid_appt_group", "n_SIMD1")] %>% rename(n = n_SIMD1)
plot_simd_1 <- gp_interaction_plot(dataset = df_simd_1,
                                  title = "A. Group 1 (Q1, most deprived)", 
                                  legend_position = "None",
                                  variable_of_interest = variable_of_interest)

# simd 2
df_simd_2 <- df_gp[, c("year", "assignvalid_appt_group", "n_SIMD2")] %>% rename(n = n_SIMD2)
plot_simd_2 <- gp_interaction_plot(dataset = df_simd_2,
                                  title = "B. Group 2 (Q2 to Q4)", 
                                  legend_position = "None",
                                  variable_of_interest = variable_of_interest)

# simd 3
df_simd_3 <- df_gp[, c("year", "assignvalid_appt_group", "n_SIMD3")] %>% rename(n = n_SIMD3)
plot_simd_3 <- gp_interaction_plot(dataset = df_simd_3,
                                  title = "C. Group 3 (Q5, least deprived)", 
                                  legend_position = "None",
                                  variable_of_interest = variable_of_interest)

plot_simd <- plot_grid(plot_simd_1, plot_simd_2, plot_simd_3, ncol = 2, nrow = 2)
plot_simd

# ethnicity --------------------------------------------------------------------------------------
# asian
df_a <- df_gp[, c("year", "assignvalid_appt_group", "n_asian")] %>% rename(n = n_asian)
plot_a <- gp_interaction_plot(dataset = df_a,
                                   title = "A. Asian", 
                                   legend_position = "None",
                                   variable_of_interest = variable_of_interest)

# black
df_b <- df_gp[, c("year", "assignvalid_appt_group", "n_black")] %>% rename(n = n_black)
plot_b <- gp_interaction_plot(dataset = df_b,
                                   title = "B. Black", 
                                   legend_position = "None",
                                   variable_of_interest = variable_of_interest)

# mixed
df_m <- df_gp[, c("year", "assignvalid_appt_group", "n_mixed")] %>% rename(n = n_mixed)
plot_m <- gp_interaction_plot(dataset = df_m,
                                   title = "C. Mixed", 
                                   legend_position = "None",
                                   variable_of_interest = variable_of_interest)

# white
df_w <- df_gp[, c("year", "assignvalid_appt_group", "n_white")] %>% rename(n = n_white)
plot_w <- gp_interaction_plot(dataset = df_w,
                              title = "D. White", 
                              legend_position = "None",
                              variable_of_interest = variable_of_interest)

# other
df_o <- df_gp[, c("year", "assignvalid_appt_group", "n_other")] %>% rename(n = n_other)
plot_o <- gp_interaction_plot(dataset = df_o,
                              title = "E. Other", 
                              legend_position = "None",
                              variable_of_interest = variable_of_interest)

plot_ethnicity <- plot_grid(plot_a, plot_b, plot_m, plot_w, plot_o, ncol = 2, nrow = 3)
plot_ethnicity



