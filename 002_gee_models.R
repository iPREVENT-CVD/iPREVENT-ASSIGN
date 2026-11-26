rm(list=ls())

####################################################################################################################################
# General information
####################################################################################################################################

# Information: gee logistic regression

####################################################################################################################################
# Packages
####################################################################################################################################

library(dplyr)
library(geepack)
library(ggplotify)
library(patchwork)

####################################################################################################################################
# Additional infomration
####################################################################################################################################

# dataset$cleaned <- paste0(format(round(dataset$odds_ratio, 2), nsmall = 2), " (", 
#                           format(round(dataset$lower_limit, 2), nsmall = 2), " to ",
#                           format(round(dataset$upper_limit, 2), nsmall = 2), ")")

####################################################################################################################################
# Load data into R
####################################################################################################################################

# load functions into R

# helper_functions
# function_fit_gee_mdel
# function_forest_plot

####################################################################################################################################
# Logistic regression models
####################################################################################################################################

# sex --------------------------------------------------------------------------------------------------------------------
df_sex <- list("Female versus male" = df_sex[2:5])
colnames(df_sex[[1]]) <- gsub("Female.versus.male.", "", colnames(df_sex[[1]]))

plot_sex <- forest_plot(dataset = df_sex, title = "A. Unadjusted")
plot_sex

# age groups --------------------------------------------------------------------------------------------------------------------
df_age <-  list("<50 years" = df_age[2:5], "50 to 70 years" = df_age[6:9])
colnames(df_age[[1]]) <- gsub("X.50.years.", "", colnames(df_age[[1]]))
colnames(df_age[[2]]) <- gsub("X50.to.70.years.versus..70.years.", "", colnames(df_age[[2]]))

plot_age <- forest_plot(dataset = df_age, title = "A. Unadjusted")
plot_age

# simd --------------------------------------------------------------------------------------------------------------------
df_simd <-  list("Group 1 (Q1, most deprived)" = df_simd[2:5], "Group 2 (Q2 to Q4)" = df_simd[6:9])
colnames(df_simd[[1]]) <- gsub("Group.1..Q1..most.deprived..", "", colnames(df_simd[[1]]))
colnames(df_simd[[2]]) <- gsub("Group.2..Q2.to.Q4..versus.group.3..Q5..least.deprived..", "", colnames(df_simd[[2]]))

plot_simd <- forest_plot(dataset = df_simd, title = "A. Unadjusted")
plot_simd

# ethnicity --------------------------------------------------------------------------------------------------------------------
df_eth <-  list("Asian" = df_eth[2:5], "Black" = df_eth[6:9], "Mixed" = df_eth[10:13], "Other" = df_eth[14:17])
colnames(df_eth[[1]]) <- gsub("Asian.", "", colnames(df_eth[[1]]))
colnames(df_eth[[2]]) <- gsub("Black.", "", colnames(df_eth[[2]]))
colnames(df_eth[[3]]) <- gsub("Mixed.", "", colnames(df_eth[[3]]))
colnames(df_eth[[4]]) <- gsub("Other.versus.White.", "", colnames(df_eth[[4]]))

plot_ethnicity <- forest_plot(dataset = df_eth, title = "A. Unadjusted", colors = c("#313657", "#DD3C51", "#1F6683", "#7C7189"))
plot_ethnicity

# rename------------------------------------------------------------------------------------------------
unadj_plot_sex <- plot_sex
unadj_plot_age <- plot_age
unadj_plot_simd <- plot_simd
unadj_plot_eth <- plot_ethnicity

####################################################################################################################################
# Adjusted logistic regression models
####################################################################################################################################
# sex --------------------------------------------------------------------------------------------------------------------
df_sex <- list("Female versus male" = df_sex[2:5])
colnames(df_sex[[1]]) <- gsub("Female.versus.male.", "", colnames(df_sex[[1]]))

plot_sex <- forest_plot(dataset = df_sex, title = "B. Sex")
plot_sex

# age groups --------------------------------------------------------------------------------------------------------------------
df_age <-  list("<50 years" = df_age[2:5], "50 to 70 years" = df_age[6:9])
colnames(df_age[[1]]) <- gsub("X.50.years.", "", colnames(df_age[[1]]))
colnames(df_age[[2]]) <- gsub("X50.to.70.years.versus..70.years.", "", colnames(df_age[[2]]))

plot_age <- forest_plot(dataset = df_age, title = "A. Age groups")
plot_age

# simd --------------------------------------------------------------------------------------------------------------------
df_simd <-  list("Group 1 (Q1, most deprived)" = df_simd[2:5], "Group 2 (Q2 to Q4)" = df_simd[6:9])
colnames(df_simd[[1]]) <- gsub("Group.1..Q1..most.deprived..", "", colnames(df_simd[[1]]))
colnames(df_simd[[2]]) <- gsub("Group.2..Q2.to.Q4..versus.group.3..Q5..least.deprived..", "", colnames(df_simd[[2]]))

plot_simd <- forest_plot(dataset = df_simd, title = "C. Socioeconomic status")
plot_simd

# ethnicity --------------------------------------------------------------------------------------------------------------------
df_eth <-  list("Asian" = df_eth[2:5], "Black" = df_eth[6:9], "Mixed" = df_eth[10:13], "Other" = df_eth[14:17])
colnames(df_eth[[1]]) <- gsub("Asian.", "", colnames(df_eth[[1]]))
colnames(df_eth[[2]]) <- gsub("Black.", "", colnames(df_eth[[2]]))
colnames(df_eth[[3]]) <- gsub("Mixed.", "", colnames(df_eth[[3]]))
colnames(df_eth[[4]]) <- gsub("Other.versus.White.", "", colnames(df_eth[[4]]))

plot_ethnicity <- forest_plot(dataset = df_eth, title = "D. Ethnicity", colors = c("#313657", "#DD3C51", "#1F6683", "#7C7189"))
plot_ethnicity

# rename------------------------------------------------------------------------------------------------
adj_plot_sex <- plot_sex
adj_plot_age <- plot_age
adj_plot_simd <- plot_simd
adj_plot_eth <- plot_ethnicity

# combine------------------------------------------------------------------------------------------------
g1 <- grid2grob(print(unadj_plot_age))
g2 <- grid2grob(print(adj_plot_age))

g3 <- grid2grob(print(unadj_plot_sex))
g4 <- grid2grob(print(adj_plot_sex))

g5 <- grid2grob(print(unadj_plot_simd))
g6 <- grid2grob(print(adj_plot_simd))

g7 <- grid2grob(print(unadj_plot_eth))
g8 <- grid2grob(print(adj_plot_eth))

# new version (figure 3) --------------------------------------------------------------------------------------------------------------

g1 <- grid2grob(print(adj_plot_age))
g2 <- grid2grob(print(adj_plot_sex))
g3 <- grid2grob(print(adj_plot_simd))
g4 <- grid2grob(print(adj_plot_eth))

