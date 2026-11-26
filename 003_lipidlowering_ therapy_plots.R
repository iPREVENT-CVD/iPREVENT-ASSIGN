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
library(flextable)
library(officer)


####################################################################################################################################
# Load data into R
####################################################################################################################################

# load functions
# helper_functions
# function_financial_year_plot
# function_df_financial_years

####################################################################################################################################
# Plot
####################################################################################################################################

df_plot$group <- as.factor(df_plot$group)
df_plot$group <- relevel(df_plot$group, "Other")
df_plot$group <- relevel(df_plot$group, "White")
df_plot$group <- relevel(df_plot$group, "Mixed")
df_plot$group <- relevel(df_plot$group, "Black")
df_plot$group <- relevel(df_plot$group, "Asian")
df_plot$group <- relevel(df_plot$group, "Group 3 (Q5, least deprived)")
df_plot$group <- relevel(df_plot$group, "Group 2 (Q2 to Q4)")
df_plot$group <- relevel(df_plot$group, "Group 1 (Q1, most deprived)")
df_plot$group <- relevel(df_plot$group, "Male")
df_plot$group <- relevel(df_plot$group, "Female")
df_plot$group <- relevel(df_plot$group, ">70 years")
df_plot$group <- relevel(df_plot$group, "50 to 70 years")
df_plot$group <- relevel(df_plot$group, "<50 years")
df_plot$group <- relevel(df_plot$group, "Total study population")

df_plot <- df_plot %>% filter(group != "Missing" & !is.na(group) & group != "Not stated")

df_long <- df_plot %>%
  mutate(prop_not_treated = 1 - prop_treated) %>%
  select(group, prop_treated, prop_not_treated) %>%
  pivot_longer(cols = c(prop_treated, prop_not_treated),
               names_to = "treatment_status",
               values_to = "prop") %>%
  mutate(treatment_status = recode(treatment_status,
                         prop_treated = "Yes",
                         prop_not_treated = "No"))

plot <- ggplot(df_long, aes(x = group, y = prop, fill = treatment_status)) +
  geom_col(width = 0.85) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white",
            size = 4) +
  geom_vline(xintercept = c(1.5, 4.5, 6.5, 9.5), linetype = "dashed") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Yes" = "#313657", "No" = "#DD3C51")) +
 labs(x = "",
       y = "Proportion (%)",
       fill = "Provision of lipid-lowering therapy within one year of ASSIGN score >20") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

plot


