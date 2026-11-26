
gp_interaction_plot <- function(dataset, title, variable_of_interest, legend_position = "right") { #fill_var = assign_appt_group or assignvalid_appt_group
  
  #remove unwanted tax years
  greyout <- c("2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019") #creates chr vector of unwanted years
  data <- dataset %>%
    filter(!year %in% greyout)
  
  data$variable_of_interest <- factor(data[, variable_of_interest])
  
  data_plot <- data %>%
    group_by(year) %>%
    mutate(prop = n / sum(n))
  
  ggplot(data_plot, aes(x = year, y = prop, fill = variable_of_interest))+
    geom_col() +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              color = "white",
              size = 4) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c(
      "ASSIGN Y, Appt Y" = "#313657",
      "ASSIGN N, Appt N" = "#1F6683",
      "ASSIGN N, Appt Y" = "#DD3C51",
      "ASSIGN Y, Appt N" = "#6C90B9"
    ), labels = c("ASSIGN Y, Appt Y" =  "ASSIGN risk score recorded and GP encounter",
                  "ASSIGN N, Appt N" =  "No ASSIGN risk score recorded and no GP encounter",
                  "ASSIGN N, Appt Y" =  "No ASSIGN risk score recorded, but GP encounter",
                  "ASSIGN Y, Appt N" =  "ASSIGN risk score recorded, but no GP encounter")) +
    labs(
      title = title,
      x = "Financial year",
      y = "Proportion (%)",
      fill = "Group"
    ) + 
    theme_bw(base_size = 16) +
    theme(legend.title = element_blank(),
          legend.position = legend_position,,
          legend.key.width = unit(1.1, "cm"),
          axis.title = element_text(size = 14),
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
}
