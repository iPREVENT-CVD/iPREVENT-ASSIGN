
##################
### function 1 ###
##################

tax_year_plot <- function(dataset,
                          variables_of_interest = "assign_score_valid",
                          legend_labels = "ASSIGN risk score",
                          stratified_by = NULL,
                          colors = c("#DD3C51", "#313657", "#1F6683", "#6C90B9", "#CABEE9", "#7C7189", "#D1C7B5", "#FAE093"),
                          line_types = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                          limits_yaxis = c(0, 1),
                          yaxis_breaks = 0.1,
                          label_yaxis = "Cardiovascular risk assessment recording",
                          title = "",
                          legend_position = "right",
                          activity_threshold = 3*30.437){
  
  # since we don't need to create the summary dataframe (input of function outside the safehaven)
  df_summary <- dataset
  
  # # preparations
  # dataset <- dataset %>% as.data.frame()
  # 
  # # create long format dataset to allow all variables to be presented in same plot
  # df_extra_long <- dataset %>% 
  #   pivot_longer(cols = all_of(variables_of_interest),
  #                names_to = "assessment_type",
  #                values_to = "assessment_in_year") %>% as.data.frame()
  # 
  if(is.null(stratified_by)){
  #   
  #   # ensure assessment_type is a factor in the order you want
  #   df_extra_long$assessment_type <- factor(df_extra_long$assessment_type, levels = variables_of_interest)
  # 
  #   # calculate proportions in each year
  #   df_summary <- df_extra_long %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
  #     group_by(year, assessment_type) %>% 
  #     summarise(n_eligible = n(),
  #               n_assessed = sum(assessment_in_year == "Yes", na.rm = T),
  #               prop_assessed = n_assessed / n_eligible,
  #               .groups = "drop") %>%
  #     # calculate confidence intervals using Wilson 95%CIs
  #     mutate(ci = binconf(n_assessed, n_eligible, method = "wilson"))
  #   
  #   df_summary$lower_limit <- df_summary$ci[,2]
  #   df_summary$upper_limit <- df_summary$ci[,3]
  #   
  #   df_summary$ci <- NULL
    
    # define colors and line types
    color_mapping <- setNames(colors[1:length(variables_of_interest)], variables_of_interest)
    line_mapping <- setNames(line_types[1:length(variables_of_interest)], variables_of_interest)
    label_mapping <- setNames(legend_labels, variables_of_interest)
  
    # plot
  plot <- ggplot(df_summary, aes(x = year, 
                                 y = prop_assessed,
                              color = assessment_type,
                              linetype = assessment_type,
                              group = assessment_type)) +
    geom_line(linewidth = 1.1) +
    geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit, fill = assessment_type), alpha = 0.2, color = NA) +
    theme_bw(base_size = 16) +
    labs(x = "",
         y = label_yaxis,
         title = title) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = limits_yaxis,
                       breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks)) +
    scale_color_manual(values = color_mapping, labels = label_mapping) +
    scale_fill_manual(values = color_mapping, labels = label_mapping) +
    scale_linetype_manual(values = line_mapping, labels = label_mapping) +
    theme(legend.title = element_blank(),
          legend.position = legend_position,
          legend.key.width = unit(1.1, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if(!is.null(stratified_by)){
    
    #  # ensure assessment_type and stratified_by are factors in the order you want
    # df_extra_long$assessment_type <- factor(df_extra_long$assessment_type, levels = variables_of_interest)
    # df_extra_long$stratified_by <- as.factor(df_extra_long[[stratified_by]])
    # 
    # # calculate proportions in each year
    # df_summary <- df_extra_long %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
    #   group_by(year, assessment_type, stratified_by) %>% 
    #   summarise(n_eligible = n(),
    #             n_assessed = sum(assessment_in_year == "Yes", na.rm = T),
    #             prop_assessed = n_assessed / n_eligible,
    #             .groups = "drop")%>%
    #   # calculate confidence intervals using Wilson 95%CIs
    #   mutate(ci = binconf(n_assessed, n_eligible, method = "wilson"))
    # 
    # df_summary$lower_limit <- df_summary$ci[,2]
    # df_summary$upper_limit <- df_summary$ci[,3]
    # 
    # df_summary$ci <- NULL
    
    # if stratified, the colors are based on the number of levels in stratification varaible
    n_levels <- nlevels(df_summary$stratified_by)
    color_mapping <- colors[1:n_levels]
    line_mapping <- setNames(line_types[1:length(variables_of_interest)], variables_of_interest)
    label_mapping <- setNames(legend_labels, variables_of_interest)
    
    # plot
   plot <- ggplot(df_summary, aes(x = year, 
                                  y = prop_assessed, 
                                  color = stratified_by, 
                                  linetype = assessment_type, 
                                  group = interaction(assessment_type, stratified_by, drop = T))) +
     geom_line(size = 1.1) +
     geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit, fill = stratified_by), alpha = 0.2, color = NA) +
     theme_bw(base_size = 16) +
     labs(x = "",
          y = label_yaxis,
          title = title) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                        limits = limits_yaxis,
                        breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks)) +
     scale_color_manual(values = color_mapping) +
     scale_fill_manual(values = color_mapping) +
     scale_linetype_manual(values = line_mapping, 
                           labels = label_mapping) +
     theme(legend.title = element_blank(),
           legend.position = legend_position,
           plot.title = element_text(hjust = 0.5, face = "bold"),
           axis.text.x = element_text(angle = 45, hjust = 1)) +
     guides(linetype = "none")

  }
  
  return(list(plot, df_summary))
}



##################
### function 2 ###
##################

tax_year_plot_eligibility <- function(dataset,
                          variables_of_interest = "assign_score_valid",
                          legend_labels = "ASSIGN risk score",
                          stratified_by = NULL,
                          colors = c("#DD3C51", "#313657", "#1F6683", "#6C90B9", "#CABEE9", "#7C7189", "#D1C7B5", "#FAE093"),
                          line_types = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                          limits_yaxis = c(0, 1),
                          yaxis_breaks = 0.1,
                          yaxis_gap = NULL,
                          label_yaxis = "Number of individuals",
                          title = "",
                          legend_position = "right",
                          activity_threshold = 3*30.437){
  
  # since we don't need to create the summary dataframe (input of function outside the safehaven)
  df_summary <- dataset
  
  # # preparations
  # dataset <- dataset %>% as.data.frame()
  # 
  # # create long format dataset to allow all variables to be presented in same plot
  # df_extra_long <- dataset %>% 
  #   pivot_longer(cols = all_of(variables_of_interest),
  #                names_to = "assessment_type",
  #                values_to = "assessment_in_year") %>% as.data.frame()
  # 
  if(is.null(stratified_by)){
    
    # # ensure assessment_type is a factor in the order you want
    # df_extra_long$assessment_type <- factor(df_extra_long$assessment_type, levels = variables_of_interest)
    # 
    # # calculate proportions in each year
    # df_summary <- df_extra_long %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
    #   group_by(year, assessment_type) %>% 
    #   summarise(n_eligible = n(),
    #             n_assessed = sum(assessment_in_year == "Yes", na.rm = T))
    
    # define colors and line types
    color_mapping <- setNames(colors[1:length(variables_of_interest)], variables_of_interest)
    line_mapping <- setNames(line_types[1:length(variables_of_interest)], variables_of_interest)
    label_mapping <- setNames(legend_labels, variables_of_interest)
    
    # plot
    plot <- ggplot(df_summary, aes(x = year, 
                                   y = n_eligible,
                                   color = assessment_type,
                                   linetype = assessment_type,
                                   group = assessment_type)) +
      geom_line(linewidth = 1.1) +
      theme_bw(base_size = 16) +
      labs(x = "Financial year",
           y = label_yaxis,
           title = title) +
      scale_y_continuous(labels = label_number(),
                         limits = limits_yaxis,
                         breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks)) +
      scale_color_manual(values = color_mapping, labels = label_mapping) +
      scale_fill_manual(values = color_mapping, labels = label_mapping) +
      scale_linetype_manual(values = line_mapping, labels = label_mapping) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            legend.key.width = unit(1.1, "cm"),
            plot.title = element_text(, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    if(!is.null(yaxis_gap)){plot <- plot + scale_y_break(c(yaxis_gap[1], yaxis_gap[2]))}
    
  } else if(!is.null(stratified_by)){
    
    # # ensure assessment_type and stratified_by are factors in the order you want
    # df_extra_long$assessment_type <- factor(df_extra_long$assessment_type, levels = variables_of_interest)
    # df_extra_long$stratified_by <- as.factor(df_extra_long[[stratified_by]])
    # 
    # # calculate proportions in each year
    # df_summary <- df_extra_long %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
    #   group_by(year, assessment_type, stratified_by) %>% 
    #   summarise(n_eligible = n(),
    #             n_assessed = sum(assessment_in_year == "Yes", na.rm = T))
    
    # if stratified, the colors are based on the number of levels in stratification varaible
    n_levels <- nlevels(df_summary$stratified_by)
    color_mapping <- colors[1:n_levels]
    line_mapping <- setNames(line_types[1:length(variables_of_interest)], variables_of_interest)
    label_mapping <- setNames(legend_labels, variables_of_interest)
    
    # plot
    plot <- ggplot(df_summary, aes(x = year, 
                                   y = n_eligible, 
                                   color = stratified_by, 
                                   linetype = assessment_type, 
                                   group = interaction(assessment_type, stratified_by, drop = T))) +
      geom_line(size = 1.1) +
      theme_bw(base_size = 16) +
      labs(x = "Financial year",
           y = label_yaxis,
           title = title) +
      scale_y_continuous(labels = label_number(),
                         limits = limits_yaxis,
                         breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks)) +
      scale_color_manual(values = color_mapping) +
      scale_fill_manual(values = color_mapping) +
      scale_linetype_manual(values = line_mapping, 
                            labels = label_mapping) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            plot.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(linetype = "none")
  
    
    if(stratified_by == "ethnicity"){
      
      plot_1 <- ggplot(df_summary, aes(x = year, 
                                     y = n_eligible, 
                                     color = stratified_by, 
                                     linetype = assessment_type, 
                                     group = interaction(assessment_type, stratified_by, drop = T))) +
        geom_line(size = 1.1) +
        theme_bw(base_size = 16) +
        labs(x = "Financial year",
             y = label_yaxis,
             title = title) +
        scale_y_continuous(labels = label_number(),
                           limits = limits_yaxis,
                           breaks = seq(limits_yaxis[1], limits_yaxis[2], 2500)) +
        scale_color_manual(values = color_mapping) +
        scale_fill_manual(values = color_mapping) +
        scale_linetype_manual(values = line_mapping, 
                              labels = label_mapping) +
        theme(legend.title = element_blank(),
              legend.position = legend_position,
              plot.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.margin = margin(b = 0, t = 0)) +
        guides(linetype = "none",
               color = guide_legend(nrow = 1)) +
        coord_cartesian(ylim = c(0, 10000))
      
      plot_2 <- ggplot(df_summary, aes(x = year, 
                                       y = n_eligible, 
                                       color = stratified_by, 
                                       linetype = assessment_type, 
                                       group = interaction(assessment_type, stratified_by, drop = T))) +
        geom_line(size = 1.1) +
        theme_bw(base_size = 16) +
        labs(x = "Financial year",
             y = label_yaxis,
             title = title) +
        scale_y_continuous(labels = label_number(),
                           limits = limits_yaxis,
                           breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks)) +
        scale_color_manual(values = color_mapping) +
        scale_fill_manual(values = color_mapping) +
        scale_linetype_manual(values = line_mapping, 
                              labels = label_mapping) +
        theme(legend.title = element_blank(),
              legend.position = "none",
              plot.title = element_text(face = "bold"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title = element_blank(),
              plot.margin = margin(b = 0)) +
        guides(linetype = "none") +
        coord_cartesian(ylim = c(140000, 300000))
    
      plot <- plot_2/plot_1 + plot_layout(heights = c(1, 1.5))
      
      }
    
    
  }
  
  return(list(plot, df_summary))
}


##################
### function 3 ###
##################

tax_year_plot_elevated <- function(dataset,
                                  variable_of_interest = "assign_score_elevated_20",
                                  legend_labels = c("Number of individuals with recording of ASSIGN score",
                                                    "Proportion of individuals with ASSIGN risk score >20"),
                                  stratified_by = NULL,
                                  colors = c("#DD3C51", "#313657", "#1F6683", "#6C90B9", "#CABEE9", "#7C7189", "#D1C7B5", "#FAE093"),
                                  line_types = c("solid", "dashed"),
                                  limits_yaxis = c(0, 1),
                                  yaxis_breaks = 0.1,
                                  label_yaxis = c("Proportion of individuals (%)",
                                                  "Number of individuals"),
                                  title = "",
                                  legend_position = "right",
                                  activity_threshold = 3*30.437){
  
  # preparations
  dataset <- dataset %>% as.data.frame()
  dataset$variable_of_interest <- dataset[, variable_of_interest]
  
  if(is.null(stratified_by)){
    
    # calculate proportions in each year
    df_summary <- dataset %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
      group_by(year) %>% 
      summarise(n_assessed = n(),
                n_elevated = sum(variable_of_interest == "Yes", na.rm = T),
                prop_elevated = n_elevated / n_assessed,
                .groups = "drop") %>%
      # calculate confidence intervals using Wilson 95%CIs
      mutate(ci = binconf(n_elevated, n_assessed, method = "wilson"))
    
    df_summary$lower_limit <- df_summary$ci[,2]
    df_summary$upper_limit <- df_summary$ci[,3]
    
    df_summary$ci <- NULL
    
    # to plot two variables with different units in the same plot, we need to scale them
    scale_factor <- max(df_summary$n_assessed) / max(df_summary$prop_elevated)
    
    # create long format dataset for plot (to allow legends)
    df_plot <- df_summary %>%
      select(year, prop_elevated, n_assessed) %>%
      pivot_longer(cols = c(prop_elevated,  n_assessed),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(value_scaled = ifelse(variable == "n_assessed",
                                   value / scale_factor,
                                   value))
    
    # plot
    plot <- ggplot(df_plot, aes(x = year)) +
      geom_line(aes(y = value_scaled, color = variable, group = variable), linewidth = 1.1) +
      geom_ribbon(data = df_summary, aes(ymin = lower_limit, ymax = upper_limit, group = 1), alpha = 0.2, fill = colors[1]) +
      theme_bw(base_size = 16) +
      labs(x = "Financial year",
           title = title) +
      scale_y_continuous(name = paste0(label_yaxis[1]),
                         labels = scales::percent_format(accuracy = 1),
                         limits = limits_yaxis,
                         breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks),
                         sec.axis = sec_axis(~. * scale_factor,
                                             name = paste0(label_yaxis[2]))) +
      scale_color_manual(values = c("prop_elevated" = colors[1],
                                    "n_assessed" = "grey70"),
                         labels = legend_labels) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            legend.key.width = unit(1.1, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if(!is.null(stratified_by)){
    
    # ensure  stratified_by are factors in the order you want
    dataset$stratified_by <- as.factor(dataset[[stratified_by]])
    
    # calculate proportions in each year
    df_summary <- dataset %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
      group_by(year, stratified_by) %>% 
      summarise(n_assessed = n(),
                n_elevated = sum(variable_of_interest == "Yes", na.rm = T),
                prop_elevated = n_elevated / n_assessed,
                .groups = "drop") %>%
      # calculate confidence intervals using Wilson 95%CIs
      mutate(ci = binconf(n_elevated, n_assessed, method = "wilson"))
    
    df_summary$lower_limit <- df_summary$ci[,2]
    df_summary$upper_limit <- df_summary$ci[,3]
    
    df_summary$ci <- NULL
    
    # to plot two variables with different units in the same plot, we need to scale them
    scale_factor <- max(df_summary$n_assessed) / max(df_summary$prop_elevated)
    
    # create long format dataset for plot (to allow legends)
    df_plot <- df_summary %>%
      select(year, prop_elevated, n_assessed, stratified_by) %>%
      pivot_longer(cols = c(prop_elevated,  n_assessed),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(value_scaled = ifelse(variable == "n_assessed",
                                   value / scale_factor,
                                   value))
    
    # if stratified, the colors are based on the number of levels in stratification varaible
    n_levels <- nlevels(df_summary$stratified_by)
    color_mapping <- colors[c(1:n_levels)]
    line_mapping <- setNames(line_types, c("prop_elevated", "n_assessed"))
    label_mapping <- setNames(legend_labels, c("n_assessed", "prop_elevated"))
    
    # plot
    plot <- ggplot(df_plot, aes(x = year, y = value_scaled, 
                                color = stratified_by, linetype = variable, 
                                group = interaction(stratified_by, variable))) +
      geom_line(linewidth = 1.1) +
      geom_ribbon(data = df_summary, 
                  aes(x = year, ymin = lower_limit, ymax = upper_limit, fill = stratified_by, group = stratified_by), 
                  alpha = 0.2, inherit.aes = FALSE) +
      theme_bw(base_size = 16) +
      labs(x = "Financial year",
           title = title) +
      scale_y_continuous(name = paste0(label_yaxis[1]),
                         labels = scales::percent_format(accuracy = 1),
                         limits = limits_yaxis,
                         breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks),
                         sec.axis = sec_axis(~. * scale_factor,
                                             name = paste0(label_yaxis[2]))) +
      scale_color_manual(values = color_mapping) +
      scale_fill_manual(values = color_mapping) +
      scale_linetype_manual(values = line_mapping, 
                            labels = label_mapping) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            legend.key.width = unit(1.1, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(linetype = "none")
    
  }
  
  return(list(plot, df_summary))
  
}



##################
### function 4 ###
##################

tax_year_plot_treated <- function(dataset,
                          variable_of_interest = "treated_within_year",
                          legend_labels = c("Number of individuals with ASSIGN risk score > 20",
                                            "Proportion with prescription within 1 year"),
                          stratified_by = NULL,
                          colors = c("#DD3C51", "#313657", "#1F6683", "#6C90B9", "#CABEE9", "#7C7189", "#D1C7B5", "#FAE093"),
                          line_types = c("solid", "dashed"),
                          limits_yaxis = c(0, 1),
                          yaxis_breaks = 0.1,
                          label_yaxis = c("Prescription within 1 year (%)",
                                          "Number of individuals"),
                          title = "",
                          legend_position = "right",
                          activity_threshold = 3*30.437){
  
  # preparations
  dataset <- dataset %>% as.data.frame()
  dataset$variable_of_interest <- dataset[, variable_of_interest]
  
  if(is.null(stratified_by)){
    
    # calculate proportions in each year
    df_summary <- dataset %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
      group_by(year) %>% 
      summarise(n_elevated = n(),
                n_treated = sum(variable_of_interest == "Yes", na.rm = T),
                prop_treated = n_treated / n_elevated,
                .groups = "drop") %>%
      # calculate confidence intervals using Wilson 95%CIs
      mutate(ci = binconf(n_treated, n_elevated, method = "wilson"))
    
    df_summary$lower_limit <- df_summary$ci[,2]
    df_summary$upper_limit <- df_summary$ci[,3]
    
    df_summary$ci <- NULL
    
    # to plot two variables with different units in the same plot, we need to scale them
    scale_factor <- max(df_summary$n_elevated) / max(df_summary$prop_treated)

    # create long format dataset for plot (to allow legends)
    df_plot <- df_summary %>% filter(year %!in% "2024-2025") %>%
      select(year, prop_treated, n_elevated) %>%
      pivot_longer(cols = c(prop_treated,  n_elevated),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(value_scaled = ifelse(variable == "n_elevated",
                                   value / scale_factor,
                                   value))
    
    # plot
    plot <- ggplot(df_plot, aes(x = year)) +
      geom_line(aes(y = value_scaled, color = variable, group = variable), linewidth = 1.1) +
      geom_ribbon(data = df_summary[df_summary$year %!in% "2024-2025", ], aes(ymin = lower_limit, ymax = upper_limit, group = 1), alpha = 0.2, fill = colors[1]) +
      theme_bw(base_size = 16) +
      labs(x = "Financial year",
           title = title) +
      scale_y_continuous(name = paste0(label_yaxis[1]),
                         labels = scales::percent_format(accuracy = 1),
                         limits = limits_yaxis,
                         breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks),
                         sec.axis = sec_axis(~. * scale_factor,
                                             name = paste0(label_yaxis[2]))) +
      scale_color_manual(values = c("prop_treated" = colors[1],
                                    "n_elevated" = "grey70"),
                         labels = legend_labels) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            legend.key.width = unit(1.1, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else if(!is.null(stratified_by)){
    
    # ensure  stratified_by are factors in the order you want
    dataset$stratified_by <- as.factor(dataset[[stratified_by]])
    
    # calculate proportions in each year
    df_summary <- dataset %>% filter(eligible_in_year == "Yes" & activity_in_year >= activity_threshold) %>%
      group_by(year, stratified_by) %>% 
      summarise(n_elevated = n(),
                n_treated = sum(variable_of_interest == "Yes", na.rm = T),
                prop_treated = n_treated / n_elevated,
                .groups = "drop") %>%
      # calculate confidence intervals using Wilson 95%CIs
      mutate(ci = binconf(n_treated, n_elevated, method = "wilson"))
    
    df_summary$lower_limit <- df_summary$ci[,2]
    df_summary$upper_limit <- df_summary$ci[,3]
    
    df_summary$ci <- NULL
    
    # to plot two variables with different units in the same plot, we need to scale them
    scale_factor <- max(df_summary$n_elevated) / max(df_summary$prop_treated)
    
    # create long format dataset for plot (to allow legends)
    df_plot <- df_summary %>% filter(year %!in% "2024-2025") %>%
      select(year, prop_treated, n_elevated, stratified_by) %>%
      pivot_longer(cols = c(prop_treated,  n_elevated),
                   names_to = "variable",
                   values_to = "value") %>%
      mutate(value_scaled = ifelse(variable == "n_elevated",
                                   value / scale_factor,
                                   value))
    
    # if stratified, the colors are based on the number of levels in stratification varaible
    n_levels <- nlevels(df_summary$stratified_by)
    color_mapping <- colors[c(1:n_levels)]
    line_mapping <- setNames(line_types, c("prop_treated", "n_elevated"))
    label_mapping <- setNames(legend_labels, c("n_elevated", "prop_treated"))
    
    # plot
    plot <- ggplot(df_plot, aes(x = year, y = value_scaled, 
                                color = stratified_by, linetype = variable, 
                                group = interaction(stratified_by, variable))) +
      geom_line(linewidth = 1.1) +
      geom_ribbon(data = df_summary[df_summary$year %!in% "2024-2025", ], 
                  aes(x = year, ymin = lower_limit, ymax = upper_limit, fill = stratified_by, group = stratified_by), 
                  alpha = 0.2, inherit.aes = FALSE) +
      theme_bw(base_size = 16) +
      labs(x = "Financial year",
           title = title) +
      scale_y_continuous(name = paste0(label_yaxis[1]),
                         labels = scales::percent_format(accuracy = 1),
                         limits = limits_yaxis,
                         breaks = seq(limits_yaxis[1], limits_yaxis[2], yaxis_breaks),
                         sec.axis = sec_axis(~. * scale_factor,
                                             name = paste0(label_yaxis[2]))) +
      scale_color_manual(values = color_mapping) +
      scale_fill_manual(values = color_mapping) +
      scale_linetype_manual(values = line_mapping, 
                            labels = label_mapping) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            legend.key.width = unit(1.1, "cm"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(linetype = "none")
    
  }
  
  return(list(plot, df_summary))
  
}

