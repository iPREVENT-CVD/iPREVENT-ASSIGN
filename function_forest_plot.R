##################
### function 1 ###
##################

forest_plot <- function(dataset, title, colors = c("#DD3C51", "#313657", "#1F6683", "#6C90B9", "#CABEE9", "#7C7189", "#D1C7B5", "#FAE093")){
  
  library(forestplot)
  # combine all datasets into one
  df_all <- bind_rows(dataset, .id = "groups")
  
  # create wide format dataset 
  df_wide <- df_all %>% tidyr::pivot_wider(
    names_from = groups,
    values_from = c(odds_ratio, lower_limit, upper_limit))
  
  table_text <- cbind(c("Financial year", df_wide$year)) %>% as.data.frame()
  
  # built matrices required for forestplot
  groups <- unique(df_all$groups)
  
  mean_cols <- paste0("odds_ratio_", groups)
  lower_cols  <- paste0("lower_limit_", groups)
  upper_cols <- paste0("upper_limit_", groups)
  
  mean <- rbind(rep(NA, length(groups)),
                as.matrix(df_wide[, mean_cols]))
  
  lower <- rbind(rep(NA, length(groups)),
                 as.matrix(df_wide[, lower_cols]))
  
  upper <- rbind(rep(NA, length(groups)),
                 as.matrix(df_wide[, upper_cols]))
  
  graph_position = 2
  align_columns <- c("l")
  
  # additional plot components
  xticks <- (c(0.50, 0.75, 1.0, 1.50, 2.0, 3.0, 4.0))
  attr(xticks, "labels") <- c("0.5", "0.75", "1.0", "1.5", "2.0", "3.0", "4.0")
  
  if(length(groups) == 1){
    
    # plot
    forestplot(table_text,
               mean = mean, lower = lower, upper = upper,
               zero = 1,
               xlab = "Adjusted odds ratio (95%CI)",
               title = title,
               graph.pos = graph_position,
               align = align_columns,
               is.summary = c(T, rep(F, 15)),
               txt_gp = fpTxtGp(ticks = gpar(cex = 0.8),
                                xlab = gpar(cex = 1, fontface = "bold"),
                                legend = gpar(cex = 1)),
               col = fpColors(box = colors, line = colors),
               cex.main = 1.5,
               cex = 1.5,
               ci.vertices = T,
               colgap = unit(5, "mm"),
               clip = c(0.5, 4),
               xticks = xticks,
               boxsize = 0.3,
               xlog = T)
    
  } else {
    
    # plot
    forestplot(table_text,
               mean = mean, lower = lower, upper = upper,
               zero = 1,
               xlab = "Adjusted odds ratio (95%CI)",
               title = title,
               graph.pos = graph_position,
               align = align_columns,
               is.summary = c(T, rep(F, 15)),
               txt_gp = fpTxtGp(ticks = gpar(cex = 0.8),
                                xlab = gpar(cex = 1, fontface = "bold"),
                                legend = gpar(cex = 1)),
               col = fpColors(box = colors, line = colors),
               cex.main = 1.5,
               cex = 1.5,
               ci.vertices = T,
               colgap = unit(5, "mm"),
               clip = c(0.5, 4),
               xticks = xticks,
               boxsize = 0.3,
               xlog = T,
               legend = groups)
  
  }

  }
