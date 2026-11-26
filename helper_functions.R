
####################################################################################################################################
# Function 1
####################################################################################################################################

"%!in%" <- Negate("%in%")

####################################################################################################################################
# Function 2
####################################################################################################################################

pretty_histogram <- function(dataset, variable, stratified_by = NULL, bins = 50){
  
  library(ggplot2)
  dataset$variable <- dataset[, variable]
  
  if(is.null(stratified_by)){
    
    histogram <- ggplot(dataset, aes(x=variable)) + xlab(variable) + ylab("Frequeny") +
      geom_histogram(position = "dodge",  fill = "#D95F02", color = "#666666",  bins = bins, alpha = 0.5) +
      theme_minimal() + theme_classic() + theme(legend.position = "top", legend.title = element_blank())
    
  } else {
    
    dataset$stratified_by <- dataset[, stratified_by]
    nlevels <- dataset$stratified_by %>% unique() %>% length()
    all_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
                "#D53E4F", "#FEE08B", "#E6F598", "#ABDDA4", "#3288BD", "darkblue", "darkred")
    colors <- all_colors[1:nlevels]
    
    histogram <- ggplot(dataset, aes(x=variable, color=stratified_by, fill=stratified_by)) + xlab(variable) + ylab("Frequency") +
      geom_histogram(position = "identity", bins = bins, alpha = 0.2) +
      scale_color_manual(values=colors) + scale_fill_manual(values=colors) +
      theme_minimal() + theme_classic() + theme(legend.position = "top", legend.title = element_blank())
    
  }
  
  print(histogram)
  
}
