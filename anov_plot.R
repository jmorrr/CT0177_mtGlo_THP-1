anov_plot <- function(data, anov_res, title){
  
  df_stat <- merge(data, anov_res, by = "concentration")
  df_stat$concentration <- factor(df_stat$concentration)
  # Create a dataframe with unique combinations of concentration and Letters
  label_df <- df_stat[!duplicated(df_stat[c('concentration', 'Letters')]), c('concentration', 'Letters')]
  
  p1 <-  ggplot(df_stat, aes(x = concentration, y = response_value)) +
          geom_boxplot() +
          geom_jitter(width = 0.1, size = 1.5, color = "purple") + # adds individual data points
          geom_text(aes(x = concentration, y = 0, label = Letters), size = 8) +
          labs(x = "Concentration", y = "Response") +
          theme(panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 1),
                axis.text = element_text(size = 20, face = "bold"),
                axis.title = element_text(size = 20),
                plot.title = element_text(size = 20)) +
          ggtitle(title)+
          ylim(0, max(df_stat$response_value)) 
  print(p1)
}