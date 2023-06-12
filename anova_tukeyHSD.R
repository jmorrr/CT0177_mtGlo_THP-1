# ANOVAS and Tukey HSDs on data with an output df with category letters
# for eventual plotting.

aov_tukey <- function(data){
  # Convert 'concentration' column to factor
  data$concentration <- factor(data$concentration)
  # Empty dataframe which will become the dataframe with Sample, gene, and 
  # letters representing categories found from the TukeyHSD.  
  df_cat <- data.frame()
  one_way <- aov(response_value ~ concentration, data = data)
  print(summary(one_way))
  tukey_one_way = TukeyHSD(one_way)
  print(tukey_one_way)
  cats = multcompLetters4(one_way, tukey_one_way)
  cats <- as.data.frame(cats$concentration[1])
  cats$concentration <- rownames(cats)
  rownames(cats) <- c()
  df_cat <- rbind(df_cat, cats)
  return(df_cat)
}