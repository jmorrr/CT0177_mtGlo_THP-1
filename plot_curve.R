plot_curve <- function(data, IC_list, title, log = FALSE){
  
  if(log){
    
    dat_log2 <- data  
    dat_log2$concentration <- log2(dat_log2$concentration)
    pl_data <- dat_log2
    # Fit a four parameter logistic model to the data
    fit <- drm(response_value ~ concentration, data=dat_log2, fct=LL.3())
  
  }else{
    
    pl_data <- data
    # Fit a four parameter logistic model to the data
    fit <- drm(response_value ~ concentration, data=data, fct=LL.3())
  }
  
  # Extract the parameter estimates
  params <- coef(fit)
  print(summary(fit))
  
  # Calculate the inhibitory concentration values
  
  IC_values <- sapply(IC_list, function(i) ED(fit, i/100, interval = "none")[[1]])
  
  print("IC_values:")
  print(IC_values)
  
  if(log){
  logRev <- 2^IC_values
  print("Log Reverted IC_values:")
  print(logRev)
  }
  
  # Create a dataframe for plotting
  plot_data <- data.frame(concentration = seq(min(pl_data$concentration), max(pl_data$concentration), length.out = 100),
                          response_value = predict(fit, newdata = data.frame(concentration = seq(min(pl_data$concentration), max(pl_data$concentration), length.out = 100))))
  
  # Plot the data and the model
  p1 <- ggplot() +
    geom_point(data = pl_data, aes(x = concentration, y = response_value), color = "purple") +
    geom_line(data = plot_data, aes(x = concentration, y = response_value), size = 1.5) +
    geom_vline(xintercept = IC_values, color = "blue", size = 1) +
    xlab("Concentration") +
    ylab("Response") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1),
          axis.text = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20)) +
    ggtitle(title)+
    ylim(0, max(pl_data$response_value))
  
  print(p1)
  
  # Calculate mean response and standard deviation at each concentration
  mean_sd_data <- aggregate(response_value ~ concentration, pl_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
  mean_sd_data <- do.call(data.frame, mean_sd_data)
  names(mean_sd_data) <- c("concentration", "response_mean", "response_sd")
  
  p2 <-  ggplot(pl_data, aes(x = concentration, y = response_mean)) +
    geom_line(data = plot_data, aes(x = concentration, y = response_value), size = 1.5) +
    geom_vline(xintercept = IC_values, color = "blue", size = 1) +
    xlab("Concentration") +
    ylab("Response") +
    geom_errorbar(data = mean_sd_data, aes(x = concentration, ymin = response_mean - response_sd, ymax = response_mean + response_sd), size = 1) +
    geom_point(data = mean_sd_data, aes(x = concentration, y = response_mean), color = "purple", size = 5) +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black", size = 1),
          axis.text = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20)) +
    ggtitle(title)+
    ylim(0, max(pl_data$response_value))
  
  print(p2)
  
}                                      