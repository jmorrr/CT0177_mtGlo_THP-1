# A dose response curve analysis using the drc package to obtain
# and plot (inhibitory concentration) IC values e.g. IC50 which
# is the half maximal inhibitory concentration.
# Good link: https://rstudio-pubs-static.s3.amazonaws.com/378543_5b5bda32bf0541a485ccc49efbea680a.html

# Source the functions for plotting
source("plot_curve.R")
source("anova_tukeyHSD.R")
source("anov_plot.R")
# Load required libraries
library(multcompView)
library(drc)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(plater)

load("plate_map.Rdata")

empty_24H <- read_plate("empty_24H.csv")
colnames(empty_24H) <- c("well", "response")
empty_24H$well <- sub("([A-Za-z])(0)([1-9])", "\\1\\3", empty_24H$well)

cells_24H <- read_plate("cells_24H.csv")
colnames(cells_24H) <- c("well", "response")
cells_24H$well <- sub("([A-Za-z])(0)([1-9])", "\\1\\3", cells_24H$well)

empty_48H <- read_plate("empty_48H.csv")
colnames(empty_48H) <- c("well", "response")
empty_48H$well <- sub("([A-Za-z])(0)([1-9])", "\\1\\3", empty_48H$well)

cells_48H <- read_plate("cells_48H.csv")
colnames(cells_48H) <- c("well", "response")
cells_48H$well <- sub("([A-Za-z])(0)([1-9])", "\\1\\3", cells_48H$well)




df1 <- plate_map_concs
colnames(df1) <- c("well", "concentration")
df2 <- empty_24H
colnames(df2) <- c("well", "empty_24H")
df3 <- cells_24H
colnames(df3) <- c("well", "24H")
df4 <- empty_48H
colnames(df4) <- c("well", "empty_48H")
df5 <- cells_48H
colnames(df5) <- c("well", "48H")

df6 <- merge(df1, df2, by = 'well')
df6 <- merge(df6, df3, by = 'well')
df6 <- merge(df6, df4, by = 'well')
df6 <- merge(df6, df5, by = 'well')



# # Example: Adjust the 'response' by subtracting the mean 'empty_response' for each 'concentration' group if needed
# df <- df %>%
#   group_by(concentration) %>%
#   mutate(adjusted_response = response - mean(empty_response))

# This is a script that will set an example to process data from the Tecan
# plate reader. An example is given in the form of some cytotox data.
# The resultant df can then be used for plotting.
data <- df6
str(data)

# Define the mappings
concentration_values <- c("VC" = 0, "C1" = 800, "C2" = 400, "C3" = 200, "C4" = 100, "C5" = 50, "C6" = 25, "C7" = 12.5, "C8" = 6.25)

# Replace concentration values
data$concentration <- ifelse(data$concentration %in% names(concentration_values), 
                             concentration_values[data$concentration],
                             data$concentration)

data$concentration <- as.numeric(data$concentration)
str(data)


data_long <- data %>%
  pivot_longer(
    cols = 3:ncol(data),
    names_to = "response_type",
    values_to = "response_value"
  )

data_long %>%
  group_by(response_type) %>%
  do({
    data <- .
    data$response_value <- as.numeric(data$response_value)
    print(data$response_type[1])
    
    data$response_value <- (data$response_value / mean(data$response_value[which(data$concentration == 0)])) * 100
    
    anov_res <- aov_tukey(data)
    
    # Create an ANOVA boxplot of the statistical data
    title <- paste("THP-1_bleomycin", data$response_type[1])
    anov_plot(data, anov_res, title)
    
    # Define the list of IC values you want plotted
    IC_list <- c(20, 50)
    
    # Create a title for the graphs
    title <- paste("THP-1_bleomycin", data$response_type[1])
    plot_curve(data, IC_list, title, log = FALSE)
    
    data_L <- data
    data_L$concentration[data_L$concentration == 0] <- 1.1
    # Create a title for the graphs
    title <- paste("THP-1_bleomycin", data$response_type[1])
    plot_curve(data_L, IC_list, title, log = FALSE)
    
  })
