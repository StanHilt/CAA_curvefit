setwd("R:/Eva Iliopoulou/UCPLFCAA/Data")

#----------- Install Packages ------

install.packages("readxl")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("naniar")
install.packages("janitor")
install.packages("zoo")
install.packages("DescTools")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cowplot")

library(readxl)
library(openxlsx)
library(tidyverse)
library(naniar)
library(janitor)
library(zoo)
library(DescTools)
library(dplyr)
library(ggplot2)
library(cowplot)

#----------- Manual Input ------

file_name <- "UPC-LF CAA 20 strips_SCAA500 USING OLD UCP VIAL 12072023.xlsx"
test_samples <- 11 #samples that are NOT part of the standard curve

#----------- Load Data -------

df <- read_excel(file_name, sheet = "Well results",
                 col_names = FALSE)

#----------- Load Functions  ------


#extract measurement data to detect peaks

df_peaks <- function(x) {
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)] 
  df_m <- t(df_m)
}

#extract strip line numbers/position

extract_stripline <- function(x) { 
  strip_line <- x %>%
    select(-c(2:12)) %>%
    filter(...1 == "Strip 1") %>%
    select(-1) %>%
    t() %>%
    as.numeric() %>%
    as.vector() %>%
    t()
}

#create tidy data frame

reshape_df <- function(x, y) {    #x is the dataframe, y is the strip line numbers, I still have to make this more tidy
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)]  
  df_m[nrow(df_m) + 1,] <- y
  df_m <- t(df_m)
  df_m <- as_tibble(df_m)
  n_position <- ncol(df_m) - 1
  df_long <- pivot_longer(df_m, cols = 1:n_position, 
                          names_to = "strip", 
                          values_to = "measurement")
  df_long <- arrange(df_long, strip)
  last_col <- paste("V", ncol(df_m), sep = "")
  df_long <- df_long %>% 
    rename("position" = last_col) 
  df_long$strip <- gsub("V", "", df_long$strip) %>%
    as.numeric(df_long$strip)
  print(df_long)
}

#create tidy dataframe with smooth data 

reshape_df_smooth <- function(x, y) { #x = dataframe of smooth peaks, y = strip line 
  strip_line_smooth <- y
  strip_line_smooth <- as.tibble(strip_line_smooth)
  peaks_smooth <- cbind(x, strip_line_smooth)
  peaks_smooth <- as_tibble(peaks_smooth)
  n_position <- ncol(peaks_smooth) - 1
  df_long_smooth <- pivot_longer(peaks_smooth, cols = 1:n_position, 
                                 names_to = "strip", 
                                 values_to = "measurement")
  df_long_smooth <- arrange(df_long_smooth, strip)
  df_long_smooth <- df_long_smooth %>% 
    rename("position" = "value")
  df_long_smooth$strip <- gsub("X", "", df_long_smooth$strip) %>%
    as.numeric(df_long_smooth$strip)
  print(df_long_smooth)
}

#reorder each matrix on the peak list so that the test peak is expressed first

order_peak_matrix <- function(matrix) {
  if (matrix[1, 2] > matrix[2, 2]) {
    matrix[c(2, 1), ]
  } else {
    matrix
  }
}

#extract peak calculations from peak list and make dataframe and reorganize

extract_peak_data <- function(x) {
  # Create an empty tibble with desired column names
  test_df <- tibble(
    height_T = NA, 
    max_T = NA, 
    start_T = NA, 
    end_T = NA,
    height_C = NA, 
    max_C = NA, 
    start_C = NA, 
    end_C = NA
  )
  
  # Iterate over the list of dataframes
  for (i in seq_along(x)) {
    # Get the current dataframe from the list
    current_df <- x[[i]]
    
    # Calculate the row index for the current dataframe
    row_index <- (i - 1) * 2 + 1
    
    # Assign values from the current dataframe to corresponding columns in test_df
    test_df[row_index, "height_T"] <- current_df[1, 1]
    test_df[row_index, "max_T"] <- current_df[1, 2]
    test_df[row_index, "start_T"] <- current_df[1, 3]
    test_df[row_index, "end_T"] <- current_df[1, 4]
    test_df[row_index + 1, "height_C"] <- current_df[2, 1]
    test_df[row_index + 1, "max_C"] <- current_df[2, 2]
    test_df[row_index + 1, "start_C"] <- current_df[2, 3]
    test_df[row_index + 1, "end_C"] <- current_df[2, 4]
  }
  
  
  # Group the rows in test_df by pairs
  test_df <- test_df %>%
    group_by(grp = (row_number() - 1) %/% 2) %>%
    summarize_all(~if (is.numeric(.)) {
      sum(., na.rm = TRUE)
    } else {
      first(.)
    }) %>%
    ungroup() %>%
    select(-grp)
}
merge_peak_data <- function(x, y){ #x = the tidy dataframe, y = the dataframe with the peaks data calculations (start, max ,end) 
  df_merged <- x %>% 
    select(-c(position, measurement))
  df_merged <- distinct(df_merged)
  df_merged <- cbind(df_merged, y)
}

#calculate area under the curve of the Test and Control peaks

auc_calculation <- function(x, y, z, peak) {   #x = the df with the smooth measurements, y = the df with the peak data (start, max, end), z = the strip line, peak = either "T" for test or "C"for control peak
  # Step 1: Iterate through the dataframe and store values for where the peak starts and ends
  a_col <- paste("start_", peak, sep = "")
  b_col <- paste("end_", peak, sep = "")
  
  a_vector <- y[, a_col]
  b_vector <- y[, b_col]
  
  # Step 2: Execute a function for each a and b in the second dataframe and store result c
  c_vector <- vector()
  
  for (col_index in 1:length(colnames(x))) {
    a <- a_vector[col_index]
    b <- b_vector[col_index]
    
    # Get the column name in peaks_smooth corresponding to the current column index
    col <- colnames(x)[col_index]
    
    # Perform auc calculations
    c <- AUC(x = z, y = x[[col]], from = z[a], to = z[b], 
             absolutearea = TRUE, method = "trapezoid")
    c_vector <- append(c_vector, c)
  }
  
  return(c_vector)
}

#----------- Reshape Data from Reader Output -------

peaks <- df_peaks(df) 

strip_line <- extract_stripline(df)
df_tidy <- reshape_df(df, strip_line) # create tidy dataframe with raw data 

#----------- Smooth Peak Data -----

p <- 5 #choose p factor for the rollmean function

peaks_smooth <- data.frame(matrix(nrow=96,ncol=ncol(peaks))) #make empty dataframe to fill in with the smooth data, length depending on the p factor

for (i in seq_along(peaks)) {
  output <- rollmean(peaks[,i], p)
  peaks_smooth[,i] <- output
}

remove(i, output, p)

#create tidy dataframe with smooth data 
length(strip_line)
strip_line <- strip_line[-c(97:100)] #the bigger the p, the more points along the measurement are "lost"
df_tidy_smooth <- reshape_df_smooth(peaks_smooth, strip_line)

df_tidy_smooth <- df_tidy_smooth %>%
  mutate(sample_id = if_else(strip %in% test_samples, "test_sample", "standard")) %>%
  arrange(strip) %>%
  unite(sample_id, strip, col = "sample_name", remove = FALSE) 

remove(test_samples)

df_tidy_smooth$caa <- NA
df_tidy_smooth <- df_tidy_smooth %>%  #Should this go into manual input?
  mutate(caa = case_when(
    sample_name == "standard_1" ~ 1000,
    sample_name == "standard_2" ~ 316,
    sample_name == "standard_3" ~ 100,
    sample_name == "standard_4" ~ 31.6,
    sample_name == "standard_5" ~ 10,
    sample_name == "standard_6" ~ 3.16,
    sample_name == "standard_7" ~ 1,
    sample_name == "standard_8" ~ 0.3,
    sample_name == "standard_9" ~ 0,
    sample_name == "standard_10" ~ 0,
#        sample_name == "standard_11" ~ 1000,
 #      sample_name == "standard_12" ~ 316,
  #    sample_name == "standard_13" ~ 100,
   #  sample_name == "standard_14" ~ 31.6,
    #sample_name == "standard_15" ~ 10,
     # sample_name == "standard_17" ~ 1,
      #sample_name == "standard_18" ~ 0.3,
#     sample_name == "standard_19" ~ 0,
 #   sample_name == "standard_20" ~ 0,
    TRUE ~ NA  
  ))

#visualize all smooth and raw peaks overlaying to compare

pdf_smoothpeaks_name <- paste0("raw_vs_smooth_peaks", file_name, ".pdf")

pdf(pdf_smoothpeaks_name) #I have to fix this to automatically take the file_name

peaks_graph_smooth <- ggplot2::ggplot() + 
  ggplot2::geom_line(data = df_tidy_smooth, 
                     mapping = aes(x = position, y = measurement, color = "Smooth Data"),
                     group = 1, linetype = "solid", linewidth = 0.3) +
  ggplot2::geom_line(data = df_tidy, 
                     mapping = aes(x = position, y = measurement, color = "Raw Data"),
                     group = 1, linetype = "solid", linewidth = 0.1) +
  ggplot2::facet_wrap(~ strip) +
  ggplot2::theme_light() +
  ggplot2::labs(x = "Position in Strip (mm)", 
                y = "Smooth Signal Intensity") +
  ggplot2::scale_color_manual(values = c("black", "red"),
                              labels = c("Raw Data", "Smooth Data"))

print(peaks_graph_smooth)
dev.off()


#----------- Peak Detection in Smooth Data ------

peaks_smooth <- as.data.frame(peaks_smooth) #or dont make it a tibble earlier, [,i] doesnt work (I still have to arrange this)

peaks_list <- vector("list", ncol(peaks_smooth))
for (i in seq_along(peaks_smooth)) {
  output <- pracma::findpeaks(peaks_smooth[,i], 
                              minpeakdistance = 20, 
                              nups = 3, ndowns = 2, 
                              npeaks = 2,
                              sortstr = FALSE)
  peaks_list[[i]] <- output
}

for (i in seq_along(peaks_list)) { #optional, to quickly check the peaks 
  print(peaks_list[[i]])
}


remove(i, output)

# Reorder each matrix of the peak list so that the test peak is expressed first
peaks_list <- lapply(peaks_list, order_peak_matrix)

#----------- Check All Graphs with Peak Points -------

pdf_peaks_name <- paste0("peaks_", file_name, ".pdf")

pdf(pdf_peaks_name) 

plots_list <- list()

for (i in 1:ncol(peaks_smooth)) {
  # Take values from the peaks_list for the current number 
  peak_test <- peaks_list[[i]]
  
  # Separate the dataframe containing the group with the corresponding number
  df_test <- filter(df_tidy_smooth, 
                    strip == i)
  # Convert peak_test and peak_points to simple vectors
  peak_test <- as.vector(peak_test)
  
  # Create a plot for the current number and store it in the list
  p <- ggplot(df_test, aes(x = position, 
                           y = measurement)) +
    geom_line(linewidth = 0.5) +
    geom_point(data = df_test[peak_test, ], 
               color = "red",
               size = 1.2) +
    labs(title = paste("strip", i),
         x = "Strip Position",
         y = "Smooth Signal Intensity",
         col = NULL) +
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 13,
                                    colour = "black"),
          axis.title.x = element_text(color = "gray37",
                                      size = 9),
          axis.title.y = element_text(colour = "gray37",
                                      size = 9),
          panel.background = element_rect(fill = 'white', color = 'gray37')) +
    scale_y_continuous(labels = scales::scientific,
                       limits = c(0,max(df_tidy_smooth$measurement) + 10000)) 
  
  plots_list[[i]] <- p
  
}
all_peaks_grid <- plot_grid(plotlist = plots_list, ncol = 4) #option 1: all peaks in a grid
#print(all_peaks_grid)

for (p in plots_list) { #option 2: each peak in a different pdf page
  print(p)
}

dev.off()

remove(i, df_test, peak_test, p)

#extract peak data from generated peak list and reorganize:

peaks_df <- extract_peak_data(peaks_list)

peaks_data <- merge_peak_data(df_tidy_smooth, peaks_df)


remove(peaks_df)

#----------- Calculate AUC for Each Peak (T and C)-------


auc_T <- auc_calculation(x = peaks_smooth, 
                         y = peaks_data, 
                         z = strip_line, 
                         peak = "T")
auc_C <- auc_calculation(x = peaks_smooth, 
                         y = peaks_data, 
                         z = strip_line, 
                         peak = "C")

peaks_data <- cbind(peaks_data, auc_T, auc_C)

peaks_data$"T/C" <- peaks_data$auc_T/peaks_data$auc_C

peaks_data_export <- select(peaks_data, sample_name, 
                            strip, caa, auc_T, auc_C, `T/C`)

peaks_data_filename <- paste0("peaks_data_", file_name, ".xlsx")

write.xlsx(peaks_data_export, peaks_data_filename)

remove(df, df_tidy, df_tidy_smooth, peaks, peaks_list, 
       peaks_smooth, strip_line, auc_C, auc_T)


#------------ Standard Curve without Fitting --------

#quickly check standard curve without fitting

pdf_curve_name <- paste0("unfitted_curve_", file_name, ".pdf")

pdf(pdf_curve_name) #I have to fix this to automatically take the file_name

ggplot(data=peaks_data, aes(x=caa, y=`T/C`)) +
  geom_point() +
  scale_x_continuous(trans = 
                       'log10') +
  geom_line()

dev.off()

remove(file_name, pdf_curve_name, pdf_peaks_name, pdf_smoothpeaks_name, peaks_data_filename,
       plots_list, peaks_graph_smooth)




