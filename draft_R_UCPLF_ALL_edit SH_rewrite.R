#----------- Install Packages ------
library(readxl)
library(pracma)
library(openxlsx)
library(tidyverse)
library(naniar)
library(janitor)
library(zoo)
library(DescTools)
library(dplyr)
library(ggplot2)
library(cowplot)
library(baseline)

#----------- Manual Input ------

file_name <- "230726, SCAA20, ITG soldier samples 21-30, STD 1-10.xlsx"
test_samples <- c(11,12,13,14,15,16,17,18,19,20) #samples that are NOT part of the standard curve

#----------- Load Data -------

df <- read_excel(file_name, sheet = "Well results",
                 col_names = FALSE)

#----------- Load Functions  ------

#extract measurement data to detect peaks

df_peaks <- function(x) {
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)] 
}

#extract strip line numbers/position

extract_stripline <- function(x) { 
  strip_line <- x %>%
    select(-c(2:12)) %>%
    filter(...1 == "Strip 1") %>%
    select(-1) %>%
    t() %>%
    as.numeric() %>%
    as.vector()
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
    
    # Perform auc calculations #x=z y=x love it 'SH'
    c <- AUC(x = z, y = x[[col]], from = z[a], to = z[b], 
             absolutearea = TRUE, method = "trapezoid")
    c_vector <- append(c_vector, c)
  }
  
  return(c_vector)
}

#much needed baseline correction for weird strips it's terrible now but will be fixed
background_correction <- function(x) {
  x_cleaned <- na.omit(x)
  df <- as.data.frame(t(x_cleaned))
  df_corrected <- baseline.medianWindow(as.matrix(df), 6, 2)$corrected
  df_corrected_abs <- abs(df_corrected)
  df_final <- as.data.frame(t(df_corrected_abs))
  
  return(df_final)
}

#----------- Reshape Data from Reader Output -------

peaks <- df_peaks(df) 

strip_line <- extract_stripline(df)
strip_line <- t(strip_line) #I have to check this in the function still
df_tidy <- reshape_df(df, strip_line) # create tidy dataframe with raw data 

#----------- Smooth Peak Data -----

peaks <- t(peaks)
N <- ncol(peaks) #count strips

peaks_smooth <- data.frame(matrix(nrow=nrow(peaks),ncol=ncol(peaks))) #make empty dataframe to fill in with the smooth data, length depending on the p factor

for (i in seq_along(peaks)) { 
  if (i<=N) { output <-peaks[,i]
  peaks_smooth[,i] <- output
  }
}

peaks_smooth = na.omit(peaks_smooth)
peaks_smooth = background_correction(peaks_smooth)

#####
strip_line = as.numeric(strip_line)

strip_line = as.data.frame(strip_line)


peaks_line = peaks_smooth %>% mutate("position" = strip_line)

ggplot(peaks_line, aes(x= `position`, y= `X1`)) + geom_line()
