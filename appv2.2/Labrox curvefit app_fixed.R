library(readxl)
library(purrr)
library(pracma)
library(openxlsx)
library(tidyr)
library(DescTools)
library(dplyr)
library(ggplot2)
library(cowplot)
library(baseline)
library(stringr)
library(drc)
library(MASS)
library(zoo)
library(shiny)
library(shiny)
library(shinythemes)

#####Global functions####

#extract signal intensity measurement data from initial input file

roll.min <- function(x, win.width=2, na.rm=T, ...)
{
  library(zoo)
  # This will return a vector of the same size as original and will deal with NAs and optimize for mean.
  return(rollapply(x, width=win.width, FUN=min, na.rm=na.rm, ..., partial=T, align='center'))
}

rollMinSubtraction <- function(y, win.width)
{
  return(y-roll.min(y, win.width=win.width))
}

removeNoise <- function(y, nSig=1.96, thresh=20*mad(y))
{
  return(y-(median(y[y<thresh]) + nSig*mad(y[y<thresh])))
}

df_peaks <- function(x) {
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)]
  print(df_m)
}

extract_stripline <- function(x) { 
  strip_line <- x %>%
    dplyr::select(-c(2:12)) %>%
    dplyr::filter(...1 == "Strip 1") %>%
    dplyr::select(-1) %>%
    as.numeric() %>%
    as.vector() %>%
    t()
}

addLeadingZero <- function(numbers) {
  result <- sapply(numbers, function(number) {
    if (nchar(number) == 1) {
      return(paste0("0", number))
    } else {
      return(as.character(number))
    }
  })
  return(result)
}

reshape_df <- function(x, y) {    #x is the dataframe, y is the strip line numbers, I still have to make this more tidy
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)]  
  df_m[nrow(df_m) + 1,] <- y
  df_m <- t(df_m)
  df_m <- dplyr::as_tibble(df_m)
  n_position <- ncol(df_m) - 1
  df_long <- tidyr::pivot_longer(df_m, cols = 1:n_position, 
                                 names_to = "strip", 
                                 values_to = "measurement")
  df_long <- dplyr::arrange(df_long, strip)
  last_col <- paste("V", ncol(df_m), sep = "")
  df_long <- df_long %>% 
    rename("position" = last_col) 
  df_long$strip <- gsub("V", "", df_long$strip) %>%
    addLeadingZero() %>%
    as.factor()
  print(df_long)
}

#rollmean function

roll <- function(peaks){
  p <- 2
  N <- nrow(peaks)-1
  peaks_smooth <- data.frame(matrix(nrow=N,ncol=ncol(peaks))) 
  for (i in seq_along(peaks)) {
    if (i <= ncol(peaks)) {
      output <- zoo::rollmean(peaks[, i], p)
      peaks_smooth[, i] <- output
    } else {
      # Handle the case when i is greater than the number of columns in peaks
    }
    stats::na.omit(peaks_smooth)
  }
  return(peaks_smooth)
}
#baseline correction 

background_correction <- function(x) {
  x_cleaned <- na.omit(x)
  df <- as.data.frame(t(x_cleaned))
  df_corrected <- baseline::baseline.medianWindow(as.matrix(df), 6, 3)$corrected
  df_corrected_abs <- abs(df_corrected)
  df_final <- as.data.frame(t(df_corrected_abs))
  
  return(df_final)
}

#create tidy dataframe with smooth data after baseline background correction

reshape_df_smooth <- function(x, y) { #x = dataframe of smooth peaks, y = strip line 
  strip_line_smooth <- y
  strip_line_smooth <- dplyr::as_tibble(strip_line_smooth)
  peaks_smooth <- cbind(x, strip_line_smooth)
  peaks_smooth <- dplyr::as_tibble(peaks_smooth)
  n_position <- ncol(peaks_smooth) - 1
  df_long_smooth <- tidyr::pivot_longer(peaks_smooth, cols = 1:n_position, 
                                        names_to = "strip", 
                                        values_to = "measurement")
  df_long_smooth <- dplyr::arrange(df_long_smooth, strip)
  df_long_smooth <- df_long_smooth %>% 
    rename("position" = "value")
  df_long_smooth$strip <- gsub("X", "", df_long_smooth$strip) %>%
    as.numeric(df_long_smooth$strip)
  print(df_long_smooth)
}

add_samples <- function(df_tidy_smooth, test_samples, assay){
  test_samples <- test_samples
  df_tidy_smooth <- dplyr::mutate(df_tidy_smooth, sample_id = if_else(strip %in% test_samples, "test_sample", "standard"))
  df_tidy_smooth <- dplyr::arrange(df_tidy_smooth, strip) 
  df_tidy_smooth <- tidyr::unite(df_tidy_smooth, sample_id, strip, 
                                 col = "sample_name", 
                                 remove = FALSE)
  df_tidy_smooth$caa <- NA
  df_tidy_smooth <- if(assay == "2" | assay == "4"){
    dplyr::mutate(df_tidy_smooth, caa = case_when(
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
    sample_name == "standard_11" ~ 1000,
    sample_name == "standard_12" ~ 316,
    sample_name == "standard_13" ~ 100,
    sample_name == "standard_14" ~ 31.6,
    sample_name == "standard_15" ~ 10,
    sample_name == "standard_16" ~ 3.16,
    sample_name == "standard_17" ~ 1,
    sample_name == "standard_18" ~ 0.3,
    sample_name == "standard_19" ~ 0,
    sample_name == "standard_20" ~ 0,
    sample_name == "standard_21" ~ 1000,
    sample_name == "standard_22" ~ 316,
    sample_name == "standard_23" ~ 100,
    sample_name == "standard_24" ~ 31.6,
    sample_name == "standard_25" ~ 10,
    sample_name == "standard_26" ~ 3.16,
    sample_name == "standard_27" ~ 1,
    sample_name == "standard_28" ~ 0.3,
    sample_name == "standard_29" ~ 0,
    sample_name == "standard_30" ~ 0,
    TRUE ~ NA  
  ))
  }else{
    dplyr::mutate(df_tidy_smooth, caa = case_when(
      sample_name == "standard_1" ~ 10000,
      sample_name == "standard_2" ~ 3160,
      sample_name == "standard_3" ~ 1000,
      sample_name == "standard_4" ~ 316,
      sample_name == "standard_5" ~ 100,
      sample_name == "standard_6" ~ 31.6,
      sample_name == "standard_7" ~ 10,
      sample_name == "standard_8" ~ 3,
      sample_name == "standard_9" ~ 0,
      sample_name == "standard_10" ~ 0,
      sample_name == "standard_11" ~ 10000,
      sample_name == "standard_12" ~ 3160,
      sample_name == "standard_13" ~ 1000,
      sample_name == "standard_14" ~ 316,
      sample_name == "standard_15" ~ 100,
      sample_name == "standard_16" ~ 31.6,
      sample_name == "standard_17" ~ 10,
      sample_name == "standard_18" ~ 3,
      sample_name == "standard_19" ~ 0,
      sample_name == "standard_20" ~ 0,
      sample_name == "standard_21" ~ 10000,
      sample_name == "standard_22" ~ 3160,
      sample_name == "standard_23" ~ 1000,
      sample_name == "standard_24" ~ 316,
      sample_name == "standard_25" ~ 100,
      sample_name == "standard_26" ~ 31.6,
      sample_name == "standard_27" ~ 10,
      sample_name == "standard_28" ~ 3,
      sample_name == "standard_29" ~ 0,
      sample_name == "standard_30" ~ 0,
      TRUE ~ NA  
    ))
    
  }
  print(df_tidy_smooth)
  return(df_tidy_smooth)
}

peaks_graph_smooth <- function(df_tidy, df_tidy_smooth){
  peaks_graph_smooth<- ggplot2::ggplot() + 
    ggplot2::geom_line(data = df_tidy_smooth, 
                       mapping = aes(x = position, 
                                     y = measurement, 
                                     color = "Smooth Data"),
                       group = 1, linetype = "solid", linewidth = 0.3) +
    ggplot2::geom_line(data = df_tidy, 
                       mapping = aes(x = `strip_line()`, 
                                     y = value, 
                                     color = "Raw Data"),
                       group = 1, linetype = "solid", linewidth = 0.1) +
    ggplot2::facet_wrap(~ as.numeric(strip), ncol = 2) +
    ggplot2::theme_light() +
    ggplot2::labs(x = "Position in Strip (mm)", 
                  y = "Signal Intensity") +
    ggplot2::scale_color_manual(values = c("black", "red"),
                                labels = c("Raw Data", "Background Corrected"))+
    ggplot2::theme(legend.position = "top",
                   legend.text = element_text(size = 15),
                   legend.title = element_blank(),
                   strip.text = element_text(size = 15))
  return(peaks_graph_smooth)
}


peak_list_ctrl <- function(peaks_smooth, range) { 
  peak_list_ctrl <- vector("list", ncol(peaks_smooth))
  for (i in seq_along(peaks_smooth)) {
    output <- findpeaks(peaks_smooth[range[1]:range[2], i],
                        nups = 2, ndowns = 2, 
                        npeaks = 1,
                        minpeakheight = 100000,
                        threshold = 10,
                        zero = "0"
    )
    peak_list_ctrl[[i]] <- output
  }
  replacement_vector <- range[1]:range[2]
  peak_list_ctrl <- lapply(peak_list_ctrl, function(matrix) {
    replace_position_values(matrix, replacement_vector)
  })
  return(peak_list_ctrl)
}

plot_list_control <- function(peaks_smooth, df_tidy_smooth, peak_list_ctrl){
  plot_list<- list()
  for (i in 1:ncol(peaks_smooth)) {
    # Take values from the peaks_list for the current number 
    peak_test <- peak_list_ctrl[[i]]
    # Separate the dataframe containing the group with the corresponding number
    df_test <- dplyr::filter(df_tidy_smooth, 
                             strip == i)
    # Convert peak_test and peak_points to simple vectors
    peak_test <- as.vector(peak_test)
    # Create a plot for the current number and store it in the list
    p <- ggplot2::ggplot(df_test, aes(x = position, 
                                      y = measurement)) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(data = df_test[peak_test, ], 
                          color = "red",
                          size = 0.9) +
      ggplot2::labs(title = paste("strip", i),
                    x = "Strip Position",
                    y = "Smooth Signal Intensity",
                    col = NULL) +
      ggplot2::theme_classic() + 
      ggplot2::theme(plot.title = element_text(hjust = 0.5,
                                               size = 13,
                                               colour = "black"),
                     axis.title.x = element_text(color = "gray37",
                                                 size = 9),
                     axis.title.y = element_text(colour = "gray37",
                                                 size = 9),
                     panel.background = element_rect(fill = 'white', 
                                                     color = 'gray37')) +
      ggplot2::scale_y_continuous(labels = scales::scientific,
                                  limits = c(0,max(df_tidy_smooth$measurement) + 10000)) 
    plot_list[[i]] <- p
  }
  ctrl_peaks_grid <- cowplot::plot_grid(plotlist = plot_list, 
                                        ncol = 4) 
  print(ctrl_peaks_grid)
}

plot_list_all <-function(peaks_smooth, df_tidy_smooth, peaks_list){
  plots_list <- list()
  for (i in 1:ncol(peaks_smooth)) {
    # Take values from the peaks_list for the current number 
    peak_test <- peaks_list[[i]]
    
    # Separate the dataframe containing the group with the corresponding number
    df_test <- dplyr::filter(df_tidy_smooth, 
                             strip == i)
    # Convert peak_test and peak_points to simple vectors
    peak_test <- as.vector(peak_test)
    
    # Create a plot for the current number and store it in the list
    p <- ggplot2::ggplot(df_test, aes(x = position, 
                                      y = measurement)) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(data = df_test[peak_test[3:8], ], 
                          color = "red",
                          size = 1.6) +
      ggplot2::labs(title = paste("strip", i),
                    x = "Strip Position",
                    y = "Smooth Signal Intensity",
                    col = NULL) +
      ggplot2::theme_classic() + 
      ggplot2::theme(plot.title = element_text(hjust = 0.5,
                                               size = 13,
                                               colour = "black"),
                     axis.title.x = element_text(color = "gray37",
                                                 size = 9),
                     axis.title.y = element_text(colour = "gray37",
                                                 size = 9),
                     panel.background = element_rect(fill = 'white', 
                                                     color = 'gray37')) +
      ggplot2::scale_y_continuous(labels = scales::scientific)
    
    
    plots_list[[i]] <- p
    
  }
  
  all_peaks_grid <- cowplot::plot_grid(plotlist = plots_list, 
                                       ncol = 2) #all peaks in a grid
  
  print(all_peaks_grid)
}

#replace restricted values that determine the range for peak detection with the corresponding strip line values

replace_position_values <- function(matrix_to_modify, replacement_vector) {
  # Replace values in the first row except for the first column
  matrix_to_modify[1, 2:4] <- replacement_vector[matrix_to_modify[1, 2:4]]
  return(matrix_to_modify)
}

#(if test peak too small to be detected) convert 1x4 matrix to 2x4 with NA in the second row

fill_matrix <- function(mat) {
  if (nrow(mat) == 1 && ncol(mat) == 4) {
    # Create a 2x4 matrix with the same data from the 1x4 matrix
    new_mat <- matrix(NA, nrow = 2, ncol = 4)
    new_mat[1, ] <- mat
    return(new_mat)
  } else {
    return(mat)
  }
}

#extract peak calculations from peak list and make dataframe and reorganize

extract_peak_data <- function(x) {
  # Create an empty tibble with desired column names
  test_df <- dplyr::tibble(
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
    test_df[row_index, "height_T"] <- current_df[2, 1]
    test_df[row_index, "max_T"] <- current_df[2, 2]
    test_df[row_index, "start_T"] <- current_df[2, 3]
    test_df[row_index, "end_T"] <- current_df[2, 4]
    test_df[row_index + 1, "height_C"] <- current_df[1, 1]
    test_df[row_index + 1, "max_C"] <- current_df[1, 2]
    test_df[row_index + 1, "start_C"] <- current_df[1, 3]
    test_df[row_index + 1, "end_C"] <- current_df[1, 4]
  }
  
  
  
  # Group the rows in test_df by pairs
  test_df <- test_df %>%
    dplyr::group_by(grp = (row_number() - 1) %/% 2) %>%
    dplyr::summarize_all(~if (is.numeric(.)) {
      sum(., na.rm = TRUE)
    } else {
      first(.)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(-grp)
}

merge_peak_data <- function(x, y){ #x = the tidy dataframe, y = the dataframe with the peaks data calculations (start, max ,end) 
  df_merged <- x %>% 
    dplyr::select(-c(position, measurement))
  df_merged <- dplyr::distinct(df_merged)
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
    c <- DescTools::AUC(x = z, y = x[[col]], from = z[a], to = z[b], 
                        absolutearea = TRUE, method = "trapezoid")
    c_vector <- append(c_vector, c)
  }
  
  return(c_vector)
}

#If needed, delete outliers from the standard curve

delete_points <- function(df, curve_number, standard_point) {
  
  row_number <- which(stringr::str_detect(df$sample_name, standard_point) & df$curve_name == curve_number) # Get the row number
  
  if (length(unique(df$curve_name)) == 2) { 
    # If there are 2 curves, replace the value you want to delete with the value from the other curve
    
    a <- dplyr::filter(df,
                       stringr::str_detect(sample_name, 
                                           standard_point),
                       curve_name != curve_number) # Select the corresponding value from the other curve
    df$`T/C`[row_number] <- a$`T/C` # Replace it
    
  } else if (length(unique(df$curve_name)) == 3) {
    # If there are 3 curves, calculate the average of the values you want to keep
    
    b <- dplyr::filter(df,
                       stringr::str_detect(sample_name, 
                                           standard_point),
                       curve_name != curve_number)
    
    b$`T/C` <- as.numeric(b$`T/C`)
    
    df$`T/C`[row_number] <- mean(b$`T/C`) # Replace with the average of the other values
  } else {
    # When there's only 1 curve, replace with NA
    df$`T/C`[row_number] <- NA
  }
  
  return(df) # Return the modified dataframe
}

#test peak detection integrated

test_peak_detect <- function(peak_list_ctrl, peaks_smooth, range){
  a <- lapply(peak_list_ctrl,function(mat) mat[, 2])
  
  a <- as.numeric(unlist(a))
  
  b <- a - range[2]
  c <- a - range[1]
  
  #range start (b) and end (c) vectors
  b <- min(b)
  c <- max(c)
  
  # Set the length of the list to 96
  peak_list_test <- vector("list", length = ncol(peaks_smooth))
  
  # Detection of the test peak
  for (i in seq_along(peaks_smooth[b:c,])) {
    output <- pracma::findpeaks(peaks_smooth[b:c,i],
                                nups = 2, ndowns = 2, 
                                npeaks = 1,
                                minpeakheight = 100000
    )
    peak_list_test[[i]] <- output
  }
  
  #replace NULL matrices with 1x4 matrices filled with NA to enable later analysis
  for (i in seq_along(peaks_smooth[b:c,])) {
    if (i <= length(peak_list_test)) {
      if (is.null(peak_list_test[[i]])) {
        peak_list_test[[i]] <- matrix(nrow = 1, ncol = 4)
      }
    } else {
      peak_list_test[[i]] <- matrix(nrow = 1, ncol = 4) #add an empty matrix to the end of the list if it was deleted due to being NULL
    }
  }
  
  contains_only_na <- function(mat) {
    all(is.na(mat))
  }
  na_matrices <- sapply(peak_list_test, contains_only_na)
  
  recheck_peaks_index <- as.vector(which(sapply(na_matrices, function(mat) all(isTRUE(mat)))))
  
  recheck_peaks <-peaks_smooth[recheck_peaks_index] #get the measurements of the strips that a test peak was not detected in
  
  
  
  recheck_peak_list_test <- vector("list", ncol(recheck_peaks)) #a new empty list
  
  
  for (i in seq_along(recheck_peaks[b:c, i])) {
    # look for peaks in an alternative way in the strips that they were not detected
    output <- pracma::findpeaks(recheck_peaks[b:c, i],
                                peakpat = "[+]{2,}[-]{1,}[+]{1,}[-]{2,}",
                                npeaks = 1,
                                minpeakheight = 100000
    )
    
    # Check if output is not NULL before assigning
    if (!is.null(output)) {
      recheck_peak_list_test[[i]] <- output
    }
    
    # Print the values to debug
    cat("i:", i, "\n")
    print(output)
  }
  
  #replace the empty matrices with peak data with the new ones if a peak was detected
  peak_list_test[recheck_peaks_index] <- recheck_peak_list_test
  
  remove(recheck_peak_list_test, recheck_peaks_index, recheck_peaks)
  
  replacement_vector <- b:c
  
  peak_list_test <- lapply(peak_list_test, function(matrix) {
    replace_position_values(matrix, replacement_vector)
  })
  
  #merge control and test peaks detected in 1 list
  peaks_list <- lapply(1:length(peak_list_test), 
                       function(i) rbind(peak_list_ctrl[[i]], 
                                         peak_list_test[[i]]))
  
}

name_curves <- function(peaks_data_standard){
  sequence <- rep(1:ceiling(nrow(peaks_data_standard)/10), each = 10, 
                  length.out = nrow(peaks_data_standard))
  peaks_data_standard$curve_name <- paste0("Curve", sequence)
  #peaks_data_standard$caa <- as.numeric(peaks_data_standard$caa)
  #peaks_data_standard$`T/C` <- as.numeric(peaks_data_standard$`T/C`)
  return(as.data.frame(peaks_data_standard))
}

rawplot <- function(peaks_data_standard){
  p <- ggplot2::ggplot(data=peaks_data_standard, 
                       aes(x=caa, y=`T/C`)) +
    ggplot2::geom_point(color = "indianred", size = 2, alpha = 0.3) +
    ggplot2::scale_x_continuous(trans = 
                                  'log10') +
    ggplot2::scale_y_continuous(trans = 
                                  'log10') + #?
    ggplot2::theme_minimal() 
  
  return(p)
}

#calculate average T/C corresponding to 0 caa
std_df<-function(peaks_data_standard){
  curve <- vector()
  for (i in 1:10) {
    a <- peaks_data_standard %>%
      dplyr::filter(stringr::str_ends(sample_name, as.character(i)))  # Convert i to a character
    b <- mean(a$`T/C`)
    curve[i] <- b  # Store the mean value in the appropriate index of the vector c
  }  
  curve[11] <- (curve[9]+curve[10])/2
  curve <- curve[-c(9,10)]
  
  standard_df <- dplyr::tibble(CAA = peaks_data_standard$caa[1:9], 
                               "T/C ratio" = curve)
}

averageplot<- function(standard_df){
  plot <- ggplot2::ggplot(data=standard_df, 
                          aes(x=CAA, y=`T/C ratio`)) +
    ggplot2::geom_point(color = "indianred", size = 4, alpha = 0.8) +
    ggplot2::scale_x_continuous(trans = 
                                  'log10') +
    ggplot2::scale_y_continuous(trans = 
                                  'log10') +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = element_text(size = 20),
                   axis.title.y = element_text(size = 20))
  
  
  return(plot)
}

model <- function(standard_df, peaks_data_standard){
  RESP <- standard_df$`T/C ratio`
  DOSE <- standard_df$CAA
  NAMES  = c("slope","lower","upper","ed50")
  NEGATIVES = 
    
    a <- drc::drm(standard_df$`T/C ratio` ~ standard_df$CAA,
                  data = peaks_data_standard, fct = LL.4(names = NAMES),
                  weights = c(5,5,5,30,30,200,300,500, 500),
                  robust = "mean",
                  lowerl = c(NA, 0, NA, NA) 
    ) #not sure about this method, can still adapt
  print(a)
  return(a)
}

curvefitplot <- function(a, peaks_data_standard, assay){
  if(assay == 1 | assay == 3){
  fitted_curve <- plot(a, col = "steelblue3",
                       xlab = "CAA",
                       ylab = "T/C",
                       ylim = c(0.000001, max(na.omit(peaks_data_standard$`T/C`))),
                       pch = 16,
                       log = "yx") 
  #If more than one standard curve, plot all points to compare
  points(peaks_data_standard$caa, peaks_data_standard$`T/C`,
         col=factor(peaks_data_standard$curve_name),
         pch = 16)
  
  abline(h = mean(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0]) + 
           2*sd(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0]))
  
  legend(0.1, 0.5, legend=c("Standard Curve 1", 
                           "Standard Curve 2",
                           "Standard Curve 3", 
                           "Average"),
         col=c("black", "coral2", "chartreuse4", "steelblue3"),
         pch = 16)

  return(fitted_curve)
  }else{
    fitted_curve <- plot(a, col = "steelblue3",
                         xlab = "CAA",
                         ylab = "T/C",
                         ylim = c(0.000001, max(na.omit(peaks_data_standard$`T/C`))),
                         pch = 16,
                         log = "yx") 
    #If more than one standard curve, plot all points to compare
    points(peaks_data_standard$caa, peaks_data_standard$`T/C`,
           col=factor(peaks_data_standard$curve_name),
           pch = 16)
    abline(h = mean(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0]) + 
             2*sd(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0]))
    legend(0.01, 0.5, legend=c("Standard Curve 1", 
                              "Standard Curve 2",
                              "Standard Curve 3", 
                              "Average"),
           col=c("black", "coral2", "chartreuse4", "steelblue3"),
           pch = 16)
    return(fitted_curve)
  }
}

cutoffcalc <- function(a, peaks_data_standard){
  negatives <- mean(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0]) + 
    2 * sd(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0])
  
  cutoff <- drc::ED(
    object = a,
    respLev = negatives,
    type = "absolute"
  )
  return(cutoff)
}

resultfile <- function(peaks_data_export, a, peaks_data_standard, exclude, assay){
  peaks_data_unknown <- peaks_data_export %>%
    dplyr::filter(str_detect(sample_id, "test_sample"))
  
  #predict caa value from T/C value
  test_sample <- drc::ED(object = a, 
                         respLev = peaks_data_unknown$`T/C`, 
                         type = "absolute")
  
  curve <- vector()
  for (i in 1:10) {
    a <- peaks_data_standard %>%
      dplyr::filter(stringr::str_ends(sample_name, as.character(i)))  # Convert i to a character
    b <- mean(a$`T/C`)
    curve[i] <- b  # Store the mean value in the appropriate index of the vector c
  }  
  curve[11] <- (curve[9]+curve[10])/2
  curve <- curve[-c(9,10)]
  
  results_df <- cbind(peaks_data_unknown, test_sample) %>%
    dplyr::select(strip, `T/C`, Estimate) 
  rownames(results_df) <- NULL
  colnames(results_df) <- c("Strip Number", "T/C Ratio", 
                            "CAA Estimate")
  results_df$`CAA Estimate` <- round(results_df$`CAA Estimate`, 2)
  results_df$`T/C Ratio` <- round(results_df$`T/C Ratio`, 3)
  
  results_df$Result <- NA #make new column for printing the result
  lower_st <- tail(which(!is.na(curve)), 1)
  cut_off <- if(assay == "1"){
    20 #adjust to dry please
  }else{
    if(assay == "2"){
      2
    }else{
      if(assay == "3"){
        30
      }else{
        if(assay == "4"){
          3
        }
      }
    }
  } #above which the sample is positive/MANUAL INPUT??
  
  results_df <- 
    dplyr::mutate(results_df, Result = case_when(
      is.nan(`CAA Estimate`) & `T/C Ratio` > curve[1] ~ "High positive, above HLOD",
      is.nan(`CAA Estimate`) & `T/C Ratio` < curve[lower_st] ~ "Negative, below LLOD",
      `CAA Estimate` >= cut_off ~ "positive",
      `CAA Estimate` < cut_off ~ "negative"
    ))
  
  # Find rows where any of the columns match the input numbers
  rows_to_replace <- apply(results_df,1, function(row) any(row %in% exclude))
  
  # Replace values with "invalid" for selected rows
  results_df[rows_to_replace, -1] <- "invalid"
  return(as.data.frame(results_df))
  
}

delete_points <- function(df, input) {
  if (input != "") {
    x <- input
    x <- gsub(" ", "", x)
    x <- strsplit(x, ",")
    x <- matrix(unlist(x))
    x <- strsplit(x, "_")
    x <- as.data.frame(unlist(x))
    curve_number <- filter(x, grepl("Curve", x[, 1]))
    standard_point <- filter(x,!grepl("Curve", x[, 1]))
    print(curve_number)
    print(standard_point)
    
    for (i in 1:nrow(curve_number)) {
      row_number <-
        which(
          stringr::str_detect(df$sample_name, standard_point[i, ]) &
            df$curve_name == curve_number[i, ]
        ) # Get the row number
      
      if (length(unique(df$curve_name)) == 2) {
        # If there are 2 curves, replace the value you want to delete with the value from the other curve
        a <- dplyr::filter(
          df,
          stringr::str_detect(sample_name,
                              standard_point[i, ]),
          curve_name != curve_number[i, ]
        ) # Select the corresponding value from the other curve
        df$`T/C`[row_number] <- a$`T/C` # Replace it
      } else if (length(unique(df$curve_name)) == 3) {
        # If there are 3 curves, calculate the average of the values you want to keep
        b <- dplyr::filter(
          df,
          stringr::str_detect(sample_name,
                              standard_point[i, ]),
          curve_name != curve_number[i, ]
        )
        b$`T/C` <- as.numeric(b$`T/C`)
        df$`T/C`[row_number] <-
          mean(b$`T/C`) # Replace with the average of the other values
      } else {
        # When there's only 1 curve, replace with NA
        df$`T/C`[row_number] <- NA
      }
    }
    
    return(df) # Return the modified dataframe
  } else{
    return(df)
  }
}
#####

ui <- fluidPage(
theme = bslib::bs_theme(bootswatch = "cosmo"),
  # Application title
titlePanel("CAA Labrox Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", label = h4("Enter plate name")),
      
      fileInput(
        inputId = "file1", 
        label = "Standard curve"
      ),
      fileInput(
        inputId = "file2", 
        label = "Samples"
      ),
      fileInput(
        inputId = "file3", 
        label = "Samples"
      ),
      fileInput(
        inputId = "file4", 
        label = "Samples"
      ),
      fileInput(
        inputId = "file5", 
        label = "Samples"
      ),
      fileInput(
        inputId = "file6", 
        label = "Samples"
      ),
      radioButtons("radio", label = h4("Select number of standards"),
                   choices = list("1" = 1, "2" = 2, "3" = 3), 
                   selected = 1),
      
      selectInput("assay", label = h3("Select Assay"), 
                  choices = list("UCAAhT17" = 1, "UCAAhT417" = 2, "SCAA20" = 3, "SCAA500" = 4), 
                  selected = 1),
      actionButton("startAnalysis", "Start Analysis"),
      checkboxInput("Show_options", "Show advanced options:", value = FALSE),
      
      conditionalPanel(
        condition = "input.Show_options",
      textInput("exclude", label = h4("Enter strip numbers to exclude, separated by commas")),
      sliderInput("range", "Set control peak detection range", min = 40, max = 100, value = c(49,76)),
      sliderInput("range2", "Set test peak detection range", min = 0, max = 40, value = c(10,40)),
      
      textInput(
        "rmstandard", 
        label = h4("Enter standard curve and number to exclude, e.g Curve1_1, Curve2_3")
      ),
      numericInput(
        "window",
        label = h4("Select rolling minimum window width:"),
        min = 20, max = 100, value = 40
      ),
    ),
  ),
    
    # Main
    mainPanel(
      tabsetPanel(
        tabPanel("Background Correction", 
                  fluidRow(
                    column(10, plotOutput("rawvssmooth")),
                    column(2, downloadButton("downloadrawvssmooth", "Download plot"))
                  )
        ),
        #tabPanel("control peaks", plotOutput("plotlistcontrol")),
        tabPanel("Peak Detection",
                 fluidRow(
                  column(10,plotOutput("plotlistall")),
                  column(2,downloadButton("downloadPeaks", "Download plot", height = "10%"))
                 )
        ),
        tabPanel("Average curve", 
                 plotOutput("standardaverage", height = "800px", width = "800px"),
                 downloadButton("downloadavgcurve", "Download plot"),
        ),
        tabPanel("Fitted curve", 
                 plotOutput("plotfitcurve", height = "800px", width = "800px"),
                 downloadButton("downloadcurve", "Download plot"),
                 verbatimTextOutput("cutoffs"),
        ),
        #tabPanel("data table", dataTableOutput("df")),
        tabPanel("Results", 
                 dataTableOutput("results"), 
                 downloadButton("Downloadresults", "Download results"))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  df_list <- reactive({
    files <- list(
      input$file1,
      input$file2,
      input$file3,
      input$file4,
      input$file5,
      input$file6
    )
    
    total_files <- length(files)
    progress_step <- 100 / total_files
    progress_value <- 0
    
    withProgress(message = "Reading data", value = 0, max = 100, {
      data_list <- lapply(files, function(file) {
        if (!is.null(file$datapath)) {
          # Read your data frame from file (adjust this based on your actual data reading code)
          df <- read_excel(path = file$datapath, sheet = "Well results", col_names = FALSE)
          
          # Increment progress value
          progress_value <<- progress_value + progress_step
          setProgress(progress_value)
          return(df)
        }
      })
      
      # Filter out NULL elements (files that were not uploaded)
      data_list <- Filter(function(x) !is.null(x), data_list)
      
      # Print or process the list of data frames
      print(data_list)
      
      # Return the list of data frames
      return(data_list)
    })
  })

  
observeEvent(input$startAnalysis, {
  
output$df <- renderDataTable(peaks_df())
  
  test_samples<-reactive({ 
    n<-as.character(
      if(input$radio == 1 & ncol(df()) == 10){
        NA
      }else{
        if(input$radio == 1){
          11:ncol(df())
        }else{
          if(input$radio == 2 & ncol(df()) == 20){
            NA
          }else{
            if(input$radio == 2){
              21:ncol(df())
            }else{
              if(input$radio == 3 & ncol(df()) == 30){
                NA
              }else{
                if(input$radio == 3){
                  31:ncol(df())
                }
              }
            }
          }
        }
      }
    )
    print(n)
    return(n)
  })

    output$results <- renderDataTable(df())
  #grab peaks
  df <- reactive({
    dfs <- lapply(df_list(), df_peaks)
    df <- do.call(rbind, dfs)
    df[] <- lapply(df, as.numeric)
    df<- t(df)
    return((as.data.frame(df)))
  })
  
  strip_line <- reactive({
    l <- extract_stripline(df_list()[[1]])
    print(l)
    return(t(l))
    })

  df_clean <- reactive({
    new_names <- (1:ncol(df()))
    peaks <- df()[1:ncol(df())] %>% set_names(new_names)
    print(is.character(peaks[2,2]))
    df_clean <- cbind(strip_line(), peaks)
    return(df_clean)
  })
  
  df_tidy <- reactive({
    df_tidy <- gather(df_clean(), "strip", "value", 2:ncol(df_clean()))
    print(df_tidy)
  })
  
  peaks_smooth <- reactive({
    rolled_min <-
      rollMinSubtraction(df_clean()[2:ncol(df_clean())], input$window)#make input!!
    smoothed <- transmute_all(rolled_min, removeNoise)
    smoothed[smoothed < 0] <- 0
    return(smoothed)
  })
  
  peaks_smooth_tidy <- reactive({
    peaks_smooth_tidy <- cbind(strip_line(), peaks_smooth())
    peaks_smooth_tidy <-
      gather(peaks_smooth_tidy,
             "strip",
             "value",
             2:ncol(peaks_smooth_tidy))
    peaks_smooth_tidy$strip <- as.numeric(peaks_smooth_tidy$strip)
    peaks_smooth_tidy <-
      rename(peaks_smooth_tidy,
             c("position" = "strip_line()", "measurement" = value))
  })
  
  df_tidy_smooth <- reactive({
    df <- peaks_smooth_tidy() %>%
      dplyr::mutate(sample_id = if_else(strip %in% test_samples(),
                                        "test_sample", "standard")) %>%
      dplyr::arrange(strip) %>%
      tidyr::unite(sample_id, strip,
                   col = "sample_name",
                   remove = FALSE)
    
    if(input$assay == 2 || input$assay == 4) {
      df <- df %>%
        dplyr::mutate(
          caa = case_when(
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
            sample_name == "standard_11" ~ 1000,
            sample_name == "standard_12" ~ 316,
            sample_name == "standard_13" ~ 100,
            sample_name == "standard_14" ~ 31.6,
            sample_name == "standard_15" ~ 10,
            sample_name == "standard_16" ~ 3.16,
            sample_name == "standard_17" ~ 1,
            sample_name == "standard_18" ~ 0.3,
            sample_name == "standard_19" ~ 0,
            sample_name == "standard_20" ~ 0,
            sample_name == "standard_21" ~ 1000,
            sample_name == "standard_22" ~ 316,
            sample_name == "standard_23" ~ 100,
            sample_name == "standard_24" ~ 31.6,
            sample_name == "standard_25" ~ 10,
            sample_name == "standard_26" ~ 3.16,
            sample_name == "standard_27" ~ 1,
            sample_name == "standard_28" ~ 0.3,
            sample_name == "standard_29" ~ 0,
            sample_name == "standard_30" ~ 0,
            TRUE ~ NA
          )
        )
    }else{
      df <- df %>%
        dplyr::mutate(
          caa = case_when(
            sample_name == "standard_1" ~ 10000,
            sample_name == "standard_2" ~ 3160,
            sample_name == "standard_3" ~ 1000,
            sample_name == "standard_4" ~ 316,
            sample_name == "standard_5" ~ 100,
            sample_name == "standard_6" ~ 31.6,
            sample_name == "standard_7" ~ 10,
            sample_name == "standard_8" ~ 3,
            sample_name == "standard_9" ~ 0,
            sample_name == "standard_10" ~ 0,
            sample_name == "standard_11" ~ 10000,
            sample_name == "standard_12" ~ 3160,
            sample_name == "standard_13" ~ 1000,
            sample_name == "standard_14" ~ 316,
            sample_name == "standard_15" ~ 100,
            sample_name == "standard_16" ~ 31.6,
            sample_name == "standard_17" ~ 10,
            sample_name == "standard_18" ~ 3,
            sample_name == "standard_19" ~ 0,
            sample_name == "standard_20" ~ 0,
            sample_name == "standard_21" ~ 10000,
            sample_name == "standard_22" ~ 3160,
            sample_name == "standard_23" ~ 1000,
            sample_name == "standard_24" ~ 316,
            sample_name == "standard_25" ~ 100,
            sample_name == "standard_26" ~ 31.6,
            sample_name == "standard_27" ~ 10,
            sample_name == "standard_28" ~ 3,
            sample_name == "standard_29" ~ 0,
            sample_name == "standard_30" ~ 0,
            TRUE ~ NA
          )
        )
      
    }
    return(df)
  })
  
  output$rawvssmooth <- renderPlot(
    withProgress(message = "Plotting", value = 0, {
      peaks_graph_smooth(df_tidy(), df_tidy_smooth())
    }),
    width = 1000,
    height = function()
      150 * (ncol(df_clean() / 2))
  )
  
  peak_list_control <- reactive({
    df <- peak_list_ctrl(peaks_smooth(), input$range)
    print(df)
  })
  
  peaks_list <- reactive({
    df <- test_peak_detect(peak_list_control(), peaks_smooth(), input$range2)
    print(df)
  })
  
  output$plotlistall <- renderPlot(
    plot_list_all(peaks_smooth(), df_tidy_smooth(), peaks_list()),
    width = 1000,
    height = function()
      150 * ncol(df_clean() / 2)
  )
  
  peaks_list_filled <- reactive({
    lapply(peaks_list(), fill_matrix)
  })
  
  peaks_df <- reactive({
    extract_peak_data(peaks_list_filled())
  })
  
  peaks_data <-reactive({
    merge_peak_data(df_tidy_smooth(), peaks_df())
  })
  
  auc_T <- reactive({
    auc_calculation(
      x = peaks_smooth(),
      y = peaks_data(),
      z = strip_line(),
      peak = "T"
    )
  })
  
  auc_C <- reactive({
    (auc_calculation(
      x = peaks_smooth(),
      y = peaks_data(),
      z = strip_line(),
      peak = "C"
    ))
  })
  
  #ratios T/C
  peaks_data_auc <- reactive({
    data <- cbind(peaks_data(), auc_T(), auc_C())
    data$"T/C" <- as.numeric(data$auc_T / data$auc_C)
    data
  })
  
  #Standard curve
  peaks_data_standard <- reactive({
    peaks_data_auc() %>%
      tidyr::drop_na(caa) %>%
      dplyr::select(sample_name, caa, `T/C`)
  })
  
  peaks_data_standard_curves <-reactive({
    (name_curves(peaks_data_standard()))
  })
  
  
  peaks_data_standard_names <- reactive({
    print(input$rmstandard)
    delete_points(df = peaks_data_standard_curves(), input$rmstandard)
  })
  
  output$standardcurve <-
    renderPlot({
      rawplot(peaks_data_standard_names())
    })
  standard_df <- reactive({
    std_df(peaks_data_standard_names())
  })
  
  output$standardaverage <-
    renderPlot(averageplot(standard_df()),
               width = 800,
               height = 800)
  
  curvemodel <- reactive({
    req(df())
    model(standard_df(), peaks_data_standard_names())
  })
  
  output$plotfitcurve <- renderPlot({
    req(df())
    curvefitplot(curvemodel(), peaks_data_standard_names(), input$assay)
  })
  
  cutoff <-
    reactive({
      cutoffcalc(curvemodel(), peaks_data_standard_names())
    })
  output$cutoffs <-
    renderText(c(paste0(
      "Calcultated cutoff = ", round(cutoff()[1, 1], digits = 2), "pg/ml"
    )))
  
  value <- reactive({
    numbers <- gsub(" ", "", input$exclude)
    numbers <- unlist(strsplit(numbers, ","))
    numbers <- as.numeric(numbers)
    return(numbers)
  })
  
  results <-
    reactive({
      as.data.frame(
        resultfile(
          peaks_data_auc(),
          curvemodel(),
          peaks_data_standard_names(),
          value(),
          input$assay
        )
      )
    })
  

  #download raw vs smooth
  output$downloadrawvssmooth = downloadHandler(
    filename = function() {
      paste0(input$name, "_raw_vs_smooth.png")
    },
    content = function(file) {
      dpi <- 800  # Set the desired DPI
      width_inches <- 6  # Set the desired width in inches
      height_inches <-
        (1 * (ncol(peaks_smooth() / 2))) # Set the desired height in inches
      
      width_pixels <- dpi * width_inches
      height_pixels <- dpi * height_inches
      
      png(
        file,
        width = width_pixels,
        height = height_pixels,
        units = "px",
        res = dpi
      )
      withProgress(message = "Downloading", value = 0,{
        print( peaks_graph_smooth(df_tidy(), df_tidy_smooth()))
      })
      dev.off()
    }
  )

  #download peaks
  output$downloadPeaks = downloadHandler(
    filename = function() {
      paste0(input$name, "_peaks.png")
    },
    content = function(file) {
      dpi <- 800  # Set the desired DPI
      width_inches <- 6  # Set the desired width in inches
      height_inches <-
        (1 * (ncol(peaks_smooth() / 2)))
      
      width_pixels <- dpi * width_inches
      height_pixels <- dpi * height_inches
      
      png(file, width = width_pixels, height = height_pixels, units = "px", res = dpi)
      withProgress(message = "Downloading", value = 0,{
        print(plot_list_all(peaks_smooth_bc(), df_tidy_smooth(), peaks_list()))
      })
      dev.off()
    }
  )
  
  output$downloadavgcurve = downloadHandler(
    filename = function() {
      paste0(input$name, "_average_curve.png")
    },
    content = function(file) {
      dpi <- 200  # Set the desired DPI
      width_inches <- 8  # Set the desired width in inches
      height_inches <- 6 # Set the desired height in inches
      
      width_pixels <- dpi * width_inches
      height_pixels <- dpi * height_inches
      
      png(file, width = width_pixels, height = height_pixels, units = "px", res = dpi)
      withProgress(message = "Downloading", value = 0,{
        print(averageplot(standard_df()))
      })
      dev.off()
    }
  )

  output$downloadcurve = downloadHandler(
    filename = function() {
      paste0(input$name, "_fitted_curve.png")
    },
    content = function(file) {
      dpi <- 200  # Set the desired DPI
      width_inches <- 8  # Set the desired width in inches
      height_inches <- 6 # Set the desired height in inches
      
      width_pixels <- dpi * width_inches
      height_pixels <- dpi * height_inches
      
      png(file, width = width_pixels, height = height_pixels, units = "px", res = dpi)
      withProgress(message = "Downloading", value = 0,{
        print(curvefitplot(curvemodel(), peaks_data_standard_names(), input$assay))
      })
      dev.off()
    }
  )
  
  #download results
  output$Downloadresults = downloadHandler(
    filename = function() {
      paste0(input$name, "_results.csv")
    },
    content = function(filename) {
      write.csv(results(), filename)
    }
  )
  
})

session$onSessionEnded(function() {
  stopApp()
})

}
# Run the application 
shinyApp(ui = ui, server = server)
