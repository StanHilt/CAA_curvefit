#----------- Load Packages ------
library(readxl)
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

#----------- Load Functions  ------

#extract measurement data to detect peaks

df_peaks <- function(x) {
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)] 
}

#extract strip line numbers/position

extract_stripline <- function(x) { 
  strip_line <- x %>%
    dplyr::select(-c(2:12)) %>%
    dplyr::filter(...1 == "Strip 1") %>%
    dplyr::select(-1) %>%
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
    as.numeric(df_long$strip)
  print(df_long)
}

#create tidy dataframe with smooth data 

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
  df_long_smooth$strip <- gsub("V", "", df_long_smooth$strip) %>%
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


#much needed baseline correction for weird strips it's terrible now but will be fixed
background_correction <- function(x) {
  x_cleaned <- na.omit(x)
  df <- as.data.frame(t(x_cleaned))
  df_corrected <- baseline::baseline.medianWindow(as.matrix(df), 6, 2)$corrected
  df_corrected_abs <- abs(df_corrected)
  df_final <- as.data.frame(t(df_corrected_abs))
  
  return(df_final)
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



#----------- Manual Input ------

file_name <- "Two standard curves"
test_samples <- NA #samples that are NOT part of the standard curve

#----------- Load Data -------

df <- readxl::read_excel(paste0(file_name, ".xlsx"), 
                         sheet = "Well results",
                         col_names = FALSE)

#----------- Reshape Data from Reader Output -------
peaks <- df_peaks(df) 

strip_line <- extract_stripline(df)

df_tidy <- reshape_df(df, strip_line) # create tidy dataframe with raw data 


#----------- Smooth Peak Data -----

peaks <- t(peaks)
N <- ncol(peaks) #count strips

peaks_smooth <- peaks

peaks_smooth <- stats::na.omit(peaks_smooth)
peaks_smooth <- background_correction(peaks_smooth)

strip_line <- as.numeric(strip_line)

#Make tidy data format dataframe from smooth data after baseline correction
df_tidy_smooth <- reshape_df_smooth(peaks_smooth, strip_line)

df_tidy_smooth <- df_tidy_smooth %>%
  dplyr::mutate(sample_id = if_else(strip %in% test_samples, 
                                    "test_sample", "standard")) %>%
  dplyr::arrange(strip) %>%
  tidyr::unite(sample_id, strip, 
               col = "sample_name", 
               remove = FALSE) 

remove(test_samples, N)

df_tidy_smooth$caa <- NA
df_tidy_smooth <- df_tidy_smooth %>%  #Should this go into manual input?
  dplyr::mutate(caa = case_when(
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
    TRUE ~ NA  
  ))


#visualize all smooth and raw peaks overlaying to compare

pdf_smoothpeaks_name <- paste0("raw_vs_smooth_peaks", file_name, ".pdf")

pdf(pdf_smoothpeaks_name) #I have to fix this to automatically take the file_name

peaks_graph_smooth <- ggplot2::ggplot() + 
  ggplot2::geom_line(data = df_tidy_smooth, 
                     mapping = aes(x = position, 
                                   y = measurement, 
                                   color = "Smooth Data"),
                     group = 1, linetype = "solid", linewidth = 0.3) +
  ggplot2::geom_line(data = df_tidy, 
                     mapping = aes(x = position, 
                                   y = measurement, 
                                   color = "Raw Data"),
                     group = 1, linetype = "solid", linewidth = 0.1) +
  ggplot2::facet_wrap(~ strip) +
  ggplot2::theme_light() +
  ggplot2::labs(x = "Position in Strip (mm)", 
                y = "Smooth Signal Intensity") +
  ggplot2::scale_color_manual(values = c("black", "red"),
                              labels = c("Raw Data", "Smooth Data"))

print(peaks_graph_smooth)
dev.off()
remove(pdf_smoothpeaks_name)
#----------- Peak Detection in Smooth Data ------ DOES NOT WORK, RECHECK

peaks_list <- vector("list", ncol(peaks_smooth))
for (i in seq_along(peaks_smooth[-c(75:112),])) {
  output <- pracma::findpeaks(peaks_smooth[,i], 
                              minpeakdistance = 15, 
                              nups = 2, ndowns = 3, #THIS MAKES A DIFFERENCE, RECHECK
                              npeaks = 2,
                              sortstr = FALSE)
  peaks_list[[i]] <- output
}

for (i in seq_along(peaks_list)) { #optional, to quickly check the peaks 
  print(peaks_list[[i]])
}


remove(i, output)

# Reorder each matrix of the peak list so that the test peak is expressed first
peaks_list <- lapply(peaks_list, 
                     order_peak_matrix)

#----------- Check All Graphs with Peak Points -------

pdf_peaks_name <- paste0("peaks_", file_name, ".pdf")

pdf(pdf_peaks_name) 

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
    ggplot2::geom_point(data = df_test[peak_test, ], 
               color = "red",
               size = 1.2) +
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
  
  plots_list[[i]] <- p
  
}

all_peaks_grid <- cowplot::plot_grid(plotlist = plots_list, 
                                     ncol = 4) #option 1: all peaks in a grid
#print(all_peaks_grid)

for (p in plots_list) { #option 2: each peak in a different pdf page
  print(p)
}

dev.off()

remove(i, df_test, peak_test, p)

#extract peak data from generated peak list and reorganize:

peaks_df <- extract_peak_data(peaks_list)

peaks_data <- merge_peak_data(df_tidy_smooth, 
                              peaks_df)


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

peaks_data_export <- dplyr::select(peaks_data, sample_name, 
                            strip, caa, auc_T, auc_C, `T/C`)

peaks_data_filename <- paste0("peaks_data_", file_name, ".xlsx")

openxlsx::write.xlsx(peaks_data_export, peaks_data_filename)

remove(df, df_tidy, df_tidy_smooth, peaks, peaks_list, 
       peaks_smooth, strip_line, auc_C, auc_T, peaks_data_filename)


#---------Reshape Data to Fit Curve---------

#make new dataframe selecting all strips that contain a standard curve measurements (caa != NA) 
peaks_data_standard <- peaks_data_export %>% 
  tidyr::drop_na(caa) %>%
  dplyr::select(sample_name, caa, `T/C`)

#name the different standard curves ("Curve1"/"Curve2"/"Curve3) 
sequence <- rep(1:ceiling(nrow(peaks_data_standard)/10), each = 10, 
                length.out = nrow(peaks_data_standard))
peaks_data_standard$curve_name <- paste0("Curve", sequence)

#check the unfitted curve of all standard measurements
unfitted_curve <- ggplot2::ggplot(data=peaks_data_standard, 
                                  aes(x=caa, y=`T/C`)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(trans = 
                                'log10') +
  ggplot2::scale_y_continuous(trans = 
                                'log10') + #?
  ggplot2::theme_minimal()


#Take the average for each measurement from each standard curve (usually 1 to 3 but can be any number of st curves)
curve <- vector()
for (i in 1:10) {
  a <- peaks_data_standard %>%
    dplyr::filter(stringr::str_ends(sample_name, as.character(i)))  # Convert i to a character
  b <- mean(a$`T/C`)
  curve[i] <- b  # Store the mean value in the appropriate index of the vector c
}


#calculate average T/C corresponding to 0 caa
curve[11] <- (curve[9]+curve[10])/2
curve <- curve[-c(9,10)]

standard_df <- dplyr::tibble(CAA = peaks_data_standard$caa[1:9], 
                             "T/C ratio" = curve)

#plot the unfitted curve with the average standard measurements (no needed if it's only 1 st curve, it'll be the same)
unfitted_curve_av <- ggplot2::ggplot(data=standard_df, 
                                     aes(x=CAA, y=`T/C ratio`)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(trans = 
                                'log10') +
  ggplot2::scale_y_continuous(trans = 
                                'log10') +
  ggplot2::theme_minimal()


#---------Fit The Curve-------------

RESP <- standard_df$`T/C ratio`
DOSE <- standard_df$CAA
NAMES  = c("slope","lower","upper","ed50")

a <- drc::drm(standard_df$`T/C ratio` ~ standard_df$CAA,
              data = peaks_data_standard, fct = LL.4(names = NAMES),
              robust = "median") #not sure about this method, can still adapt

#Plot the standard curve made with the averages and compare with individual measurements (different colors)

pdf_standardcurve_name <- paste0("standard_curve_", file_name, ".pdf")

pdf(pdf_standardcurve_name) #I have to fix this to automatically take the file_name

plot(a, col = "steelblue3",
     xlab = "CAA",
     ylab = "T/C",
     pch = 16,
     log = "xy") 
#If more than one standard curve, plot all points to compare
points(peaks_data_standard$caa, peaks_data_standard$`T/C`,
       col=factor(peaks_data_standard$curve_name),
       pch = 16)
legend(0.01, 2, legend=c("Standard Curve 1", 
                         "Standard Curve 2",
                         "Standard Curve 3", 
                         "Average"),
       col=c("black", "coral2", "chartreuse4", "steelblue3"),
       pch = 16)

dev.off()
remove(pdf_standardcurve_name)



#---------Delete a Data Point (If Needed)-----------------

peaks_data_standard <- delete_points(df = peaks_data_standard,    
                                     curve_number = "Curve1",  #the standard curve from which you wish to delete a standard T/C value
                                     standard_point = "9")          #the standard point number you want to delete (eg "8" if it's the 8th point)
peaks_data_standard <- delete_points(df = peaks_data_standard,    
                                     curve_number = "Curve2",  #the standard curve from which you wish to delete a standard T/C value
                                     standard_point = "10")  
#repeat for as many points as needed

#---------Repeat All Curve Fit Steps --------------
#if a point was deleted in the previous step

#Take the average for each measurement from each standard curve (usually 1 to 3 but can be any number of st curves)
curve <- vector()
for (i in 1:10) {
  a <- peaks_data_standard %>%
    dplyr::filter(str_ends(sample_name, as.character(i)))  # Convert i to a character
  b <- mean(a$`T/C`)
  curve[i] <- b  # Store the mean value in the appropriate index of the vector c
}

peaks_data_standard

#calculate average T/C corresponding to 0 caa
curve[11] <- (curve[9]+curve[10])/2
curve <- curve[-c(9,10)]

standard_df <- dplyr::tibble(CAA = peaks_data_standard$caa[1:9], 
                             "T/C ratio" = curve)

#plot the unfitted curve with the average standard measurements (no needed if it's only 1 st curve, it'll be the same)
unfitted_curve_av <- ggplot2::ggplot(data=standard_df, aes(x=CAA, y=`T/C ratio`)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(trans = 
                                'log10') +
  ggplot2::theme_minimal()

#Fit The Curve after deleting point

RESP <- standard_df$`T/C ratio`
DOSE <- standard_df$CAA
NAMES  = c("slope","lower","upper","ed50")

a <- drc::drm(standard_df$`T/C ratio` ~ standard_df$CAA,
              data = peaks_data_standard, fct = LL.4(names = NAMES),
              robust = "median") #not sure about this method, can still adapt

#Plot the standard curve made with the averages and compare with individual measurements (different colors)
pdf_standardcurve_name <- paste0("standard_curve_", file_name, ".pdf")

pdf(pdf_standardcurve_name) #I have to fix this to automatically take the file_name

plot(a, col = "steelblue3",
     xlab = "CAA",
     ylab = "T/C",
     pch = 16,
     log = "xy") 
#If more than one standard curve, plot all points to compare
points(peaks_data_standard$caa, peaks_data_standard$`T/C`,
       col=factor(peaks_data_standard$curve_name),
       pch = 16)
legend(0.01, 2, legend=c("Standard Curve 1", 
                         "Standard Curve 2",
                         "Standard Curve 3", 
                         "Average"),
       col=c("black", "coral2", "chartreuse4", "steelblue3"),
       pch = 16)

dev.off()
remove(pdf_standardcurve_name)
#---------Unknown Test Samples CAA Prediction-----------

#make a new data frame selecting unknown test samples 
peaks_data_unknown <- peaks_data_export %>%
  dplyr::filter(str_detect(sample_name, "test_sample"))

#predict caa value from T/C value
test_sample <- drc::ED(object = a, 
                       respLev = peaks_data_unknown$`T/C`, 
                       type = "absolute")

results_df <- cbind(peaks_data_unknown, test_sample) %>%
  dplyr::select(strip, `T/C`, Estimate, `Std. Error`) 
rownames(results_df) <- NULL
colnames(results_df) <- c("Strip Number", "T/C Ratio", 
                          "CAA Estimate", "Std. Error")

results_df$Result <- NA #make new column for printing the result
lower_st <- tail(which(!is.na(curve)), 1)


cut_off <- 1 #above which the sample is positive/manual input??

results_df <- results_df %>%
  dplyr::mutate(Result = case_when(
    is.nan(`CAA Estimate`) & `T/C Ratio` > curve[1] ~ "above limit of detection",
    is.nan(`CAA Estimate`) & `T/C Ratio` < curve[lower_st] ~ "below limit of detection/negative",
    `CAA Estimate` > cut_off ~ "positive",
    `CAA Estimate` < cut_off ~ "negative"
  ))

results_filename <- paste0("results_", file_name, ".xlsx")

openxlsx::write.xlsx(results_df, results_filename)

remove(b, curve, cut_off, DOSE, RESP, lower_st,
       i, NAMES, results_filename, pdf_standardcurve_name,
       pdf_peaks_name, test_sample, peaks_data, peaks_data_standard,
       peaks_data_unknown, sequence)

