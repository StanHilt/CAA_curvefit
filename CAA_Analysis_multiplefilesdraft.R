## ----setup, include=FALSE---------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE)

library(here)
setwd(here::here("R:/Eva Iliopoulou/UCPLFCAA"))



## ----load packages, warning=FALSE, eval=FALSE-------------------------------------------------------------
## 
## library(readxl)
## library(pracma)
## library(openxlsx)
## library(tidyr)
## library(DescTools)
## library(dplyr)
## library(ggplot2)
## library(cowplot)
## library(baseline)
## library(stringr)
## library(drc)
## library(MASS)
## library(zoo)
## library(stringr)
## library(ggforce)
## 


## ----load functions, eval=FALSE---------------------------------------------------------------------------
## 
## #extract signal intensity measurement data from initial input file
## 
## df_peaks <- function(x) {
##   df_m <- dplyr::filter(x, x$...1 %in% "line1")
##   df_m <- df_m[,-c(1:12)]
## }
## 
## #extract strip line numbers/position from initial input file (position on the strip (mm) where each measurement was taken)
## 
## extract_stripline <- function(x) {
##   strip_line <- x %>%
##     dplyr::select(-c(2:12)) %>%
##     dplyr::filter(...1 == "Strip 1") %>%
##     dplyr::select(-1) %>%
##     as.numeric() %>%
##     as.vector() %>%
##     t()
## }
## 
## #create tidy data frame
## #1) function to add 0 before one digit numbers, so that for instance 2 becomes 02 (to help with order)
## addLeadingZero <- function(numbers) {
##   result <- sapply(numbers, function(number) {
##     if (nchar(number) == 1) {
##       return(paste0("0", number))
##     } else {
##       return(as.character(number))
##     }
##   })
##   return(result)
## }
## 
## #2) reshape the dataframe into tidy data format
## reshape_df <- function(x, y) {    #x is the dataframe, y is the strip line numbers, I still have to make this more tidy
##   df_m <- dplyr::filter(x, x$...1 %in% "line1")
##   df_m <- df_m[,-c(1:12)]
##   df_m[nrow(df_m) + 1,] <- y
##   df_m <- t(df_m)
##   df_m <- dplyr::as_tibble(df_m)
##   n_position <- ncol(df_m) - 1
##   df_long <- tidyr::pivot_longer(df_m, cols = 1:n_position,
##                                  names_to = "strip",
##                                  values_to = "measurement")
##   df_long <- dplyr::arrange(df_long, strip)
##   last_col <- paste("V", ncol(df_m), sep = "")
##   df_long <- df_long %>%
##     rename("position" = last_col)
##   df_long$strip <- gsub("V", "", df_long$strip) %>%
##     addLeadingZero() %>%
##     as.factor()
##   print(df_long)
## }
## 
## reshape_df_try <- function(x, y) {    #x is the dataframe, y is the strip line numbers, I still have to make this more tidy
##   df_m <- dplyr::filter(x, x$...1 %in% "line1")
##   df_m <- df_m[,-c(1:12)]
##   df_m[nrow(df_m) + 1,] <- y
##   df_m <- t(df_m)
##   df_m <- dplyr::as_tibble(df_m)
##   n_position <- ncol(df_m) - 1
##   df_long <- tidyr::pivot_longer(df_m, cols = 1:n_position,
##                                  names_to = "strip",
##                                  values_to = "measurement")
##   df_long <- dplyr::arrange(df_long, strip)
##   last_col <- paste("V", ncol(df_m), sep = "")
##   df_long <- df_long %>%
##     rename("position" = last_col)
##   df_long$strip <- gsub("V", "", df_long$strip) %>%
##     addLeadingZero() %>%
##     as.factor()
##   df_name <- deparse(substitute(x))
##   df_long$import_file <- df_name
##   df_long$strip <- paste(df_long$import_file, df_long$strip, sep = "_")
##   print(df_long)
## }
## 
## 
## 
## 
## #baseline correction
## 
## background_correction <- function(x) {
##   x_cleaned <- na.omit(x)
##   df <- as.data.frame(t(x_cleaned))
##   df_corrected <- baseline::baseline.medianWindow(as.matrix(df), 6, 3)$corrected
##   df_corrected_abs <- abs(df_corrected)
##   df_final <- as.data.frame(t(df_corrected_abs))
## 
##   return(df_final)
## }
## 
## #create tidy dataframe with smooth data after baseline background correction
## 
## reshape_df_smooth <- function(x, y) { #x = dataframe of smooth peaks, y = strip line
##   strip_line_smooth <- y
##   strip_line_smooth <- dplyr::as_tibble(strip_line_smooth)
##   peaks_smooth <- cbind(x, strip_line_smooth)
##   peaks_smooth <- dplyr::as_tibble(peaks_smooth)
##   n_position <- ncol(peaks_smooth) - 1
##   df_long_smooth <- tidyr::pivot_longer(peaks_smooth, cols = 1:n_position,
##                                         names_to = "strip",
##                                         values_to = "measurement")
##   df_long_smooth <- dplyr::arrange(df_long_smooth, strip)
##   df_long_smooth <- df_long_smooth %>%
##     rename("position" = "value")
##   df_long_smooth$strip <- as.factor(df_long_smooth$strip)
##   print(df_long_smooth)
## }
## 
## #replace restricted values that determine the range for peak detection with the corresponding strip line values
## 
## replace_position_values <- function(matrix_to_modify, replacement_vector) {
##   # Replace values in the first row except for the first column
##   matrix_to_modify[1, 2:4] <- replacement_vector[matrix_to_modify[1, 2:4]]
##   return(matrix_to_modify)
## }
## 
## #(if test peak too small to be detected) convert 1x4 matrix to 2x4 with NA in the second row
## 
## fill_matrix <- function(mat) {
##   if (nrow(mat) == 1 && ncol(mat) == 4) {
##     # Create a 2x4 matrix with the same data from the 1x4 matrix
##     new_mat <- matrix(NA, nrow = 2, ncol = 4)
##     new_mat[1, ] <- mat
##     return(new_mat)
##   } else {
##     return(mat)
##   }
## }
## 
## #extract peak calculations from peak list and make dataframe and reorganize
## 
## extract_peak_data <- function(x) {
##   # Create an empty tibble with desired column names
##   test_df <- dplyr::tibble(
##     height_T = NA,
##     max_T = NA,
##     start_T = NA,
##     end_T = NA,
##     height_C = NA,
##     max_C = NA,
##     start_C = NA,
##     end_C = NA
##   )
## 
##   # Iterate over the list of dataframes
##   for (i in seq_along(x)) {
##     # Get the current dataframe from the list
##     current_df <- x[[i]]
## 
##     # Calculate the row index for the current dataframe
##     row_index <- (i - 1) * 2 + 1
## 
##     # Assign values from the current dataframe to corresponding columns in test_df
##     test_df[row_index, "height_T"] <- current_df[2, 1]
##     test_df[row_index, "max_T"] <- current_df[2, 2]
##     test_df[row_index, "start_T"] <- current_df[2, 3]
##     test_df[row_index, "end_T"] <- current_df[2, 4]
##     test_df[row_index + 1, "height_C"] <- current_df[1, 1]
##     test_df[row_index + 1, "max_C"] <- current_df[1, 2]
##     test_df[row_index + 1, "start_C"] <- current_df[1, 3]
##     test_df[row_index + 1, "end_C"] <- current_df[1, 4]
##   }
## 
## 
## 
##   # Group the rows in test_df by pairs
##   test_df <- test_df %>%
##     dplyr::group_by(grp = (row_number() - 1) %/% 2) %>%
##     dplyr::summarize_all(~if (is.numeric(.)) {
##       sum(., na.rm = TRUE)
##     } else {
##       first(.)
##     }) %>%
##     dplyr::ungroup() %>%
##     dplyr::select(-grp)
## }
## 
## merge_peak_data <- function(x, y){ #x = the tidy dataframe, y = the dataframe with the peaks data calculations (start, max ,end)
##   df_merged <- x %>%
##     dplyr::select(-c(position, measurement))
##   df_merged <- dplyr::distinct(df_merged)
##   df_merged <- cbind(df_merged, y)
## }
## 
## #calculate area under the curve of the Test and Control peaks
## 
## auc_calculation <- function(x, y, z, peak) {   #x = the df with the smooth measurements, y = the df with the peak data (start, max, end), z = the strip line, peak = either "T" for test or "C"for control peak
##   # Step 1: Iterate through the dataframe and store values for where the peak starts and ends
##   a_col <- paste("start_", peak, sep = "")
##   b_col <- paste("end_", peak, sep = "")
## 
##   a_vector <- y[, a_col]
##   b_vector <- y[, b_col]
## 
##   # Step 2: Execute a function for each a and b in the second dataframe and store result c
##   c_vector <- vector()
## 
##   for (col_index in 1:length(colnames(x))) {
##     a <- a_vector[col_index]
##     b <- b_vector[col_index]
## 
##     # Get the column name in peaks_smooth corresponding to the current column index
##     col <- colnames(x)[col_index]
## 
##     # Perform auc calculations #x=z y=x love it 'SH'
##     c <- DescTools::AUC(x = z, y = x[[col]], from = z[a], to = z[b],
##                         absolutearea = TRUE, method = "trapezoid")
##     c_vector <- append(c_vector, c)
##   }
## 
##   return(c_vector)
## }
## 
## #If needed, delete outliers from the standard curve
## 
## delete_points <- function(df, curve_number, standard_point) {
## 
##   row_number <- which(stringr::str_detect(df$sample_name, standard_point) & df$curve_name == curve_number) # Get the row number
## 
##   if (length(unique(df$curve_name)) == 2) {
##     # If there are 2 curves, replace the value you want to delete with the value from the other curve
## 
##     a <- dplyr::filter(df,
##                        stringr::str_detect(sample_name,
##                                            standard_point),
##                        curve_name != curve_number) # Select the corresponding value from the other curve
##     df$`T/C`[row_number] <- a$`T/C` # Replace it
## 
##   } else if (length(unique(df$curve_name)) == 3) {
##     # If there are 3 curves, calculate the average of the values you want to keep
## 
##     b <- dplyr::filter(df,
##                        stringr::str_detect(sample_name,
##                                            standard_point),
##                        curve_name != curve_number)
## 
##     b$`T/C` <- as.numeric(b$`T/C`)
## 
##     df$`T/C`[row_number] <- mean(b$`T/C`) # Replace with the average of the other values
##   } else {
##     # When there's only 1 curve, replace with NA
##     df$`T/C`[row_number] <- NA
##   }
## 
##   return(df) # Return the modified dataframe
## }
## 
## 


## ----import data, eval=FALSE------------------------------------------------------------------------------
## 
## #set folder where files to be analyzed are located
## folder_name <- "R:/Eva Iliopoulou/UCPLFCAA/freeBILy CAA-CCA Labrox readings/Labrox readings/230309 UCAA17/UCAA17_Sen + Cam"
## 
## #set name of each file
## import_name_1 <- paste(folder_name,
##                        "20 strips plate_20230309_151426_UCAA_1-20 (S1-S10+860-896)",
##                        sep = "/")
## import_name_2 <- paste(folder_name,
##                        "20 strips plate_20230309_152138_UCAA_21-40 (900-17)",
##                        sep = "/")
## import_name_3 <- paste(folder_name,
##                        "20 strips plate_20230309_152848_UCAA_41-60 (19-42)",
##                        sep = "/")
## import_name_4 <- paste(folder_name,
##                        "20 strips plate_20230309_153649_UCAA_61-80 (44-69)",
##                        sep = "/")
## import_name_5 <- paste(folder_name,
##                        "20 strips plate_20230309_154522_UCAA_81-96 (70-97)",
##                        sep = "/")
## 
## #choose a name for the result files
## file_name <- "UCAA17_Sen + Cam"
## 
## 
## df1 <- readxl::read_excel(paste0(import_name_1, ".xlsx"), #import excel file
##                          sheet = "Well results",
##                          col_names = FALSE)
## df2 <- readxl::read_excel(paste0(import_name_2, ".xlsx"), #import excel file
##                          sheet = "Well results",
##                          col_names = FALSE)
## df3 <- readxl::read_excel(paste0(import_name_3, ".xlsx"), #import excel file
##                          sheet = "Well results",
##                          col_names = FALSE)
## df4 <- readxl::read_excel(paste0(import_name_4, ".xlsx"), #import excel file
##                          sheet = "Well results",
##                          col_names = FALSE)
## df5 <- readxl::read_excel(paste0(import_name_5, ".xlsx"), #import excel file
##                          sheet = "Well results",
##                          col_names = FALSE)
## 
## remove(import_name_1, import_name_2, import_name_3, import_name_4,
##        import_name_5, folder_name)
## 


## ----tidy data--------------------------------------------------------------------------------------------

#extract signal measurements for all dfs
peaks1 <- df_peaks(df1)  
peaks2 <- df_peaks(df2)
peaks3 <- df_peaks(df3)
peaks4 <- df_peaks(df4)
peaks5 <- df_peaks(df5)
peaks <- rbind(peaks1, 
               peaks2, 
               peaks3, 
               peaks4, 
               peaks5
               ) #merge 
peaks <- t(peaks)

df_list <- list(df1, 
                df2, 
                df3, 
                df4, 
                df5
                )

strip_line <- extract_stripline(df1) #extract stripline positions

df_list  <- lapply(df_list, reshape_df, strip_line) #reshape all dataframes in the list in the tidy data format

#add the original df info for each df in the list in a new column
df_list <- lapply(seq_along(df_list), function(i) {
  transform(df_list[[i]], df_number = paste0("df_", i))
}) 

#merge all dfs in the list
df_tidy <- Reduce(full_join,df_list)

df_tidy$strip <- paste(df_tidy$df_number, df_tidy$strip, sep = "_") #create new column with the number of strip + the origin df import

df_tidy$strip <- as.factor(df_tidy$strip)


levels(df_tidy$strip)
colnames(peaks) <- c(levels(df_tidy$strip))

remove(peaks1, peaks2, peaks3, 
       peaks4, peaks5, df1, df2, df3, df4, df5)

#OK TILL HERE -> correct order 


## ----data correction--------------------------------------------------------------------------------------

N <- nrow(peaks)-1 #length of the dataframe, depends on p factor chosen

#smooth peaks
p <- 2 #choose p factor for the rollmean function

peaks_smooth <- data.frame(matrix(nrow=N,ncol=ncol(peaks))) #make empty dataframe to fill in with the smooth data, length depending on the p factor



for (i in seq_along(peaks)) {
  if (i <= ncol(peaks)) {
    output <- zoo::rollmean(peaks[, i], p)
    peaks_smooth[, i] <- output
  } else {
    # Handle the case when i is greater than the number of columns in peaks
      }
}

remove(i, output, p, N)

#Background correction 
peaks_smooth <- stats::na.omit(peaks_smooth)
peaks_smooth <- background_correction(peaks_smooth)

strip_line <- strip_line[-100] #depends on the background correction settings, 1 or 2 stripline points are lost at the end of the strip

colnames(peaks_smooth) <- c(levels(df_tidy$strip))



## ----tidy data smooth-------------------------------------------------------------------------------------
#Make tidy data format dataframe from smooth data after smoothing and background correction
df_tidy_smooth <- reshape_df_smooth(peaks_smooth, strip_line) 


levels(df_tidy_smooth$strip)

df_tidy_smooth <- df_tidy_smooth %>%
  mutate(df_number = case_when(
    str_detect(strip, "df_1_") ~ "df_1",
    str_detect(strip, "df_2_") ~ "df_2",
    str_detect(strip, "df_3_") ~ "df_3",
    str_detect(strip, "df_4_") ~ "df_4",
    str_detect(strip, "df_5_") ~ "df_5"
  ))

standard_curve_strips <- c("df_1_01", "df_1_02", "df_1_03", "df_1_04",
                    "df_1_05", "df_1_06", "df_1_07", "df_1_08", 
                    "df_1_09", "df_1_10")


df_tidy_smooth <- df_tidy_smooth %>%
  dplyr::mutate(sample_id = if_else(strip %in% standard_curve_strips, 
                                    "standard", "test_sample")) %>%
  dplyr::arrange(strip) %>%
  tidyr::unite(sample_id, strip, 
               col = "sample_name", 
               remove = FALSE) 

remove(standard_curve_strips)

#Determine the rows containing standard curve point and assign them the known CAA concentrations
df_tidy_smooth$caa <- NA



df_tidy_smooth <- df_tidy_smooth %>%
  mutate(caa = case_when(
    grepl("standard", sample_id) & str_ends(strip, "1") ~ "10000",
 grepl("standard", sample_id) & str_ends(strip, "2") ~ "3160",
 grepl("standard", sample_id) & str_ends(strip, "3") ~ "1000",
 grepl("standard", sample_id) & str_ends(strip, "4") ~ "316",
 grepl("standard", sample_id) & str_ends(strip, "5") ~ "100",
 grepl("standard", sample_id) & str_ends(strip, "6") ~ "31.6",
 grepl("standard", sample_id) & str_ends(strip, "7") ~ "10",
 grepl("standard", sample_id) & str_ends(strip, "8") ~ "3.2",
 grepl("standard", sample_id) & str_ends(strip, "9") ~ "0",
 grepl("standard", sample_id) & str_ends(strip, "0") ~ "0",
    TRUE ~ NA_character_  # Default case if none of the conditions are met
  ))


print(df_tidy_smooth)



## ----smooth vs raw peaks----------------------------------------------------------------------------------
#visualize all smooth and raw peaks overlaying to see the extend of data correction and compare
#multiple page pdf

pdf_smoothpeaks_name <- paste0("raw_vs_smooth_peaks_", file_name, ".pdf")


pdf(pdf_smoothpeaks_name)

for(i in 1:5) {
  print(ggplot2::ggplot(data = df_tidy_smooth) + 
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
  ggforce::facet_wrap_paginate(~ strip, ncol = 4, nrow = 5, page = i) +
  ggplot2::theme_light() +
  ggplot2::labs(x = "Position in Strip (mm)", 
                y = "Signal Intensity") +
  ggplot2::scale_color_manual(values = c("black", "red"),
                              labels = c("Raw Data", "Smooth Data")))
}
  
dev.off()





## ----control peak detection-------------------------------------------------------------------------------

#detection of CONTROL peak in a selected range of restricted positions

peak_list_ctrl <- vector("list", ncol(peaks_smooth))
for (i in seq_along(peaks_smooth[49:76,])) {
  output <- pracma::findpeaks(peaks_smooth[49:76,i],
                              nups = 2, ndowns = 2, 
                              npeaks = 1,
                              minpeakheight = 100000,
                              threshold = 10,
                              zero = "0"
  )
  peak_list_ctrl[[i]] <- output
}

#replace selected range with real strip line positions
replacement_vector <- 49:76
peak_list_ctrl <- lapply(peak_list_ctrl, function(matrix) {
  replace_position_values(matrix, replacement_vector)
})


#Make graph with control peaks highlighted:
plot_list <- list()

df_tidy_smooth <- df_tidy_smooth %>%
  mutate(
    strip_number = as.numeric(factor(strip, levels = unique(df_tidy_smooth$strip)))
  )

for (i in 1:ncol(peaks_smooth)) {
  # Take values from the peaks_list for the current number 
  peak_test <- peak_list_ctrl[[i]]
  
  # Separate the dataframe containing the group with the corresponding number
  df_test <- dplyr::filter(df_tidy_smooth, 
                           strip_number == i)
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
                                     ncol = 5) 


width <- 8.5  # Adjust as needed
height <- 49  # Adjust as needed

filename <- paste0("control_peaks_", file_name, ".pdf")
ggsave(filename, ctrl_peaks_grid, device = "pdf", width = width, height = height)


remove(p, df_test, output, i, peak_test, 
       ctrl_peaks_grid, width, height, replacement_vector,
       pdf_smoothpeaks_name, plot_list, filename)

#GRAPH WORKS BUT TIDY EVERYTHING UP BECAUSE ITS A MESS, MAYBE TRY PAGES INSTEAD OF SUPER LONG FILE


## ----test peak detection----------------------------------------------------------------------------------
#To find the test peaks in a location determined by its distance from the control peak:

#select the locations of the control peaks and save them into a vector
a <- lapply(peak_list_ctrl,function(mat) mat[, 2])

a <- as.numeric(unlist(a))

b <- a - 40
c <- a - 10

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
                             minpeakheight = 50000
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



## ----re-measure undetected test peaks---------------------------------------------------------------------

#select all matrices that are empty to recheck in the next step
contains_only_na <- function(mat) {
  all(is.na(mat))
}
na_matrices <- sapply(peak_list_test, contains_only_na)

recheck_peaks_index <- which(sapply(na_matrices, function(mat) all(isTRUE(mat))))

recheck_peaks <- peaks_smooth[, recheck_peaks_index] #get the measurements of the strips that a test peak was not detected in



recheck_peak_list_test <- vector("list", ncol(recheck_peaks)) #a new empty list


for (i in seq_along(recheck_peaks[b:c,])) {
  # look for peaks in an alternative way in the strips that they were not detected
  output <- pracma::findpeaks(recheck_peaks[b:c, i],
                              peakpat = "[+]{2,}[-]{1,}[+]{1,}[-]{2,}",
                              npeaks = 1,
                              minpeakheight = 50000
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



## ----peak graphs------------------------------------------------------------------------------------------


#replace with real strip line positions
replacement_vector <- b:c

peak_list_test <- lapply(peak_list_test, function(matrix) {
  replace_position_values(matrix, replacement_vector)
})

#merge control and test peaks detected in 1 list
peaks_list <- lapply(1:length(peak_list_test), 
                     function(i) rbind(peak_list_ctrl[[i]], 
                                       peak_list_test[[i]]))


plots_list <- list()

for (i in 1:ncol(peaks_smooth)) {
  # Take values from the peaks_list for the current number 
  peak_test <- peaks_list[[i]]
  
  # Separate the dataframe containing the group with the corresponding number
  df_test <- dplyr::filter(df_tidy_smooth, 
                           strip_number == i)
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
  
  plots_list[[i]] <- p
  
}



all_peaks_grid <- cowplot::plot_grid(plotlist = plots_list, 
                                     ncol = 5) 


width <- 8.5  # Adjust as needed
height <- 49  # Adjust as needed

filename <- paste0("all_peaks_", file_name, ".pdf")

ggsave(filename, all_peaks_grid, device = "pdf", width = width, height = height)




print(all_peaks_grid)

dev.off()

remove(a, b, c, i, output, peak_test, replacement_vector, 
       p, peak_list_ctrl, peak_list_test, all_peaks_grid,
       filename, height, width, df_test, pdf_smoothpeaks_name,
       pdf_detectedpeaks_name)
remove(na_matrices, selected_columns, i, output)

#GRAPH ALSO OK BUT PEAK DETECTION CRAP IN SOME CASES, RECHECK
#ALSO NEEDS TIDYING UP




## ----auc calculation--------------------------------------------------------------------------------------

#(if first peak too small to be detected) convert 1x4 matrix ("missing" data from peak that was not detected)to 2x4 with NA in the second row
peaks_list <- lapply(peaks_list, fill_matrix) #WAIT TO CHECK ON TEST_PEAK CHANGES!

#extract peak data from generated peak list and reorganize:
peaks_df <- extract_peak_data(peaks_list)

peaks_data <- merge_peak_data(df_tidy_smooth, 
                              peaks_df)


remove(peaks_df)

#Calculate AUC for Each Peak (T and C)
auc_T <- auc_calculation(x = peaks_smooth, 
                         y = peaks_data, 
                         z = strip_line, 
                         peak = "T")
auc_C <- auc_calculation(x = peaks_smooth, 
                         y = peaks_data, 
                         z = strip_line, 
                         peak = "C")

#make dataframe with AUC and T/C and export:

peaks_data <- cbind(peaks_data, auc_T, auc_C)

peaks_data$"T/C" <- peaks_data$auc_T/peaks_data$auc_C

peaks_data_export <- dplyr::select(peaks_data, sample_name, 
                            strip, caa, auc_T, auc_C, `T/C`) 

peaks_data_filename <- paste0("peaks_data_", file_name, ".xlsx")

openxlsx::write.xlsx(peaks_data_export, peaks_data_filename)

remove(df, df_tidy, df_tidy_smooth, peaks, peaks_list, 
       peaks_smooth, strip_line, auc_C, auc_T, peaks_data_filename)

print(peaks_data_export)


## ----standard curve prep----------------------------------------------------------------------------------

#make new dataframe selecting all strips that contain a standard curve measurements (caa != NA) 
peaks_data_standard <- peaks_data_export %>% 
  tidyr::drop_na(caa) %>%
  dplyr::select(sample_name, caa, `T/C`)

#name the different standard curves ("Curve1"/"Curve2"/"Curve3") 
sequence <- rep(1:ceiling(nrow(peaks_data_standard)/10), each = 10, 
                length.out = nrow(peaks_data_standard))
peaks_data_standard$curve_name <- paste0("Curve", sequence)

peaks_data_standard$caa <- as.numeric(peaks_data_standard$caa)

#check the unfitted curve of all standard measurements
ggplot2::ggplot(data=peaks_data_standard, 
                                  aes(x=caa, y=`T/C`)) +
  ggplot2::geom_point(color = "indianred", size = 2, alpha = 0.3) +
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

#plot the unfitted curve with the average standard measurements (no needed if it's only 1 standard curve, it'll be the same as the previous graph)
ggplot2::ggplot(data=standard_df, 
                                     aes(x=CAA, y=`T/C ratio`)) +
  ggplot2::geom_point(color = "indianred", size = 2, alpha = 0.3) +
  ggplot2::scale_x_continuous(trans = 
                                'log10') +
  ggplot2::scale_y_continuous(trans = 
                                'log10') +
  ggplot2::theme_minimal() 





## ----standard curve 4PL-----------------------------------------------------------------------------------

RESP <- standard_df$`T/C ratio`
DOSE <- standard_df$CAA
NAMES  = c("slope","lower","upper","ed50")

a <- drc::drm(standard_df$`T/C ratio` ~ standard_df$CAA,
              data = peaks_data_standard, fct = LL.4(names = NAMES),
              robust = "median") #not sure about this method, can still adapt

#Plot the standard curve made with the averages and compare with individual measurements (different colors)

pdf_standardcurve_name <- paste0("standard_curve_", file_name, ".pdf")

pdf(pdf_standardcurve_name) 

plot(a, col = "steelblue3",
     xlab = "CAA",
     ylab = "T/C",
     pch = 16,
     log = "x") 
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



## ---- echo=FALSE, warning=FALSE---------------------------------------------------------------------------

plot(a, col = "steelblue3",
     xlab = "CAA",
     ylab = "T/C",
     pch = 16,
     log = "x") 
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


## ----outliers exclusion, eval=FALSE-----------------------------------------------------------------------
## peaks_data_standard <- delete_points(df = peaks_data_standard,
##                                      curve_number = "Curve3",  #the standard curve from which you wish to delete a standard T/C value
##                                      standard_point = "9")          #the standard point number you want to delete (eg "8" if it's the 8th point)
## peaks_data_standard <- delete_points(df = peaks_data_standard,
##                                      curve_number = "Curve2",  #the standard curve from which you wish to delete a standard T/C value
##                                      standard_point = "10")
## #repeat for as many points as needed


## ----unknown samples results------------------------------------------------------------------------------
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


cut_off <- 32 #above which the sample is positive/MANUAL INPUT??

results_df <- results_df %>%
  dplyr::mutate(Result = case_when(
    is.nan(`CAA Estimate`) & `T/C Ratio` > curve[1] ~ "above limit of detection",
    is.nan(`CAA Estimate`) & `T/C Ratio` < curve[lower_st] ~ "below limit of detection/negative",
    `CAA Estimate` > cut_off ~ "positive",
    `CAA Estimate` < cut_off ~ "negative"
  ))

results_df$`T/C Ratio` <- round(results_df$`T/C Ratio`, 2)
results_df$`CAA Estimate` <- round(results_df$`CAA Estimate`, 1)

print(results_df)

results_filename <- paste0("results_", file_name, ".xlsx")

openxlsx::write.xlsx(results_df, results_filename)

remove(b, curve, cut_off, DOSE, RESP, lower_st,
       i, NAMES, results_filename, pdf_standardcurve_name,
       pdf_peaks_name, test_sample, peaks_data, peaks_data_standard,
       peaks_data_unknown, sequence)


