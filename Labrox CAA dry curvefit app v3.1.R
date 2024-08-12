## Salus algorythm Version 1.4

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
library(shinythemes)
library(data.table)
library(DT)

#' defaults
#'
#' Purpose of this function is to centralize all the parameters and
#' make it easier to toggle parameters between being fixed/hidden or visible/editable
#' Simply gets config parameters by name
#'
#' @return list of lists of parameter characteristics (variable=T/F, title=character, description=character, default.value=character or numeric or boolean, choices=vector of same type as default value)
defaults <- function()
{
  params <- list(
    peakAlg=list(
      visible=F,
      title='Peak/AUC Algorithm',
      description='Which algorithm should be used to analyze the data.',
      default.value=1,
      choices=list("Pracma"=0, "Salus" = 1)
    ),
    nSig=list(
      visible=F, # If true, the variable shows for the user. If false, it is hidden and fixed at default value.
      title='Multiples of Sigma',
      description='Multiples of sigma (noise) to subtract from signal.',
      default.value=1
    ),
    
    stripe.width.mm=list(
      visible=T, # If true, the variable shows for the user. If false, it is hidden and fixed at default value.
      title='Stripe Width [mm]',
      description='Width (thickness) of the test and control lines in [mm].',
      default.value=2
    ),
    
    radiobuttons=list(
      visible=T
    )
  )
}

# options(shiny.legacy.datatable = TRUE) # `shiny::renderDataTable()` is deprecated as of shiny 1.8.1. Please use `DT::renderDT()` instead.

roll.min <- function(x, win.width=2, na.rm=T, ...)
{
  library(zoo)
  # This will return a vector of the same size as original and will deal with NAs and optimize for mean.
  return(rollapply(x, width=win.width, FUN=min, na.rm=na.rm, ..., partial=T, align='center'))
}

window.auc <- function(y, win.width=length(y), subtractBase=T, y.precision=1e-9)
{
  library(pracma)
  if(length(y) < win.width)
  {
    # x1 x2    m    b auc peak
    return(data.table(x1=as.double(NA), x2=as.double(NA), m=as.double(NA), b=as.double(NA), peak.max=as.double(NA), peak.x=as.double(NA), auc=as.double(NA)))
  }
  n <- length(y)
  m <- (y[n] - y[1]) / (n - 1) # Calculate the slope of the line through the first and last point
  b <- (y[1] - m*1) # Calculate the intercept of the line through the first and last point
  center <- (n+1)%/%2
  above <- (y - (m*(1:n)+b)) > y.precision # which points are above the line.
  x1 <- tail(which(!above & 1:n <= center), n=1) # find the point preceeding the points consistently above the line left from center
  if((y[x1] - (m*(x1)+b)) < -y.precision)
  {
    # Then don't include point as part of peak
    x1 <- x1 + 1
  }
  x2 <- which(!above & 1:n >= center)[1] # find the point preceeding the points consistently above the line right from center
  if((y[x2] - (m*(x2)+b)) < -y.precision)
  {
    # Then don't include point as part of peak
    x2 <- x2 - 1
  }
  if(length(x1)==0 || length(x2)==0 || x1>=x2) # This implies that a peak is required to be more than 1 point to be considered a real peak
  {
    return(data.table(x1=as.double(NA), x2=as.double(NA), m=as.double(NA), b=as.double(NA), peak.max=as.double(NA), peak.x=as.double(NA), auc=as.double(NA)))
  }
  peak.x <- which.max(y[x1:x2])[1] + (x1-1)
  peak.max <- max(y[x1:x2])
  auc <- pracma::trapz(x1:x2, y[x1:x2])#-pracma::trapz(c(x1,x2), y[c(x1,x2)])
  return(data.table(x1=x1-(center), x2=x2-(center), m=m, b=b, peak.max=peak.max, peak.x=peak.x-(center), auc=auc))
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

df_peaks <- function(x) {
  df_m <- dplyr::filter(x, x$...1 %in% "line1") 
  df_m <- df_m[,-c(1:12)]
  print(df_m)
}

find.peaks <- function(x, y, win.width=15, ...)
{
  library(zoo)
  # This will return a vector of the same size as original and will deal with NAs and optimize for mean.
  ret <- data.table(rollapply(y, width=win.width, FUN=window.auc, win.width=win.width, ..., partial=T, align='center'))
  # Change positions from relative to the window to relative to the y vector index (must be done before removing non-unique peaks to keep y indices correct)
  ret[, ':='(x1=x1+1:.N, x2=x2+1:.N, peak.x=peak.x+1:.N)]
  # Only keep unique peaks
  ret <- unique(ret[auc>0], by=c('x1','x2','peak.x'))
  # Sort them in descending order of auc, then break ties with peak.max and peak.x
  setorder(ret, -auc, -peak.max, peak.x)
  return(ret[])
}

rollMinSubtraction <- function(y, win.width)
{
  return(y-roll.min(y, win.width=win.width))
}

removeNoise <- function(y, nSig=1, thresh=20*mad(y))
{
  return(y-(median(y[y<thresh]) + nSig*mad(y[y<thresh])))
}

replace_position_values <- function(matrix_to_modify, replacement_vector) {
  # Replace values in the first row except for the first column
  matrix_to_modify[1, 2:4] <- replacement_vector[matrix_to_modify[1, 2:4]]
  return(matrix_to_modify)
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
    # Use 'free_x' to make x-axis scale show on each individual plot for easier window setting.
    ggplot2::facet_wrap(~ as.numeric(strip), ncol = 2, scales='free_x') +
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

peak_list_ctrl <- function(peaks_smooth, strip_line, range, excluded, peakAlg, win.width.n) { 
  
  # Find index positions corresponding to mm search positions
  ctrl_peak_range_min_i <- which(strip_line >= range[1])[1]
  ctrl_peak_range_max_i <- tail(which(strip_line <= range[2]), n=1)
  
  if(length(ctrl_peak_range_min_i)==0 || length(ctrl_peak_range_max_i)==0 || is.na(ctrl_peak_range_min_i) || is.na(ctrl_peak_range_max_i) || ctrl_peak_range_min_i==ctrl_peak_range_max_i)
  {
    stop("Data outside limits of peak search parameters. Change advanced settings.")
  }
  peak_list_ctrl <- list() #vector("list", ncol(peaks_smooth))
  for (i in names(peaks_smooth)) {
    if(as.character(i) %in% excluded)
    {
      next
    }
    
    if(peakAlg==0) # Pracma
    {
      output <- pracma::findpeaks(peaks_smooth[ctrl_peak_range_min_i:ctrl_peak_range_max_i, i],
                                  # nups = 2, ndowns = 2, 
                                  # npeaks = 0, # Need this because some saturated peaks won't be found otherwise messing with critical auc calculation (we'll choose the highest)
                                  peakpat = "[+]{,}[0]{0,}[-]{,}", 
                                  minpeakheight = 1,
                                  threshold = 0, # minimum depth of valley between peaks to separate a peak into two peaks
                                  # zero = "+",
                                  sortstr = TRUE # List the highest peak first instead of "first" peak
      )
    }
    else if(peakAlg==1) # Salus
    {
      # win.width.n <- which(abs(strip_line-strip_line[1]) >= getInput()$stripe.width.mm)[1] # Find how many datapoints are needed to fully span wind.width [mm]
      output <- find.peaks(x=ctrl_peak_range_min_i:ctrl_peak_range_max_i,
                           y=peaks_smooth[ctrl_peak_range_min_i:ctrl_peak_range_max_i, i], 
                           win.width = win.width.n)
      output <- matrix(data=c(output$peak.max[1], output$peak.x[1], output$x1[1], output$x2[1]), nrow=1)
    }
    if(is.null(output))
    {
      # browser()
      stop(paste("Control line missing from curve ", i, ". Check raw plots and change advanced settings to detect the control peak or exclude sample.", sep=''))
      # peak_list_ctrl[as.character(i)] <- list(output)
    }
    else
    {
      # print(output)
      output[, 2:4] <- output[, 2:4] + (ctrl_peak_range_min_i-1)
      peak_list_ctrl[[as.character(i)]] <- matrix(output[1,], nrow=1)
    }
  }
  
  # This is done inside the for loop now to adjust the positions back to absolute
  # replacement_vector <-ctrl_peak_range_min_i:ctrl_peak_range_max_i
  # peak_list_ctrl <- lapply(peak_list_ctrl, function(matrix) {
  #   replace_position_values(matrix, replacement_vector)
  # })
  
  return(peak_list_ctrl)
}

test_peak_detect <- function(peak_list_ctrl, peaks_smooth, strip_line, range, excluded, peakAlg, win.width.n){
  
  a <- lapply(peak_list_ctrl,function(mat) mat[, 2])
  a <- as.numeric(as.character(a)) # using as.character instead of unlist avoids dropping of NULL values which results in a vector that is shorter than the original
  names(a) <- names(peak_list_ctrl)
  
  if(any(is.na(a)))
  {
    # This should be caught in the ctrl_peak_detect but putting here for redundancy
    stop(paste0("Control line missing from curve ", which(is.na(a)), ". Check raw plots and change advanced settings to detect the control peak or exclude the sample."))
  }
  
  # Find mm positions that correspond to ctrl peaks
  ctrl_peaks_mm <- strip_line[a]
  
  # Find mm offset positions for range
  test_peak_range_min_mm <- ctrl_peaks_mm + range[1]
  test_peak_range_max_mm <- ctrl_peaks_mm + range[2]
  
  # Find index positions corresponding to mm search positions
  test_peak_range_min_i <- sapply(test_peak_range_min_mm, function(x){which(strip_line >= x)[1]})
  test_peak_range_max_i <- sapply(test_peak_range_max_mm, function(x){tail(which(strip_line <= x), n=1)[1]})
  
  b <- test_peak_range_min_i
  names(b) <- names(peak_list_ctrl)
  c2 <- test_peak_range_max_i # Renamed to c2 so that it can be inspected using the browser() command (c is typically used for "continue" during debug)
  names(c2) <- names(peak_list_ctrl)
  
  # Set the length of the list to 96
  peak_list_test <- list() #vector("list", length = ncol(peaks_smooth))
  
  # Detection of the test peak (searching from left to right)
  for (i in names(peak_list_ctrl)) {
    
    if(peakAlg==0) # Pracma
    {
      output <- pracma::findpeaks(peaks_smooth[b[i]:c2[i],i],
                                  # nups = 2, ndowns = 2, 
                                  # npeaks = 2, # Find two peaks in case the first isn't the hi
                                  peakpat = "[+]{2,}[0]{0,}[-]{2,}",
                                  minpeakheight = 2,
                                  # zero = "-",
                                  sortstr = TRUE # List the highest peak first instead of "first" peak
      )
    }
    else if(peakAlg==1)
    {
      # win.width.n <- which(abs(strip_line-strip_line[1]) >= 2.5)[1] # Find how many datapoints are needed to fully span wind.width [mm]
      output <- find.peaks(x=b[i]:c2[i],
                           y=peaks_smooth[b[i]:c2[i],i], 
                           win.width = win.width.n)
      output <- matrix(data=c(output$peak.max[1], output$peak.x[1], output$x1[1], output$x2[1]), nrow=1)
    }
    if(is.null(output))
    {
      peak_list_test[[as.character(i)]] <- matrix(nrow = 1, ncol = 4)
    }
    else
    {
      output[, 2:4] <- output[, 2:4]+(b[i]-1)
      peak_list_test[[as.character(i)]] <- matrix(output[1,], nrow=1)
    }
  }
  
  # I found some odd behavior with the all(is.na(mat)) code that was creating issues and
  # probably why this bit was here as some NULL list entries were being lost. I think
  # the above way seems to be more robust, handling null values as they arise.
  # #replace NULL matrices with 1x4 matrices filled with NA to enable later analysis
  # peaks
  # for (i in seq_along(peaks_smooth)) {
  #   if (i <= length(peak_list_test)) {
  #     if (is.null(peak_list_test[[as.character(i)]])) {
  #       peak_list_test[[as.character(i)]] <- matrix(nrow = 1, ncol = 4)
  #     }
  #   } else {
  #     peak_list_test[[as.character(i)]] <- matrix(nrow = 1, ncol = 4) #add an empty matrix to the end of the list if it was deleted due to being NULL
  #   }
  # }
  
  matrix_contains_only_na <- function(x) {
    is.matrix(x) && all(is.na(x))
  }
  na_matrices <- lapply(peak_list_test, matrix_contains_only_na)
  
  recheck_peaks_index <- names(na_matrices)[which(na_matrices==TRUE)] #as.vector(which(sapply(na_matrices, function(mat) all(isTRUE(mat)))))
  
  recheck_peaks <-peaks_smooth[recheck_peaks_index] #get the measurements of the strips that a test peak was not detected in
  
  recheck_peak_list_test <- list() #vector("list", ncol(recheck_peaks)) #a new empty list
  
  for (i in names(recheck_peaks)) {
    # look for peaks in an alternative way in the strips that they were not detected
    if(peakAlg==0) # Pracma
    {
      output <- pracma::findpeaks(recheck_peaks[b[i]:c2[i],i],
                                  peakpat = "[+]{1,}[0]{0,}[-]{1,}",
                                  # npeaks = 1,
                                  minpeakheight = 2,
                                  # zero = "-",
                                  sortstr = FALSE # List the highest peak first instead of the "first" peak
      )
    }
    else if(peakAlg==1)
    {
      # win.width.n <- which(abs(strip_line-strip_line[1]) >= 2.5)[1] # Find how many datapoints are needed to fully span wind.width [mm]
      output <- find.peaks(x=b[i]:c2[i],
                           y=recheck_peaks[b[i]:c2[i],i], 
                           win.width = win.width.n)
      output <- matrix(data=c(output$peak.max[1], output$peak.x[1], output$x1[1], output$x2[1]), nrow=1)
    }
    
    # Check if output is not NULL before assigning
    if(is.null(output))
    {
      recheck_peak_list_test[[as.character(i)]] <- matrix(nrow = 1, ncol = 4)
    }
    else
    {
      output[, 2:4] <- output[, 2:4]+(b[i]-1)
      recheck_peak_list_test[[as.character(i)]] <- matrix(output[1,], nrow=1)
    }
    
    # Print the values to debug
    cat("test peak i:", i, "\n")
    print(output)
  }
  
  #replace the empty matrices with peak data with the new ones if a peak was detected
  if(length(recheck_peaks_index) > 0)
  {
    recheck_peak_list_test[recheck_peaks_index] <- recheck_peak_list_test
  }
  
  
  remove(recheck_peak_list_test, recheck_peaks_index, recheck_peaks)
  
  # This is done inside the for loop now to adjust positions back to absolute
  # each curve
  # replacement_vector <- b:c
  # 
  # peak_list_test <- lapply(peak_list_test, function(matrix) {
  #   replace_position_values(matrix, replacement_vector)
  # })
  
  #merge control and test peaks detected in 1 list
  peaks_list <- lapply(names(peak_list_ctrl), 
                       function(sampleName) rbind(peak_list_ctrl[[sampleName]], 
                                                  peak_list_test[[sampleName]]))
  names(peaks_list) <- names(peak_list_ctrl)
  return(peaks_list)
}

plot_list_all <-function(peaks_smooth, df_tidy_smooth, peaks_list, peak_list_ctrl){
  #determine range for shading test peak detect
  plots_list <- list()
  for (i in names(peak_list_ctrl)) {
    
    strip.i <- which(names(peak_list_ctrl)==i)
    
    # Take values from the peaks_list for the current number 
    peak_test <- peaks_list[[i]]
    
    # Separate the dataframe containing the group with the corresponding number
    df_test <- dplyr::filter(df_tidy_smooth, 
                             strip == strip.i)
    # Convert peak_test and peak_points to simple vectors
    peak_test <- as.vector(peak_test)
    
    C.peak <- NULL
    T.peak <- NULL
    # Shading peak areas
    if(!is.na(peak_test[1]))
    {
      C.peak <- df_test[peak_test[5]:peak_test[7],c('position','measurement')]
      C.peak <- rbind(C.peak, data.frame(position=c(tail(C.peak$position, n=1), C.peak$position[1]), measurement=c(0,0)))
      C.peak$peak <- 'Control'
    }
    if(!is.na(peak_test[2]))
    {
      T.peak <- df_test[peak_test[6]:peak_test[8],c('position','measurement')]
      T.peak <- rbind(T.peak, data.frame(position=c(tail(T.peak$position, n=1), T.peak$position[1]), measurement=c(0,0)))
      T.peak$peak <- 'Test'
    }
    peak_areas <- rbind(C.peak, T.peak)
    
    # Create a plot for the current number and store it in the list
    p <- ggplot2::ggplot(df_test, aes(x = position, 
                                      y = measurement)) +
      ggplot2::geom_line(linewidth = 0.5, alpha = 1) +
      ggplot2::geom_point(data = cbind(df_test[peak_test[3:8], ], data.frame(peak=c('Control','Test'))), 
                          aes(x=position, y=measurement, color=peak), size = 1.6, alpha = 1) +
      ggplot2::labs(title = paste("strip", strip.i),
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
      {if(!is.null(peak_areas)){ggplot2::geom_polygon(data = peak_areas, aes(x=position, y=measurement, fill=peak))}} +
      ggplot2::scale_y_continuous(labels = scales::scientific) +
      theme(legend.position="none")
    
    
    plots_list[[as.character(strip.i)]] <- p
    
  }
  
  all_peaks_grid <- cowplot::plot_grid(plotlist = plots_list, 
                                       ncol = 2) #all peaks in a grid
  
  print(all_peaks_grid)
}


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

extract_peak_data <- function(x) {
  # Create an empty tibble with desired column names
  test_df <- dplyr::tibble(
    strip = NA,
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
  rowCount <- 1
  for (i in names(x)) {
    # Get the current dataframe from the list
    current_df <- x[[i]]
    
    # Calculate the row index for the current dataframe
    # row_index <- (i - 1) * 2 + 1
    
    # Assign values from the current dataframe to corresponding columns in test_df
    test_df[rowCount, "strip"] <- as.numeric(which(names(x)==i)) # Add strip number so that we can merge later and be sure nothing gets off via indicies
    test_df[rowCount, "height_T"] <- current_df[2, 1]
    test_df[rowCount, "max_T"] <- current_df[2, 2]
    test_df[rowCount, "start_T"] <- current_df[2, 3]
    test_df[rowCount, "end_T"] <- current_df[2, 4]
    test_df[rowCount, "height_C"] <- current_df[1, 1]
    test_df[rowCount, "max_C"] <- current_df[1, 2]
    test_df[rowCount, "start_C"] <- current_df[1, 3]
    test_df[rowCount, "end_C"] <- current_df[1, 4]
    rowCount <- rowCount + 1
  }
  
  ## No longer needed
  # # Group the rows in test_df by pairs
  # test_df <- test_df %>%
  #   dplyr::group_by(grp = (row_number() - 1) %/% 2) %>%
  #   dplyr::summarize_all(~if (is.numeric(.)) {
  #     sum(., na.rm = TRUE)
  #   } else {
  #     first(.)
  #   }) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(-grp)
  
  return(test_df)
}

merge_peak_data <- function(x, y){ #x = the tidy dataframe, y = the dataframe with the peaks data calculations (start, max ,end) 
  df_merged <- x %>% 
    dplyr::select(-c(position, measurement))
  df_merged <- dplyr::distinct(df_merged)
  df_merged <- left_join(df_merged, y, by = 'strip') # join by 'id'/name rather than index to be more robust.
}

# win.width in [mm]
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

name_curves <- function(peaks_data_standard){
  sequence <- rep(1:ceiling(nrow(peaks_data_standard)/10), each = 10, 
                  length.out = nrow(peaks_data_standard))
  peaks_data_standard$curve_name <- paste0("Curve", sequence)
  return(as.data.frame(peaks_data_standard))
}

rawplot <- function(peaks_data_standard){
  p <- ggplot2::ggplot(data=peaks_data_standard, 
                       aes(x=caa, y=`T/C`)) +
    ggplot2::geom_point(color = "indianred", size = 2, alpha = 0.3) +
    ggplot2::scale_x_continuous(trans = 
                                  'log10') +
    ggplot2::scale_y_continuous(trans = 
                                  'log10') + 
    ggplot2::theme_minimal() 
  
  return(p)
}

std_df <- function(peaks_data_standard){
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
  NAMES  = c("slope","lower","upper","ed50")
  # This was ambiguous as to which data was actually being modeled. Upon digging
  # it looks like only mean data is being fit, reducing the number of datapoints
  # by 1 because the 0 is the only concentration with a replicate. However, the
  # weight of the 0 point is equal to the weight of the next two concentrations
  # instead of double due to double the data.
  # This code was left into keep things consistent.
  a <- drc::drm(standard_df$`T/C ratio` ~ standard_df$CAA,
                data = peaks_data_standard, fct = LL.4(names = NAMES),
                weights = c(20,10,10,30,30,200,300,500,500),
                #robust = "mean",
                lowerl = c(NA, 0, NA, NA))
  
  #  # This fits all the data points (i.e., each replicate is a separate datapoint)
  #  # and given there are twice as many datapoints at zero, each of the zeros
  #  # datapoints are given a weight such that the total is the same as the total
  #  # used in the above code (i.e., 2.5+2.5=5) to be equivalent.
  #  # Given the note "not sure about this method" I added this code for
  #  # consideration. It also adds some more specificity around bounds for 
  #  # improved ability to converge on a solution.
  #   a <- drc::drm(`T/C` ~ caa,
  # 	            data=peaks_data_standard, fct = LL.4(names = NAMES),
  # 	            weights = c(2.5, 2.5, 5, 5, 30, 30, 200, 300, 500, 500)[(((1:nrow(peaks_data_standard))-1)%%10)+1], # Roughly (LOD/(CAA+5)) could be used with peaks_data_standard instead
  # 	            robust='median',
  #   				   lowerl = c(-Inf, 0, .Machine$double.eps, .Machine$double.eps), # Setting all limits helps convergence
  #   				   upperl = c(-1*.Machine$double.neg.eps,Inf,Inf,Inf)) # Setting all limits helps convergence
  
  return(a)
}

curvefitplot <- function(a, peaks_data_standard, assay){
  if(assay %in% c(1,3)){
    fitted_curve <- plot(a, col = "steelblue3",
                         xlab = "CAA",
                         ylab = "T/C",
                         ylim = c(0.000001, max(na.omit(peaks_data_standard$`T/C`))),
                         pch = 16,
                         log = "xy") 
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
  }else if(assay %in% c(0,2,4)){
    fitted_curve <- plot(a, col = "steelblue3",
                         xlab = "CAA",
                         ylab = "T/C",
                         ylim = c(0.000001, max(na.omit(peaks_data_standard$`T/C`))),
                         pch = 16,
                         log = "xy") 
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
  }else{
    stop("Unknown assay!")
  }
}

cutoffcalc <- function(a, peaks_data_standard){
  negatives <- mean(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0]) + 
    2 * sd(peaks_data_standard$`T/C`[peaks_data_standard$caa == 0])
  cutoff <- drc::ED(
    object = a,
    respLev = negatives,
    type = "absolute"
  )[1,1]
  return(cutoff)
}

resultfile <- function(peaks_data_export, a, peaks_data_standard, exclude, assay){
  peaks_data_unknown <- peaks_data_export %>%
    dplyr::filter(str_detect(sample_id, "test_sample"))
  
  #predict caa value from T/C value
  test_sample <- drc::ED(object = a, 
                         respLev = as.vector(peaks_data_unknown$`T/C`), 
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
    10 #adjust to dry please
  }else{
    if(assay == "2"){
      1
    }else{
      if(assay == "3"){
        30
      }else{
        if(assay == "4"){
          3
          }
        }
      }
    }#above which the sample is positive/MANUAL INPUT??
  
  results_df <- 
    dplyr::mutate(results_df, Result = case_when(
      is.nan(`CAA Estimate`) & `T/C Ratio` > curve[lower_st] ~ "High positive, above HLOD",
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

#' if.else
#'
#' Simple helper function to reduce code
#'
#' @param test boolean whether to return yes parameter or no parameter
#' @param yes anything
#' @param no anything
if.else <- function(test, yes, no)
{
  if(test)
  {
    return(yes)
  }
  else
  {
    return(no)
  }
}

#' getDefault
#'
#' Simple helper function to test for invalidness of a value according to a user-
#' supplied test function. Return the default value if the passed value is true
#' by the user-provided test (default function - 'is.null'). Otherwise, simply
#' return the value provided.
#'
#' @param x value to 
#' @param yes anything
#' @param no anything
getDefault <- function(x, default, test=is.null)
{
  ret <- copy(x)
  result <- test(x)
  if(length(result) > 1)
  {
    ret[result] <- default
  }
  else
  {
    if(result)
    {
      ret <- default
    }
  }
  # ret[test(x)] <- default
  return(ret)
}

addcaa <- function(full_data, a, radio) {
  if (radio == 0) {
    return(full_data)
      }else{
  #predict caa value from T/C value
  test_sample <- drc::ED(
    object = a,
    respLev = as.vector(full_data$`T/C`),
    type = "absolute"
  )
  
  full_data_caa <- cbind(full_data, test_sample[, 1, drop = F])
  rownames(full_data_caa) <- NULL
  full_data_caa <- dplyr::rename(full_data_caa, "CAA Estimate" = "Estimate")
  return(full_data_caa)
  }
}
#### Define UI for application ####
ui <- fluidPage(
  
  # Application title
  titlePanel("Labrox dry CAA analysis v3 (Salus Algorithm)"),
  
  # Sidebar with a slider input for number of bins 
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
      selectInput(
        "assay", label = h4("Select Assay"), 
        choices = list("UCAAhT17 cutoff: 10pg/ml" = 1, "UCAAhT417 cutoff: 1pg/ml" = 2, "SCAA20 cutoff: 30pg/ml" = 3, "SCAA500 cutoff: 3pg/ml" = 4), 
        selected = 1
      ),
      actionButton("startAnalysis", "Start Analysis", class = "btn-primary"),
      actionButton("Restart", "Reload software", class  = "btn-warning"),
      actionButton("Close", "Close software", class = "btn-danger"),
      uiOutput("getoptionsUI"),
      
      checkboxInput("Show_options", "Show advanced options:", value = FALSE),
      
      conditionalPanel(
        condition = "input.Show_options",
        uiOutput("getAdvancedOptionsUIElements")
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Background Correction", 
                 fluidRow(
                   column(10, plotOutput("rawvssmooth")),
                   column(2, downloadButton("downloadrawvssmooth", "Download plot")),
                   verbatimTextOutput("x"))
        ),
        tabPanel("Peak Detection", 
                 fluidRow(
                   column(10, plotOutput("plotlistall")),
                   column(2, downloadButton("downloadPeaks", "Download plot"))
                 )
        ),
        tabPanel("Average curve", 
                 fluidRow(
                   column(10, plotOutput("standardaverage", height = "800px", width = "800px")),
                   column(2, downloadButton("downloadavgcurve", "Download plot"))
                 )
        ),
        tabPanel("Fitted curve", 
                 fluidRow(
                   column(10, plotOutput("plotfitcurve", height = "800px", width = "800px")),
                   column(2, downloadButton("downloadcurve", "Download plot")),
                   column(10, verbatimTextOutput("cutoffs"))
                 )
        ),
        tabPanel("Results", 
                 fluidRow(
                   column(2, downloadButton("Downloadresults", "Download results as CSV")),
                   column(1, downloadButton("Downloadexcelresults", "Download results as Excel"))),
                 fluidRow(
                   column(10, dataTableOutput("results")), 
                 )),
        tabPanel("Full Data",
                 fluidRow(
                   column(2, downloadButton("Downloadfullresults", "Download results as CSV")),
                   column(1, downloadButton("Downloadexcelfullresults", "Download results as Excel"))
                 ),
                 fluidRow(
                   column(5, dataTableOutput("full")))
                 
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$startAnalysis, {
    
    options(shiny.maxRequestSize=10*1024^2) # Default file size limit is 5 MB. This changes it to 30 MB
    
    currentShowVar <- FALSE
    
    currentShow <- reactiveVal(currentShowVar)
    
    # Helper function to either get the input value from the UI (if available) or from the list of default values.
    getInput <- reactive({
      return(list(
        peakAlg=getDefault(input$peakAlg, defaults()$peakAlg$default.value),
        nSig=getDefault(input$nSig, defaults()$nSig$default.value),
        stripe.width.mm=getDefault(input$stripe.width.mm, defaults()$stripe.width.mm$default.value)
      ))
    })
    
    excluded <- reactive({
      if('exclude' %in% names(input))
      {
        return(unlist(strsplit(input$exclude, ",")))
      }
      else
      {
        return(character(0))
      }
    })
    
    test_samples <- reactive({
      n <- as.character(1:ncol(peaks_smooth()))
      if(input$radio > 0){
        n <- as.character(if (input$radio == 1 & ncol(peaks_smooth()) <= 10){
          NA
        }else{
          if (input$radio == 1) {
            11:ncol(peaks_smooth())
          }else{
            if (input$radio == 2 & ncol(peaks_smooth()) <= 20) {
              NA
            }else{
              if (input$radio == 2) {
                21:ncol(peaks_smooth())
              }else{
                if (input$radio == 3 & ncol(peaks_smooth()) <= 30) {
                  NA
                }else{
                  if (input$radio == 3) {
                    31:ncol(peaks_smooth())
                  }
                }
              }
            }
          }
        })
      }
      print(n)
      return(n)
    })
    
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
    
    df <- reactive({
      req(input$file1)
      print(input$file1)# Ensure input$file is available
      isolate(currentShow(input$Show_options)) # Quietly store the current value of 'Show_options'
      updateCheckboxInput(session, 'Show_options', value=TRUE)
      dfs <- lapply(df_list(), df_peaks)
      df <- do.call(rbind, dfs)
      df[] <- lapply(df, as.numeric)
      df<- t(df)
      return((as.data.frame(df)))
    })
    
    strip_line <- reactive({
      req(df_list())
      l <- extract_stripline(df_list()[[1]])
      print(l)
      return(t(l))
    })
    
    df_clean <- reactive({
      if(all(grepl('Y..', names(df())[], fixed=T)))
      {
        # Do as usual and rename the columns
        new_names <- (1:(ncol(df())))
        peaks <- df()[1:ncol(df())] %>% set_names(new_names)
      }
      else
      {
        # Keep the column names
        peaks <- df()[1:ncol(df())]
      }
      df_clean <- cbind(strip_line(), peaks)
    })
    
    df_tidy <- reactive({
      colNames <- names(df_clean())[2:ncol(df_clean())]
      df_tidy <- gather(df_clean(), "strip", "value", 2:ncol(df_clean()))
      df_tidy$strip <- match(df_tidy$strip, colNames) # Get by name instead of index
      print(df_tidy)
    })
    
    df_tidy_smooth <- reactive({
      df <- peaks_smooth_tidy() %>%
        dplyr::mutate(sample_id = if_else(strip %in% test_samples(),
                                          "test_sample", "standard")) %>%
        dplyr::arrange(strip) %>%
        tidyr::unite(sample_id, strip,
                     col = "sample_name",
                     remove = FALSE)
      
      if(input$assay %in% c(0,2,4)) {
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
      }else if(input$assay %in% c(1,3)){
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
      } else{
        df$caa <- as.double(NA)
      }
      return(df)
    })
    
    peaks_smooth <- reactive({
      rolled_min <-
        rollMinSubtraction(df_clean()[2:ncol(df_clean())], win.width.n())
      smoothed <- transmute_all(rolled_min, removeNoise, nSig=getInput()$nSig)
      smoothed[smoothed < 0] <- 0
      return(smoothed)
    })
    
    peaks_smooth_tidy <- reactive({
      peaks_smooth_tidy <- cbind(strip_line(), peaks_smooth())
      colNames <- names(peaks_smooth_tidy)[2:ncol(peaks_smooth_tidy)]
      peaks_smooth_tidy <-
        gather(peaks_smooth_tidy,
               "strip",
               "value",
               2:ncol(peaks_smooth_tidy))
      peaks_smooth_tidy$strip <- match(peaks_smooth_tidy$strip, colNames) # robustly number according to name	
      peaks_smooth_tidy <-
        rename(peaks_smooth_tidy,
               c("position" = "strip_line()", "measurement" = value))
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
      req(strip_line(), input$range)
      df <- peak_list_ctrl(peaks_smooth(), strip_line(), input$range, excluded(), getInput()$peakAlg, win.width.n())
      print(df)
    })
    
    peaks_list <- reactive({
      df <- test_peak_detect(peak_list_control(), peaks_smooth(), strip_line(), input$range2, excluded(), getInput()$peakAlg, win.width.n())
      print(df)
    })
    
    output$plotlistall <- renderPlot(
      plot_list_all(peaks_smooth(), df_tidy_smooth(), peaks_list(), peak_list_control()),
      width = 1000,
      height = function()
        150 * ncol(df_clean() / 2)
    )
    
    # No longer needed
    # peaks_list_filled <- reactive({
    #   lapply(peaks_list(), fill_matrix)
    # })
    
    peaks_df <- reactive({
      extract_peak_data(peaks_list()) # No longer need peaks_list_filled()
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
      data <- cbind(peaks_data(), data.frame(auc_T=auc_T(), auc_C=auc_C()))
      data$"T/C" <- as.numeric(data$auc_T / data$auc_C)
      tempNames <- names(data)
      data$sample_note <- names(df()[2:ncol(df())])[as.numeric(data$strip)]
      return(data[, c('sample_note', tempNames)]) # Make 'sample_note' the first column
    })
    
    #Standard curve
    peaks_data_standard <- reactive({
      if(input$radio > 0)
      {
        # Only make standard curves if standards are present
        peaks_data_auc() %>%
          tidyr::drop_na(caa) %>%
          dplyr::select(sample_name, caa, `T/C`)
      }
    })
    
    peaks_data_standard_curves <-reactive({
      req(peaks_data_standard())
      (name_curves(peaks_data_standard()))
    })
    
    peaks_data_standard_names <- reactive({
      req(peaks_data_standard_curves())
      print(input$rmstandard)
      delete_points(df = peaks_data_standard_curves(), input$rmstandard)
    })
    
    output$standardcurve <-
      renderPlot({
        req(peaks_data_standard_names())
        rawplot(peaks_data_standard_names())
      })
    
    standard_df <- reactive({
      req(peaks_data_standard_names())
      std_df(peaks_data_standard_names())
    })
    
    output$standardaverage <-
      renderPlot({
        req(standard_df())
        averageplot(standard_df())},
        width = 800,
        height = 800)
    
    
    curvemodel <- reactive({
      req(df(), peaks_data_standard())
      model(standard_df(), peaks_data_standard())
    })
    
    output$plotfitcurve <- renderPlot({
      req(df(), curvemodel(), peaks_data_standard_names())
      curvefitplot(curvemodel(), peaks_data_standard_names(), input$assay)
    })
    
    cutoff <-
      reactive({
        req(curvemodel(), peaks_data_standard_names())
        cutoffcalc(curvemodel(), peaks_data_standard_names())
      })
    
    output$cutoffs <-
      renderText({
        req(cutoff())
        c(paste0(
          "Calculated cutoff = ", round(cutoff(), digits = 4), "pg/ml"
        ))})
    
    value <- reactive({
      numbers <- gsub(" ", "", input$exclude)
      numbers <- unlist(strsplit(numbers, ","))
      numbers <- as.numeric(numbers)
      return(numbers)
    })
    
    results <-
      reactive({
        req(peaks_data_standard_names(), peaks_data_auc(), curvemodel())
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
    
    # Function to convert from mm to an equivalent number of rows in the data
    win.width.n <- reactive({
      req(strip_line())
      return(which(abs(strip_line()-strip_line()[1]) >= getInput()$stripe.width.mm)[1])
    })
    
    # In general, inputs that are not rendered cannot be accessed and do not
    # update if they are added from the server side like I'm doing here. If they
    # are directly added as part of the "conditionalPanel" statement on the UI
    # side, they remain accessible but they cannot take any information from the
    # uploaded dataset to help set their parameters (e.g., the min and max of
    # sliders for where to find peaks). I wanted to enable the ability to set
    # slider limits based on the data to avoid errors. So, when new data is
    # loaded, I reset the slider limits. To do so, I use a "local" server side
    # variable to save the current 'Show_options' checkbox value and then
    # temporarily and programmatically set 'Show_options' TRUE, so the slider is
    # accessble for updates. I then update the UI element using the updated data
    # and then programmatically reset the checkbox to what it was. If it was
    # hidden initially (which is the default), this process temporarily shows the
    # options and then immediately hides them from the user again. If they were
    # originally visible, they stay visible. This new approach avoids use of
    # inappropriate slider ranges with newly imported data.
    
    output$getoptionsUI <- renderUI({
      if.else(defaults()$radiobuttons$visible,
              radioButtons(
                "radio", label = h4("Select number of standard curves"),
                choices = list("0" = 0, "1" = 1, "2" = 2, "3" = 3), 
                selected = 1
              ), NULL)
    })
    
    output$getAdvancedOptionsUIElements <- renderUI({
      req(strip_line())
      bounds <- range(strip_line())
      right <- 29
      left <- 23
      ret <- tagList(
        if.else(defaults()$nSig$visible,
                numericInput(
                  "nSig",
                  label = h4("Multiple of sigma (noise) to substract from signal."),
                  min = -3, max = 3, value = defaults()$nSig$default.value),
                NULL
        ),
        if.else(defaults()$peakAlg$visible,
                selectInput(
                  "peakAlg", label = h4(defaults()$peakAlg$title), 
                  choices = defaults()$peakAlg$choices, 
                  selected = defaults()$peakAlg$default.value),
                NULL
        ),
        sliderInput(
          "range", 
          label = h4("Control peak search range [mm] (absolute)"),
          min = min(bounds), max = max(bounds), value = round(c(left, right), 1), step=0.1
        ),
        sliderInput(
          "range2", 
          label = h4("Test peak search range [mm] (offset from control)"),
          min = -10, max = -0.5, value = c(-7.5,-3), step=0.1
        ),
        textInput(
          "exclude", 
          label = h4("Enter strip numbers to exclude, separated by commas")
        ),
        textInput(
          "rmstandard", 
          label = h4("Enter standard curve and number to exclude, e.g Curve1_1, Curve2_3")
        ),
        if.else(defaults()$stripe.width.mm$visible,
                numericInput("stripe.width.mm",
                             h4(defaults()$stripe.width.mm$title),
                             value = defaults()$stripe.width.mm$default.value),
                NULL
        )
      )
      
      isolate(updateCheckboxInput(session, "Show_options", value=currentShow()))
      
      return(ret)
    })
    
    output$results <- renderDataTable(results(), options=list(pageLength=100))
    output$all <- renderText(strip_line())
    
    full_caa <- reactive({addcaa(peaks_data_auc(), curvemodel(), input$radio)})
    
    output$full <- renderDT(DT::datatable(
      full_caa()[-1], 
      options=list(pageLength=100))%>%
        formatRound(c(5, 9, 13, 14), 0, mark = "")%>%
        formatRound(15, 4, mark = "")%>%
        formatRound(16, 2, mark = ""))
    
    #download raw vs smooth
    
    output$downloadrawvssmooth = downloadHandler(
      filename=function(){paste0(gsub(".xlsx", "", input$name), "_raw_vs_smooth.pdf")},
      content=function(file){
        ggsave(file, peaks_graph_smooth(df_tidy(), df_tidy_smooth()), width = 6*800,
               height = (ncol(df_clean() / 2))*600,
               units = "px",
               limitsize = F)
      }
    )
    
    
    output$downloadPeaks = downloadHandler(
      filename=function(){paste0(gsub(".xlsx", "", input$name), "_peaks.pdf")},
      content=function(file){
        ggsave(file, plot_list_all(peaks_smooth(), df_tidy_smooth(), peaks_list(), peak_list_control()), width = 6*800,
               height = (ncol(df_clean() / 2))*600,
               units = "px",
               limitsize = F)
      }
    )
    
    output$downloadavgcurve = downloadHandler(
      filename = function() {
        paste0(gsub(".xlsx", "", input$name), "_average_curve.png")
      },
      content = function(file) {
        dpi <- 200  # Set the desired DPI
        width_inches <- 8  # Set the desired width in inches
        height_inches <- 6 # Set the desired height in inches
        
        width_pixels <- dpi * width_inches
        height_pixels <- dpi * height_inches
        
        png(
          file,
          width = width_pixels,
          height = height_pixels,
          units = "px",
          res = dpi
        )
        withProgress(message = "Downloading", value = 0, {
          print(averageplot(standard_df()))
        })
        dev.off()
      }
    )
    
    output$downloadcurve = downloadHandler(
      filename = function() {
        paste0(gsub(".xlsx", "", input$name), "_fitted_curve.png")
      },
      content = function(file) {
        dpi <- 200  # Set the desired DPI
        width_inches <- 8  # Set the desired width in inches
        height_inches <- 6 # Set the desired height in inches
        
        width_pixels <- dpi * width_inches
        height_pixels <- dpi * height_inches
        
        png(
          file,
          width = width_pixels,
          height = height_pixels,
          units = "px",
          res = dpi
        )
        withProgress(message = "Downloading", value = 0, {
          print(curvefitplot(curvemodel(), peaks_data_standard_names(), input$assay))
        })
        dev.off()
      }
    )
    
    #download results
    output$Downloadresults = downloadHandler(
      filename = function() {
        paste0(gsub(".xlsx", "", input$name), "_results.csv")
      },
      content = function(filename) {
        write.csv(results(), filename)
      }
    )
    
    output$Downloadexcelresults = downloadHandler(
      filename = function() {
        paste0(gsub(".xlsx", "", input$name), "_results.xlsx")
      },
      content = function(filename) {
        write.xlsx(results(), filename)
      }
    )
    
    #download full results
    output$Downloadfullresults = downloadHandler(
      filename = function() {
        paste0(gsub(".xlsx", "", input$name), "_fullresults.csv")
      },
      content = function(filename) {
        write.csv(peaks_data_auc()[-1], filename)
      }
    )
    
    output$Downloadexcelfullresults = downloadHandler(
      filename = function() {
        paste0(gsub(".xlsx", "", input$name), "_fullresults.xlsx")
      },
      content = function(filename) {
        write.xlsx(peaks_data_auc()[-1], filename)
      }
    )
  })
  
  observeEvent(input$Restart,{
    session$reload()
  })
  
  observeEvent(input$Close,{
    stopApp()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
