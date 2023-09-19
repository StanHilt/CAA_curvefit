install.packages("drc")
install.packages("MASS")
library(drc)
library(MASS)

#---------Reshape Data to Fit Curve---------

#make new dataframe selecting all strips that contain a standard curve measurements (caa != NA) 
peaks_data_standard <- peaks_data_export %>% 
  drop_na(caa) %>%
  dplyr::select(sample_name, caa, `T/C`)

#name the different standard curves ("Curve1"/"Curve2"/"Curve3) 
sequence <- rep(1:ceiling(nrow(peaks_data_standard)/10), each = 10, length.out = nrow(peaks_data_standard))
peaks_data_standard$curve_name <- paste0("Curve", sequence)

#check the unfitted curve of all standard measurements
unfitted_curve <- ggplot(data=peaks_data_standard, aes(x=caa, y=`T/C`)) +
  geom_point() +
  scale_x_continuous(trans = 
                       'log10') +
  scale_y_continuous(trans = 
                       'log10') + #?
  theme_minimal()

#Take the average for each measurement from each standard curve (usually 1 to 3 but can be any number of st curves)
curve <- vector()
for (i in 1:10) {
  a <- peaks_data_standard %>%
    filter(str_ends(sample_name, as.character(i)))  # Convert i to a character
  b <- mean(a$`T/C`)
  curve[i] <- b  # Store the mean value in the appropriate index of the vector c
}

#calculate average T/C corresponding to 0 caa
curve[11] <- (curve[9]+curve[10])/2
curve <- curve[-c(9,10)]

standard_df <- tibble(CAA = peaks_data_standard$caa[1:9], "T/C ratio" = curve)

#plot the unfitted curve with the average standard measurements (no needed if it's only 1 st curve, it'll be the same)
unfitted_curve_av <- ggplot(data=standard_df, aes(x=CAA, y=`T/C ratio`)) +
  geom_point() +
  scale_x_continuous(trans = 
                       'log10') +
  scale_y_continuous(trans = 
                       'log10') +
  theme_minimal()


#---------Fit The Curve-------------

RESP <- standard_df$`T/C ratio`
DOSE <- standard_df$CAA
NAMES  = c("slope","lower","upper","ed50")
#LOWERL = c(-Inf, 0, -Inf, -Inf) ?
#UPPERL = c(Inf, 25, Inf, Inf) ?

a <- drm(standard_df$`T/C ratio` ~ standard_df$CAA,
         data = peaks_data_standard, fct = LL.4(names = NAMES),
         robust = "median") #not sure about this method, can still adapt

#Plot the standard curve made with the averages and compare with individual measurements (different colors)
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


#---------Delete a Data Point (If Needed)-----------------

delete_points <- function(df, curve_number, standard_point) {
  
  row_number <- which(str_detect(df$sample_name, standard_point) & df$curve_name == curve_number) # Get the row number
  
  if (length(unique(df$curve_name)) == 2) { 
    # If there are 2 curves, replace the value you want to delete with the value from the other curve
    
    a <- filter(df,
                str_detect(sample_name, standard_point),
                curve_name != curve_number) # Select the corresponding value from the other curve
    df$`T/C`[row_number] <- a$`T/C` # Replace it
    
  } else if (length(unique(df$curve_name)) == 3) {
    # If there are 3 curves, calculate the average of the values you want to keep
    
    b <- filter(df,
                str_detect(sample_name, standard_point),
                curve_name != curve_number)
    
    b$`T/C` <- as.numeric(b$`T/C`)
    
    df$`T/C`[row_number] <- mean(b$`T/C`) # Replace with the average of the other values
  } else {
    # When there's only 1 curve, replace with NA
    df$`T/C`[row_number] <- NA
  }
  
  return(df) # Return the modified dataframe
}

peaks_data_standard <- delete_points(df = peaks_data_standard,    
                                     curve_number = "Curve1",  #the standard curve from which you wish to delete a standard T/C value
                                     standard_point = "8")          #the standard point number you want to delete (eg "8" if it's the 8th point)

#repeat for as many points as needed

#---------Repeat All Curve Fit Steps --------------
#if a point was deleted in the previous step

#Take the average for each measurement from each standard curve (usually 1 to 3 but can be any number of st curves)
curve <- vector()
for (i in 1:10) {
  a <- peaks_data_standard %>%
    filter(str_ends(sample_name, as.character(i)))  # Convert i to a character
  b <- mean(a$`T/C`)
  curve[i] <- b  # Store the mean value in the appropriate index of the vector c
}

peaks_data_standard

#calculate average T/C corresponding to 0 caa
curve[11] <- (curve[9]+curve[10])/2
curve <- curve[-c(9,10)]

standard_df <- tibble(CAA = peaks_data_standard$caa[1:9], "T/C ratio" = curve)

#plot the unfitted curve with the average standard measurements (no needed if it's only 1 st curve, it'll be the same)
unfitted_curve_av <- ggplot(data=standard_df, aes(x=CAA, y=`T/C ratio`)) +
  geom_point() +
  scale_x_continuous(trans = 
                       'log10') +
  theme_minimal()


#Fit the curve

RESP <- standard_df$`T/C ratio`
DOSE <- standard_df$CAA
NAMES  = c("slope","lower","upper","ed50")
#LOWERL = c(-Inf, 0, -Inf, -Inf) ?
#UPPERL = c(Inf, 25, Inf, Inf) ?


a <- drm(standard_df$`T/C ratio` ~ standard_df$CAA,
         data = peaks_data_standard, fct = LL.4(names = NAMES),
         robust = "median")

#Plot the standard curve made with the averages and compare with individual measurements (different colors)
plot(a, col = "steelblue3",
     xlab = "CAA",
     ylab = "T/C",
     pch = 16) 
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
#REPEAT EACH TIME YOU DELETE A POINT FROM THE STANDARD CURVE AND CHECK THE FITTING


#---------Unknown Test Samples CAA Prediction-----------

#make a new data frame selecting unknown test samples 
peaks_data_unknown <- peaks_data_export %>%
  filter(str_detect(sample_name, "test_sample"))

#predict caa value from T/C value
test_sample <- drc::ED(object = a, respLev = peaks_data_unknown$`T/C`, type = "absolute")

results_df <- cbind(peaks_data_unknown, test_sample) %>%
  select(strip, `T/C`, Estimate, `Std. Error`) 
rownames(results_df) <- NULL
colnames(results_df) <- c("Strip Number", "T/C Ratio", 
                          "CAA Estimate", "Std. Error")

results_df$Result <- NA #make new column for printing the result
lower_st <- tail(which(!is.na(curve)), 1)

results_df <- results_df %>%
  mutate(Result = case_when(
    is.nan(`CAA Estimate`) & `T/C Ratio` > curve[1] ~ "above limit of detection",
    is.nan(`CAA Estimate`) & `T/C Ratio` < curve[lower_st] ~ "below limit of detection/negative",
    `CAA Estimate` > 1 ~ "positive",
    `CAA Estimate` < 1 ~ "negative"
  ))


#---------5 Parameter Curve-------------

#5PL 


#names = c("b", "c", "d", "e", "f")

RESP <- standard_df$`T/C ratio`
DOSE <- standard_df$CAA
NAMES  = c("b", "c", "d", "e", "f")

#b = Minimum asymptote. In a bioassay where you have a standard curve, this can be thought of as the response value at 0 standard concentration.
#c = Hill's slope. The Hill's slope refers to the steepness of the curve. It could either be positive or negative.
#d = Inflection point. The inflection point is defined as the point on the curve where the curvature changes direction or signs. C is the concentration of analyte where y=(D-A)/2.
#e = Maximum asymptote. In an bioassay where you have a standard curve, this can be thought of as the response value for infinite standard concentration.
#f = Asymmetry factor. When E=1 we have a symmetrical curve around inflection point and so we have a four-parameters logistic equation.

a_5 <- drm(standard_df$`T/C ratio` ~ standard_df$CAA,
           data = peaks_data_standard, fct = LL.5(fixed = c(NA, NA, NA, NA, NA), names = NAMES),
           robust = "median")

plot(a_5, col = "steelblue3",
     xlab = "CAA",
     ylab = "T/C",
     pch = 16,
     log = "xy") 


#Residuals comparison try


res_a5 <- residuals(a_5, typeRes = "working")
res_a <- residuals(a, typeRes = "working")

residuals_df <- cbind(res_a, res_a5)
#<- c("caa_1000", "caa_316", "caa_100", "caa_31.5", "caa_10",
#               "caa_3.16", 
#               "caa_1",
#             "caa_0.3", 
#            "caa_0")


residuals_df$caa_point <- c("caa_1000", "caa_316", "caa_100", "caa_31.5", "caa_10",
                            "caa_3.16", 
                            "caa_1",
                            "caa_0.3", 
                            "caa_0")


residuals_df <- pivot_longer(residuals_df, cols = 1:2, names_to ="method",
                             values_to = "residual value")

residuals_df <- as.data.frame(residuals_df)





df_residuals <- residuals_df
df_residuals$`residual value`

ggplot2::ggplot(data = df_residuals, mapping = aes(x = caa_point,
                                                   y = `residual value`,
                                                   fill = method)) +
  geom_bar(stat = "identity", position=position_dodge())




  

