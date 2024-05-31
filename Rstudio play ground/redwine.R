
library("e1071")
##################################
# ! Question 1 - Understand the Data
##################################
myData <- read.table("RedWine.txt")
# names(myData) <- c("citric-acid", "CL-", "total-SO2 ", "pH", "alcohol", "quality")

data.raw <- as.matrix(myData)

# 224279168
set.seed(220090063) # using your student ID number for reproducible sampling with the seed function

data.subset <- data.raw[sample(1:1599, 500), c(1:6)]
data.variable.names <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol")


# Create 5 scatterplots function (for each X variable against the variable of interest Y)
q1_scatterplots_ <- function(x) {
  plot(data.subset[, x],
    data.subset[, 6],
    xlab = data.variable.names[x],
    ylab = "quality",
    main = paste("Scatterplot of", data.variable.names[x], "vs quality")
  )
}

par(mfrow = c(1, 1))
# par(mfrow = c(2, 3))
for (i in 1:5) {
  q1_scatterplots_(i)
}

# Create 6 histograms for each X variable and Y
q1_histograms_ <- function(x) {
  if (x == 6) {
    xAsix <- "quality"
    mainTitle <- "histograms of quality"
  } else {
    xAsix <- data.variable.names[x]
    mainTitle <- paste("histograms of", data.variable.names[x], "vs quality")
  }
  hist(data.subset[, x],
    xlab = xAsix,
    main = paste(mainTitle),
    col = "blue"
  )
}

# par(mfrow = c(3, 3))
for (i in 1:6) {
  q1_histograms_(i)
}
par(mfrow = c(1, 1))

################################
# ! Question 2 - Transform the Data
################################
################################
# ! Functions for question 2
################################
# A Min-Max and/or Z-score transformation should then be used to adjust the scale of each variable
minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# z-score standardisation and scaling to unit interval
unit.z <- function(x) {
  0.15 * ((x - mean(x)) / sd(x)) + 0.5
}

# data.transformed[, 1] <- minmax(data.transformed[, 1]) # for example,using min-max normalisation for the first varible.

#* Handle the outliers of Clorides data
Winsorized <- function(x, h = 0) { # 1. pre-defining the inputs
  n <- length(x) # 2. store the length of x
  repl <- floor(h * n) # 3. how many data to replace
  x <- sort(x) # 4. sort x
  x[1:repl] <- x[repl + 1] # 5. replace lower values
  x[(n - repl + 1):n] <- x[n - repl] # 6. replace upper values
  return(x)
}

#* Function to make scatter plots for question 2
q2_scatterplots_ <- function(x) {
  plot(data_transformed[, x],
    data_transformed[, 5],
    xlab = data.variable.names[x + 1],
    ylab = "quality",
    main = paste("Scatterplot of", data.variable.names[x + 1], "vs quality")
  )
}

#* Function to make histograms for question 2
q2_histograms_ <- function(x) {
  if (x == 5) {
    xAsix <- "quality" # quality data
    mainTitle <- "histograms of quality"
    # } else if (x == 4) {
    #   xAsix <- data.variable.names[5] # Alcohol data
    #   mainTitle <- paste("histograms of", data.variable.names[5], "vs quality")
  } else {
    xAsix <- data.variable.names[x + 1]
    mainTitle <- paste("histograms of", data.variable.names[x + 1], "vs quality")
  }

  hist(data_transformed[, x],
    xlab = xAsix,
    main = paste(mainTitle),
    col = "red"
  )
}
################################
# ! END Functions for question 2
################################

################################
# ! First Transformation
################################
# ! Declare and summary the variables for transformation
I <- c(2, 3, 4, 5, 6) # Choose any four X variables and Y

variables_for_transform <- data.subset[, I] # obtain a 400 by 5 matrix

summary(data.subset) #* Summary of the original data
summary(variables_for_transform) #* Summary of the variables for transformation

#* Checking skewness of each variable
# ! must transform the skewness in ranghe (-0.5, 0.5)
skewness(data.subset[, 1]) #* citric-acid = 0.2880969
cor(data.subset[, 1], data.subset[, 6]) #* citric-acid vs quality = 0.2575766

skewness(variables_for_transform[, 1]) #* CL- = 5.549833
skewness(variables_for_transform[, 2]) #* SO2 = 1.107329
skewness(variables_for_transform[, 3]) #* pH = 0.2939697
skewness(variables_for_transform[, 4]) #* alcohol = 0.973979
skewness(variables_for_transform[, 5]) #* quality = 0.3256281


#* Checking correlation of each variable with quality
# ! must all (+) or all (-) correlation with quality
cor(variables_for_transform[, 1], variables_for_transform[, 5]) #* CL- vs quality = -0.1796647
cor(variables_for_transform[, 2], variables_for_transform[, 5]) #* SO2 vs quality = -0.2283422
cor(variables_for_transform[, 3], variables_for_transform[, 5]) #* pH vs quality = -0.0908259
cor(variables_for_transform[, 4], variables_for_transform[, 5]) #* alcohol vs quality = 0.5088754

# ! END Define and summary the variables for transformation

# for each variable, you need to figure out a good data transformation method,
# such as Polynomial, log and negation transformation. The k-S test and Skewness
# calculation may be helpful to select the transformation method

#* Copy "variables_for_transform" in case if needed
data_transformed <- variables_for_transform

################################
#* Because "CL-" it has some outliers, we need to handle the outliers
#* It's made the data too skew
#* We will use Winsorized transformation to handle the outliers of "CL-" data
################################

#* Handle the outliers of Clorides data
#* 0.002 is the percentage of data to replace 0.2% of data (old is 0.01 = 1%)
data_transformed[, 1] <- Winsorized(data_transformed[, 1], 0.002)

#* Double check the skewness & correlation of "CL-" data after Winsorized transformation
#* Nothing changed
skewness(data_transformed[, 1]) #* CL- = 5.549833 -> 5.563453
cor(data_transformed[, 1], data_transformed[, 5]) #* CL- vs quality = -0.1796647 -> 0.02539569

#* Double-check the graphs of the "CL-" data after Winsorized transformation
q2_scatterplots_(1)
q2_histograms_(1)

################################
# ! END First Transformation
################################

################################
# ! Second Transformation
################################
#* Define "^p" to transform the data to the range (-0.5, 0.5)
#* Data 3 and 5 are already in the range (-0.5, 0.5)
data_transformed[, 1] <- log(data_transformed[, 1]) #* CL-
data_transformed[, 2] <- variables_for_transform[, 2]^(0.1) #* SO2
data_transformed[, 4] <- variables_for_transform[, 4]^(-2) #* alcohol

# ! Now all data in range (-0.5, 0.5)
skewness(data_transformed[, 1]) #* CL- = 5.563453 -> 2.463744 (2.298781 is winsorized h=0.01)
skewness(data_transformed[, 2]) #* SO2 = 1.107329  -> 0.02647984
skewness(data_transformed[, 4]) #* alcohol = 0.973979 -> -0.3814045

#* Double-check the correlation with quality after Poly transformation
cor(data_transformed[, 1], data_transformed[, 5]) #* = 0.02539569 -> 0.009410949 (0.009663275 is winsorized h=0.01)
cor(data_transformed[, 2], data_transformed[, 5]) #* = -0.2199692
cor(data_transformed[, 3], data_transformed[, 5]) #* = -0.0908259
cor(data_transformed[, 4], data_transformed[, 5]) #* = -0.5000348

# ! We have Correlation of CL- is POSITIVE... Others are NEGATIVE
#* Negation transformation to let the correlation with quality all the same sign
#* Here is change the "CL-" to NEGATIVE value
data_transformed[, 1] <- min(data_transformed[, 1]) + max(data_transformed[, 1]) - data_transformed[, 1]


#* Double-check the correlation with quality after negation transformation
# ! After negation transformation, the correlation with quality all negative
cor(data_transformed[, 1], data_transformed[, 5]) #* = 0.02539569 -> -0.009410949
cor(data_transformed[, 2], data_transformed[, 5]) #* = -0.2199692
cor(data_transformed[, 3], data_transformed[, 5]) #* = -0.0908259
cor(data_transformed[, 4], data_transformed[, 5]) #* = -0.5000348

#* Double check the skewness of each variable after all correlation with quality are the same sign (NEGATIVE)
# ! Now all data in range (-0.5, 0.5)
skewness(data_transformed[, 1]) #* CL- = -5.563453 -> -2.463744
skewness(data_transformed[, 2]) #* SO2 = -1.107329  -> 0.02647984
skewness(data_transformed[, 3]) #* pH = 0.2939697
skewness(data_transformed[, 4]) #* alcohol = 0.973979 -> -0.3814045
skewness(data_transformed[, 5]) #* quality = 0.3256281

# ! Graphs of the data after transformation
for (i in 1:4) {
  q2_scatterplots_(i)
}

for (i in 1:5) {
  q2_histograms_(i)
}

################################
# ! END Second Transformation
################################


# summary(variables_for_transform) #* Summary of the variables for transformation
# summary(data_transformed) #* Summary of the transformed data

min_value <- numeric(ncol(data_transformed))
max_value <- numeric(ncol(data_transformed))

# Loop through each column
for (i in 1:ncol(data_transformed)) {
  min_value[i] <- min(data_transformed[, i], na.rm = TRUE) # na.rm = TRUE to handle any NA values
  max_value[i] <- max(data_transformed[, i], na.rm = TRUE)
}


################################
# ! Third Transformation
################################
#* The skewness of "CL-" data is still high
#* Because of the log transformation, all the values are negative
#* => Do the scaling first to get the positive values before applying the log/poly transformation to reduce the skewness
#* Now Scaling transformations to adjust the scale of each variable to the range (0, 1)
for (i in 1:5) {
  data_transformed[, i] <- minmax(data_transformed[, i])
}

for (i in 1:4) {
  q2_scatterplots_(i)
}

for (i in 1:5) {
  q2_histograms_(i)
}
################################
# ! END Third Transformation
################################

################################
# ! Forth Transformation
################################
data_transformed[, 1] <- data_transformed[, 1]^(2.5)
#* Double-check the skewness & correlation of "CL-" data after Poly transformation
skewness(data_transformed[, 1]) #* CL- = 5.549833 -> -2.463744 -> -0.1331598
cor(data_transformed[, 1], data_transformed[, 5]) #* = 0.02539569 -> -0.009410949 -> 0.006507077


data_transformed[, 1] <- min(data_transformed[, 1]) + max(data_transformed[, 1]) - data_transformed[, 1]
#* Double-check the skewness & correlation of "CL-" data after Negation transformation
skewness(data_transformed[, 1]) #* CL- = 5.549833 -> -2.463744 -> -0.1331598 -> 0.1331598
cor(data_transformed[, 1], data_transformed[, 5]) #* = 0.2427764 -> -0.009410949 -> 0.01152566 -> -0.006507077

#* Double-check the graphs of the "CL-" data after Forth transformation
q2_scatterplots_(1)
q2_histograms_(1)
################################
# ! END Forth Transformation
################################


#* All the shape of graph are bell shape and easier to read on the scatter plot
for (i in 1:4) {
  q2_scatterplots_(i)
}

for (i in 1:5) {
  q2_histograms_(i)
}

skewness(data_transformed[, 1]) #* CL- = 0.1331598
skewness(data_transformed[, 2]) #* SO2 = 0.02647984
skewness(data_transformed[, 3]) #* pH = 0.2939697
skewness(data_transformed[, 4]) #* alcohol = -0.3814045
skewness(data_transformed[, 5]) #* quality = 0.3256281
cor(data_transformed[, 1], data_transformed[, 5]) #* = -0.006507077
cor(data_transformed[, 2], data_transformed[, 5]) #* = -0.2199692
cor(data_transformed[, 3], data_transformed[, 5]) #* = -0.0908259
cor(data_transformed[, 4], data_transformed[, 5]) #* = -0.5000348

# Save this transformed data to a text file
write.table(data_transformed, "Truong-Phuc-LE-transformed.txt") # replace ??name?? with either your surname or first name.


##########################################
# ! Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data_transformed_copy <- as.matrix(read.table("Truong-Phuc-LE-transformed.txt")) # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM()
fit.QAM(data_transformed_copy, "Q3-1_WAM.txt", "Q3-1_Stats.txt")

# Get weights for Power Mean p=0.5 with fit.QAM()
fit.QAM(data_transformed_copy, "Q3-2_PM05.txt", "Q3-2_PM05_Stats.txt", PM05, invPM05) # p = 0.5


# Get weights for Power Mean p=2 with fit.QAM()
fit.QAM(data_transformed_copy, "Q3-3_QM.txt", "Q3-3_QM_Stats.txt", QM, invQM) # p = 2

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data_transformed_copy, "Q3-4_OWA.txt", "Q3-4_OWA_Stats.txt") # OWA

# ! The best is the Q3-3_QM.txt


#######################################
# Question 4 - Use Model for Prediction
#######################################

new_input <- c(0.9, 0.65, 38, 2.53, 7.1)
new_input_for_transform <- matrix(new_input[c(2, 3, 4, 5)], , nrow = 1, ncol = 5, byrow = TRUE) # choose the same four X variables as in Q2

# transforming the four variables in the same way as in question 2
matrix_input <- rbind(new_input_for_transform, data.subset[, I])
matrix_input[, 1] <- Winsorized(matrix_input[, 1], 0.002)

matrix_input[, 1] <- log(matrix_input[, 1]) #* CL- 
matrix_input[, 2] <- matrix_input[, 2]^(0.1) #* SO2
matrix_input[, 4] <- matrix_input[, 4]^(-2) #* alcohol

matrix_input[, 1] <- min(matrix_input[, 1]) + max(matrix_input[, 1]) - matrix_input[, 1]
new_input_for_transform

for (i in 1:4) {
  matrix_input <- minmax(matrix_input)
}

matrix_input[, 1] <- (matrix_input[, 1])^(2.5) #* CL-
matrix_input[, 1] <- min(matrix_input[, 1]) + max(matrix_input[, 1]) - matrix_input[, 1]

matrix_input[1, 1:4] #* 0.0000000 0.4178195 0.5146479 0.2919217


# applying the transformed variables to the best model selected from Q3 for Y prediction
w <- c(0.384981592186693, 0, 0.615018407813309, 0)
predict_result <- PM(matrix_input[1, 1:4], w, 2)
predict_result #* 0.4036029


# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
Reverse_predict_result <- round(predict_result * (max(variables_for_transform[, 5]) - min(variables_for_transform[, 5])) + min(variables_for_transform[, 5]))
Reverse_predict_result #* 5



#############################################################################################
# References
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# Meyer, D., Dimitriadou, E., Hornik, K., et al (2022) "e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071)", TU Wien. [Software]. Available at: https://cran.r-project.org/web/packages/e1071/index.html (Accessed: [Date of Access])# 
