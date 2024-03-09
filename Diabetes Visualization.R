library(readr)

diabetes <- read.csv("/Users/yousefkhaled/WORK/Data Analysis/diabetes.csv")

# Create a scatterplot
plot(diabetes$Insulin, diabetes$Glucose,
     xlab = "Insulin Concentration",
     ylab = "Glucose",
     main = "Relation between Glucose and Insulin")
# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Filter the data to keep only Glucose values between 50 and 160
diabetes_filtered <- diabetes[diabetes$Glucose >=50 & diabetes$Glucose <= 160, ]

# Create a histogram
hist(diabetes_filtered$Glucose,
     breaks = 20,
     xlab = "Glucose Value",
     main = "Glucose value in blood")
# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Create a boxplot
boxplot(diabetes$BloodPressure,
        main = "Blood Pressure Measurements",
        ylab = "Blood Pressure")
diabetes <- read.csv("diabetes.csv")

# Aggregate the maximum Insulin dose by age
max_insulin_by_age <- aggregate(Insulin ~ Age, data = diabetes, max)

# Create a bar plot
barplot(max_insulin_by_age$Insulin,
        names.arg = max_insulin_by_age$Age,
        xlab = "Age",
        ylab = "Maximum Insulin Dose",
        main = "Maximum Insulin Dose by Age")
# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Construct a table for the Pregnancies attribute
pregnancies_table <- table(diabetes$Pregnancies)

# Print the table
pregnancies_table
# Create a bar plot
barplot(pregnancies_table,
        xlab = "Number of Pregnancies",
        ylab = "Frequency",
        main = "Distribution of Pregnancies")
# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Calculate the mean, median, min, max of the Insulin dose
#used round function to approximate the number of the mean 
insulin_stats <- c(mean = round (mean(diabetes$Insulin), digits =3),
                   median = median(diabetes$Insulin),
                   min = min(diabetes$Insulin),
                   max = max(diabetes$Insulin))

# Create a pie chart
pie(insulin_stats,
    labels = paste(names(insulin_stats), insulin_stats, sep = ": "),
    main = "Statistics of Insulin Dose")
# Load the dataset
diabetes <- read.csv("diabetes.csv")
#create 2 denisty plot for diabetes by degree function and age  
# Create a density plot for DiabetesPedigreeFunction
plot(density(diabetes$DiabetesPedigreeFunction),
     xlab = "Diabetes Pedigree Function",
     main = "Diabetes Pedigree Function Distribution")

# Create a density plot for Age
plot(density(diabetes$Age),
     xlab = "Age",
     main = "Age Distribution")
# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Create a dot plot
dotchart(diabetes$DiabetesPedigreeFunction,
         groups = diabetes$Outcome,
         xlab = "Diabetes Pedigree Function per Diabetes outcome",
         main = "Dot Plot of Diabetes Pedigree Function by Outcome")
# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Select the five attributes
diabetes_subset <- diabetes[, c("Insulin", "Age", "Glucose", "BloodPressure", "DiabetesPedigreeFunction")]

# Create a scatterplot matrix
pairs(diabetes_subset,
      col = c("green", "purple")[diabetes$Outcome + 1],
      main = "Pairwise Relationships for Five Attributes")

# Add a legend
legend("topleft",
       legend = c("Outcome 0", "Outcome 1"),
       col = c("green", "purple"),
       pch = 1)

# Load the dataset
diabetes <- read.csv("diabetes.csv")

# Set the layout of the plots
par(mfrow = c(3, 3))

# Plot 1: Scatterplot between Insulin and Glucose
# This plot shows the relationship between Insulin and Glucose
# It seems that there is no clear linear correlation between the two variables
# There are many points with zero Insulin value, which could indicate missing values
plot(diabetes$Insulin, diabetes$Glucose,
     xlab = "Insulin Concentration",
     ylab = "Glucose",
     main = "Relation between Glucose and Insulin")

# Plot 2: Histogram of Glucose value in blood
# This plot shows the frequency of occurrence of each glucose value in blood
# It seems that the distribution is skewed to the right, with a long tail
# The most common glucose value is around 100
diabetes_filtered <- diabetes[diabetes$Glucose >= 50 & diabetes$Glucose <= 150, ]
hist(diabetes_filtered$Glucose,
     breaks = 20,
     xlab = "Glucose Value",
     main = "Glucose value in blood")

# Plot 3: Boxplot of BloodPressure
# This plot shows the outliers in the BloodPressure vector
# It seems that there are some values that are zero, which are clearly outliers and probably missing values
# There are also some values above 100, which are also outliers and could indicate high blood pressure
boxplot(diabetes$BloodPressure,
        main = "Blood Pressure Measurements",
        ylab = "Blood Pressure")

# Plot 4: Bar plot of the maximum Insulin dose by age
# This plot shows the maximum Insulin dose grouped per age of patient
# It seems that there is no clear pattern or trend in the maximum Insulin dose by age
# The highest maximum Insulin dose is observed for patients aged 60 and 66
max_insulin_by_age <- aggregate(Insulin ~ Age, data = diabetes, max)
barplot(max_insulin_by_age$Insulin,
        names.arg = max_insulin_by_age$Age,
        xlab = "Age",
        ylab = "Maximum Insulin Dose",
        main = "Maximum Insulin Dose by Age")

# Plot 5: Bar plot of the frequency of Pregnancies
# This plot shows the frequency of each number of pregnancies
# It seems that the distribution is skewed to the right, with a long tail
# The most common number of pregnancies is zero or one
pregnancies_table <- table(diabetes$Pregnancies)
barplot(pregnancies_table,
        xlab = "Number of Pregnancies",
        ylab = "Frequency",
        main = "Distribution of Pregnancies")

# Plot 6: Pie chart of the mean, median, min, max of the Insulin dose
# This plot shows the mean, median, min, max of the Insulin dose using one pie chart with labels
# It seems that the mean and max values are much larger than the median and min values
# This could indicate that the distribution of Insulin dose is skewed to the right and has some extreme values
insulin_stats <- c(mean = mean(diabetes$Insulin),
                   median = median(diabetes$Insulin),
                   min = min(diabetes$Insulin),
                   max = max(diabetes$Insulin))
pie(insulin_stats,
    labels = paste(names(insulin_stats), insulin_stats, sep = ": "),
    main = "Statistics of Insulin Dose")

# Plot 7: Density plot for DiabetesPedigreeFunction and Age
# These plots show the distribution of DiabetesPedigreeFunction and Age using density plots
# It seems that both distributions are skewed to the right, with long tails
# The DiabetesPedigreeFunction has a higher peak and a lower spread than Age
plot(density(diabetes$DiabetesPedigreeFunction),
     xlab = "Diabetes Pedigree Function",
     main = "Diabetes Pedigree Function Distribution")
plot(density(diabetes$Age),
     xlab = "Age",
     main = "Age Distribution")

# Plot 8: Dot plot of DiabetesPedigreeFunction by Outcome
# This plot shows the DiabetesPedigreeFunction per outcome attribute using a dot plot
# It seems that there is some overlap between outcome 0 and 1 in terms of DiabetesPedigreeFunction values
# However, there are more points with higher DiabetesPedigreeFunction values for outcome 1 than for outcome 0
dotchart(diabetes$DiabetesPedigreeFunction,
         groups = diabetes$Outcome,
         xlab = "Diabetes Pedigree Function per Diabetes outcome",
         main = "Dot Plot of Diabetes Pedigree Function by Outcome")

