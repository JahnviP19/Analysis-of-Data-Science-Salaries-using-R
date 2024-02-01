# Data Exploration and Cleaning:
# Loaded the dataset
df <- read.csv("ds_salaries.csv")

# Explored dataset structure
str(df)
summary(df)

# Checked for missing values
sum(is.na(df))  # no missing value found

# Checked for extreme values
summary(df$salary_in_usd)

# Detected Outliers
boxplot(df$salary_in_usd, main = "Boxplot of Salary")

# Calculated quartiles and IQR
Q1 <- quantile(df$salary_in_usd, 0.25)
Q3 <- quantile(df$salary_in_usd, 0.75)
IQR <- Q3 - Q1

# Defined the threshold for outliers
threshold <- 1.5 * IQR

# Identified outliers
outliers <- df$salary_in_usd < (Q1 - threshold) | df$salary_in_usd > (Q3 + threshold)

# Print rows corresponding to outliers
print(df[outliers, ])

# Replaced outliers with median of salary_in_usd directly in the same column
df$salary_in_usd[outliers] <- median(df$salary_in_usd, na.rm = TRUE) #repeated all the outliers steps twice to get rid of all outliers

#Data Visualization:
# Check the structure of 'experience_level'    
str(df$experience_level)

# Check summary statistics of 'experience_level'
summary(df$experience_level)

# Convert 'experience_level' to a factor
df$experience_level <- as.factor(df$experience_level)

# Convert 'experience_level' to an ordered factor
df$experience_level <- factor(df$experience_level, levels = c("EN", "EX", "MI", "SE"), ordered = TRUE)

# Bar plot for 'experience_level'
barplot(table(df$experience_level), main = "Distribution of Experience Levels", xlab = "Experience Level", ylab = "Frequency")

# Plot the scatter plot
plot(df$experience_level, df$salary_in_usd, 
     main = "Salary vs. Experience Level", 
     xlab = "Experience Level",
     ylab = "Salary in USD")

# Histogram for 'salary_in_usd'
hist(df$salary_in_usd, main = "Histogram of Salary in USD", xlab = "Salary in USD")

#Descriptive Statistics:
# Descriptive statistics for salary
mean_salary <- mean(df$salary_in_usd)
median_salary <- median(df$salary_in_usd)
sd_salary <- sd(df$salary_in_usd)

# Print results
cat("Mean Salary:", mean_salary, "\n")
cat("Median Salary:", median_salary, "\n")
cat("Standard Deviation:", sd_salary, "\n")

#Hypothesis Testing:
# ANOVA for 'salary_in_usd' and 'employment_type'
anova_model <- aov(salary_in_usd ~ employment_type, data = df)
summary(anova_model)

# Pairwise t-test for 'salary_in_usd' and 'employment_type'
pairwise.t.test(df$salary_in_usd, df$employment_type, p.adjust.method = "bonferroni")


#Predictive Modeling:
#To calculate the Gain Ratio for each feature
library(rpart)
library(caret)
library(ggplot2)

# Create a decision tree model
tree_model <- rpart(salary_in_usd ~ experience_level + employment_type + job_title + remote_ratio + company_size,data = df)

# Calculate Gain Ratio using varImp
variable_importance <- varImp(tree_model, scale = FALSE)

# Print variable importance
print(variable_importance)

# Build a linear regression predictive model
model <- lm(salary_in_usd ~ experience_level + employment_type + job_title + company_size, data = df)

#Split out new training and test datasets:
train_indices <- createDataPartition(df$salary_in_usd, p = 0.8, list = FALSE)
train_data <- df[train_indices, ] 
test_data <- df[-train_indices, ]            #the model has been trained on the full dataset with all factor levels for job_title

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Add predictions to the test_data dataframe
test_data$predicted_salary <- predictions

# Evaluate the model performance
mse <- mean((test_data$salary_in_usd - predictions)^2)  
rmse <- sqrt(mse)                                       
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Save the updated dataframe with predictions to a CSV file
write.csv(test_data, "predictions.csv", row.names = FALSE)

#Visualization:

# Count the frequencies of remote_ratio
remote_counts <- table(df$remote_ratio)
# Create a pie chart
pie(remote_counts, labels = paste(names(remote_counts), ": ", remote_counts), main = "Remote Ratio Pie Chart")


# Count the frequencies of work_year
work_year_counts <- table(df$work_year)
# Create a bar chart
barplot(work_year_counts, col = rainbow(length(work_year_counts)), main = "Work Year Distribution", xlab = "Count", ylab = "Work Year", horiz = TRUE)

# Count the frequencies of employee_residence
residence_counts <- table(df$employee_residence)
# Sort the counts in descending order
sorted_counts <- sort(residence_counts, decreasing = TRUE)
# Take the top 5 counts and corresponding residences
top_residences <- names(sorted_counts)[1:5]
top_counts <- sorted_counts[1:5]
# Created a bar chart for the top 5 residences
barplot(top_counts, col = rainbow(length(top_counts)), main = "Top 5 Employee Residences", names.arg = top_residences, xlab = "Employee Residence", ylab = "Count", las = 2)


# Count the frequencies of company_location
location_counts <- table(df$company_location)
# Sort the counts in descending order
sorted_counts <- sort(location_counts, decreasing = TRUE)
# Take the top 10 counts and corresponding locations
top_locations <- names(sorted_counts)[1:10]
top_counts <- sorted_counts[1:10]
# Create a bar chart for the top 10 company locations
barplot(top_counts, col = rainbow(length(top_counts)), main = "Top 10 Company Locations", names.arg = top_locations, xlab = "Company Location", ylab = "Count", las = 2)


# Convert 'work_year' to a factor for better ordering
df$work_year <- as.factor(df$work_year)

# Create a line chart for job growth by year
ggplot(df, aes(x = work_year, group = 1)) +
  geom_line(stat = "count", aes(y = ..count..), color = "blue") +
  geom_point(stat = "count", aes(y = ..count..), color = "red", size = 2) +
  labs(title = "Job Growth Over Years",
       x = "Work Year",
       y = "Job Count") +
  theme_minimal()

# Create a line chart for job growth by experience level
ggplot(df, aes(x = work_year, group = experience_level, color = experience_level)) +
  geom_line(stat = "count", aes(y = ..count..)) +
  geom_point(stat = "count", aes(y = ..count..), size = 2) +
  labs(title = "Job Growth Over Years by Experience Level",
       x = "Work Year",
       y = "Job Count",
       color = "Experience Level") +
  theme_minimal()


library(dplyr)
# Calculate the percentage of remote jobs per year
remote_percentage <- df %>%
  group_by(work_year, remote_ratio) %>%
  summarise(job_count = n()) %>%
  mutate(percentage = job_count / sum(job_count) * 100)

# Create a bar chart for the percentage of remote vs in-person jobs per year
ggplot(remote_percentage, aes(x = work_year, y = percentage, fill = remote_ratio)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage of Remote vs In-Person Jobs Per Year",
       x = "Work Year",
       y = "Percentage",
       fill = "Remote Ratio") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
