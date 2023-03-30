library(tidyverse)
library(caret)
library(xgboost)

# Importing the dataset
dataset <- read_excel("D:/machine learning/supermarket_sales.xlsx")
head(dataset)

# Checking for null values
sum(is.na(dataset))

# Checking for duplicates
sum(duplicated(dataset))

# Checking for outliers
boxplot(dataset)

# Removing outliers
Q1 <- quantile(dataset, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
dataset <- dataset[!apply(dataset, 1, function(x) any(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))), ]

# Checking for outliers
boxplot(dataset)

# Checking for correlation
corr <- round(cor(dataset), 2)
corr
heatmap(corr, annot = TRUE)

# check for skewness of each numeric feature
skewness <- dataset %>% select_if(is.numeric) %>% sapply(fivenum) %>% as.data.frame() %>% 
  rownames_to_column() %>% pivot_longer(-1, names_to = "statistic", values_to = "value") %>% 
  pivot_wider(names_from = "statistic", values_from = "value") %>% mutate(IQR = V3 - V2) %>% 
  mutate(outliers = (V1 < (V2 - 1.5 * IQR)) | (V5 > (V3 + 1.5 * IQR))) %>% select(-c(V1, V2, V3, V4, V5))

# Checking for skewness
print("Skewness of each numeric feature:")
skewness

# Checking for distribution
par(mfrow=c(4, 4))
for(i in 1:ncol(dataset)){
  hist(dataset[,i], main = colnames(dataset)[i], xlab = "Features", ylab = "Frequency")
}

# Select only numerical columns
numerical_cols <- names(which(sapply(dataset, is.numeric)))
# Scale the numerical columns
scaler <- preProcess(dataset[numerical_cols], method = c("range"))
dataset_scaled <- predict(scaler, dataset[numerical_cols])
# Create a new dataframe with the scaled values
dataset_scaled <- as.data.frame(dataset_scaled)
colnames(dataset_scaled) <- numerical_cols
# Concatenate the scaled numerical columns with the non-numerical columns
dataset <- cbind(dataset_scaled, dataset[!names(dataset) %in% numerical_cols])

# Select only numerical columns
numerical_cols <- names(which(sapply(dataset, is.numeric)))
# Scale the numerical columns
scaler <- preProcess(dataset[numerical_cols], method = c("range"))
dataset[numerical_cols] <- predict(scaler, dataset[numerical_cols])
# Create pairplot of all numerical features
pairs(dataset[numerical_cols])

# Create bar chart of total sales by date
total_sales_by_date <- dataset %>% group_by(Date) %>% summarise(Total = sum(Total, na.rm = TRUE))
ggplot(total_sales_by_date, aes(Date, Total)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create scatter plot of gross income vs. total sales
ggplot(dataset, aes(gross.income, Total)) + geom_point()

