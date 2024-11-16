# 1. Load Libraries and Data

library(dplyr) # data wrangling
library(ggplot2) # data visualization
library(lubridate) # handling date formatting
library(gridExtra) # merging ggplots
library(factoextra) # silhouette method, visualizing clusters

data <- read.csv("claims_q12023.csv", sep = ";")

# 2. Data Preparation

data[1:6, 1:6]

dim(data)

str(data)

## 2.1 Set Index

rownames(data) <- data$policy_id
data <- data %>% select(-policy_id)
data[1:6, 1:6]

## 2.2 Column Data Type

data <- data %>% mutate(
  zip_code = as.factor(zip_code),
  incident_hour = as.factor(incident_hour),
  production_year = as.factor(production_year),
  ) %>% mutate_if(
    is.character, as.factor
  ) %>% mutate(
    coverage_start_date = dmy(coverage_start_date),
    claim_incurred_date = dmy(claim_incurred_date)
  )

str(data)

# 3. Exploratory Data Analysis

## 3.1 Descriptive Statistics

summary(data)

## 3.2 Data Visualization

### Numeric Variables

plots_numeric <- c()

for (col in colnames(data %>% select(is.numeric))) {
  
  plot <- ggplot(data) +
    geom_density(aes(data[[col]]), fill = "lightblue") +
    labs(x = col) +
    theme_minimal()
  
  plots_numeric[[col]] <- ggplotGrob(plot)
}

grid.arrange(grobs = plots_numeric, ncol = 4)

### Categorical Variables

categorical_cols <- colnames(data %>% select(is.factor))

n_half <- round(length(categorical_cols)/2)

plot_category1 <- categorical_cols[1:n_half]
plot_category2 <- categorical_cols[(n_half+1):length(categorical_cols)]

plots_categorical1 <- c()

for (col in plot_category1) {
  
  top_cat <- data[[col]] %>% table() %>% sort(decreasing = TRUE) %>% head(5)
  
  plot <- ggplot(data.frame(top_cat)) +
    geom_col(aes(., Freq), fill = "darkgreen") +
    labs(y = col) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  plots_categorical1[[col]] <- ggplotGrob(plot)
}

grid.arrange(grobs = plots_categorical1, ncol = 3)

plots_categorical2 <- c()

for (col in plot_category2) {
  
  top_cat <- data[[col]] %>% table() %>% sort(decreasing = TRUE) %>% head(5)
  
  plot <- ggplot(data.frame(top_cat)) +
    geom_col(aes(., Freq), fill = "darkgreen") +
    labs(y = col) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  plots_categorical2[[col]] <- ggplotGrob(plot)
}

grid.arrange(grobs = plots_categorical2, ncol = 3)

## 3.3 Missing Values

sum(is.na(data))

# 4. Feature Engineering

## 4.1 Coverage to Claim Duration

data <- data %>% 
  mutate(
    coverage_to_claim_duration = 
      as.numeric(
        claim_incurred_date - coverage_start_date)
  )

head(data$coverage_to_claim_duration)

summary(data$coverage_to_claim_duration)

ggplot(data) + 
  geom_density(aes(coverage_to_claim_duration), fill = "lightblue") +
  theme_minimal() + 
  labs(x = "Coverage to Claim Duration")

## 4.2 Incident Time

data$incident_time <- cut(
  as.numeric(data$incident_hour),
            breaks = c(0, 6, 12, 18, 24),
            labels = c("Night", "Morning", "Afternoon", "Evening"))

head(data$incident_time)

summary(data$incident_time)

plot_data <- data.frame(sort(table(data$incident_time), decreasing = TRUE))

ggplot(plot_data) + 
  geom_col(aes(Var1, Freq), fill = "darkgreen") + 
  theme_minimal() + 
  labs(x = "Time", y = "Frequency")

## 4.3 Collision

data <- data %>% 
  mutate(isCollision = 
           as.factor(
             ifelse((grepl("collision", claim_type)) &
                  (grepl("collision", acc_type)),
                  "Yes", "No"))
  )

head(data$isCollision)

summary(data$isCollision)

plot_data <- data.frame(sort(table(data$isCollision), decreasing = TRUE))

ggplot(plot_data) + 
  geom_col(aes(Var1, Freq), fill = "darkgreen") + 
  theme_minimal() + 
  labs(x = "Collision", y = "Frequency")

## 4.4 Final Features

data <- data %>% 
  select(-c(zip_code, incident_city, car_brand, car_model, coverage_start_date, claim_incurred_date, incident_hour, claim_type, acc_type, injury_claim, property_claim, vehicle_claim))

# 5. Customer Segmentation

data[1:6, 1:6]

dim(data)

## 5.1 Factorization

data_transformed <- data %>% 
  mutate(across(where(is.factor), as.numeric))

data_transformed[1:6, 1:6]

## 5.2 Optimal Number of Cluster

fviz_nbclust(data_transformed, kmeans, "silhouette")

## 5.3 K-Means Clustering

set.seed(0)
kmeans_model <- kmeans(data_transformed, 2)

clusters_def <- data.frame(kmeans_model$centers)
clusters_def

## 5.4 Cluster Identification

data.frame(
  Subject = c("Collision", "Claim Amount", "Vehicle Involvements", "Emergency Service Notified"),
  "Cluster 1" = c("No", "Low", "Single-Vehicle", "Yes"),
  "Cluster 2" = c("Yes", "High", "Multi-Vehicle", "No")
)

## 5.5 Cluster Visualization

### Principal Component Analysis

fviz_cluster(kmeans_model, data_transformed,
             palette = c("blue", "red"), 
             geom = "point",
             ggtheme = theme_minimal()
             )

### Variable Selection

ggplot(data_transformed) + 
  geom_point(aes(x = total_claim_amount,
                 y = annual_prem,
                 color =
                   as.factor(kmeans_model$cluster)
                 )
             ) + 
  scale_color_manual(values = c("1" = "blue", "2" = "red")) +
  theme_minimal() + 
  labs(x = "Total Claim Amount", y = "Annual Premium", color = "Cluster")