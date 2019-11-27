library(MASS)
data("Boston")
# center and standardize variables
boston_scaled <- scale(Boston)

# summaries of the scaled variables
summary(boston_scaled)

# class of the boston_scaled object
class(boston_scaled)

# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)

# summary of the scaled crime rate
summary(boston_scaled$crim)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime2 <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
crimev <- as.vector(crime2)
# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)

# number of rows in the Boston dataset 
n <- nrow(boston_scaled)
km_cluster <- kmeans(boston_scaled, centers = 3)
#boston_scaled <- boston_scaled %>% mutate(cluster=km_cluster$cluster)
boston_scaled$cluster <- km_cluster$cluster

# choose randomly 80% of the rows
ind <- sample(n,  size = n * 0.8)

# create train set
train <- boston_scaled[ind,]
train_cluster <- boston_scaled[ind,]

# create test set 
test <- boston_scaled[-ind,]

# save the correct classes from test data
correct_classes <- test$crime

# remove the crime variable from test data
test <- dplyr::select(test, -crime)

# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)
dim(train)
dim(lda.fit$scaling)
str(train)



model_predictors <- dplyr::select(train, -crime)

# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)

# matrix multiplication
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)

# k-means clustering
train2 <- dplyr::select(train, -crime)
km <- kmeans(train2, centers = 3)

#install.packages("plotly")
library(plotly)
plot_ly(x = matrix_product$LD1, 
        y = matrix_product$LD2, 
        z = matrix_product$LD3, 
        type= 'scatter3d', 
        mode='markers',
        color = train$crime)

plot_ly(x = matrix_product$LD1, 
        y = matrix_product$LD2, 
        z = matrix_product$LD3, 
        type= 'scatter3d', 
        mode='markers',
        color = train_cluster$cluster)

plot_ly(x = matrix_product$LD1, 
        y = matrix_product$LD2, 
        z = matrix_product$LD3, 
        type= 'scatter3d', 
        mode='markers',
        color = km$cluster)