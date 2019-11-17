# Reijo Sund 18.11.2019 - RStudio exercise #3 for the IODS course

# Data source: UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/dataset)
# Metadata available at: https://archive.ics.uci.edu/ml/datasets/Student+Performance
#   The data are from two identical questionaires related to secondary school student alcohol
#   comsumption in Portugal.
# P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance.
# http://www3.dsi.uminho.pt/pcortez/student.pdf

source <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
dest <- "~/IODS-project/data/student.zip"

# Load Data from the web and unzip it
setwd("~/IODS-project")
download.file(source,dest)
unzip(dest,exdir="data/student")

# read the datasets into memory
por <- read.table("~/IODS-project/data/student/student-por.csv", sep = ";", header=TRUE)
math <- read.table("~/IODS-project/data/student/student-mat.csv", sep = ";", header=TRUE)

# Define own id for both datasets
library(dplyr)
por_id <- por %>% mutate(id=1000+row_number()) 
math_id <- math %>% mutate(id=2000+row_number())

# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)

pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))
            
pormath <- por_id %>% 
  bind_rows(math_id) %>%            # Combine datasets to one long data
  group_by(.dots=join_cols) %>%     # Aggregate data (more joining variables than in the example)
  summarise(                        # Calculating required variables from two obs
    n=n(),
    p.id=min(id),
    m.id=max(id), 
    p.failures=first(failures),
    p.paid=first(paid),
    p.absences=first(absences),
    p.G1=first(G1),
    p.G2=first(G2),
    p.G3=first(G3),
    m.failures=last(failures),
    m.paid=last(paid), 
    m.absences=last(absences),
    m.G1=last(G1),
    m.G2=last(G2),
    m.G3=last(G3),
    ) %>%
  filter(n==2, m.id-p.id>650)  # Remove lines that do not have exactly one obs from both datasets








# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

mp1 <- por_id %>% bind_rows(math_id) %>%
  arrange(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet,guardian,traveltime,studytime,failures,schoolsup,famsup,paid,activities,higher,internet,romantic,famrel,freetime,goout,Dalc,Walc,health,id)

#mp2 <- mp1 %>% group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet) %>% summarise(n=n(),min=min(id),max=max(id))
mp2 <- mp1 %>% 
  group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet,guardian,traveltime,romantic,famrel,freetime,goout) %>%
  summarise(n=n(),por.id=min(id),math.id=max(id)) %>%
  filter(n==2, math.id-por.id>650)
#mp2 <- mp1 %>% group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet,romantic,famrel,freetime,goout) %>% summarise(n=n(),min=min(id),max=max(id))



mp3_1 <- mp2 %>% filter(n<2)
mp3_2 <- mp2 %>% filter(n==2)
mp3_3 <- mp2 %>% filter(n>2) %>% ungroup() %>% mutate(n=row_number())
#mp3_3 <- mp2 %>% filter(n>2) %>% tibble::rowid_to_column("rn")

mp3_2inv <- mp3_2 %>% mutate(ero=max-min) %>% filter(ero<650)

mp3_3_org <- mp1 %>% right_join(mp3_3,by=join_by) %>% arrange(n,id)
mp3_3_org <- mp1 %>% right_join(mp3_2inv,by=join_by) %>% arrange(n,id)


# common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# %>% arrange(join_by)







koe <- por %>% group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet) %>% count()
koe2 <- math %>% group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet) %>% count()
koe3 <- d3 %>% group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet) %>% count()
koe4 <- math_id %>% group_by(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet) %>% count()


#d3=merge(math,por,by=join_by)
           



por_id <- por %>% mutate(id=1000+row_number()) 

por_join <- por_id %>% select(one_of(c("id",join_by)))

# join the two datasets by the selected identifiers
math_id <- math %>% inner_join(por_join, by = join_by)
por_math <- por_id %>% semi_join(math_id,by="id")
por_math_id <- por_math %>% semi_join(math_id,by="id")

  
missing <- por_id %>% anti_join(math_id,by="id")

  
# see the new column names
colnames(math_por)

# glimpse at the data
glimpse(math_por)

#3
# dplyr, math_por, join_by are available

# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# columns that were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

# alc is available

# access the 'tidyverse' packages dplyr and ggplot2
library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(alc, aes(high_use))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex") + geom_bar()

#4
# alc is available

# access the tidyverse libraries tidyr, dplyr, ggplot2
library(tidyr); library(dplyr); library(ggplot2)

# glimpse at the alc data
glimpse(alc) 

# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

# draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

#5
# alc is available

# access the tidyverse libraries dplyr and ggplot2
library(dplyr); library(ggplot2)

# produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

#6
library(ggplot2)

# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")

#7
# alc is available 

# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)

#8
# alc and dlyr are available 

# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)

#9
# alc, dplyr are available

# fit the model
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% tail(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

#10
# alc is available

# access dplyr and ggplot2
library(dplyr); library(ggplot2)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>% prop.table %>% addmargins

#11
# the logistic regression model m and dataset alc with predictions are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

#12
# the logistic regression model m and dataset alc (with predictions) are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]






