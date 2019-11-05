# Reijo Sund 5.11.2019 - RStudio exercise #2 for the IODS course

# Data source: Kimmo Vehkalahti: ASSIST 2014 - Vaihe 3 (osan 2 lopussa), N=183
# Metadata available at:
# - https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-meta.txt
# - https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS2-meta.txt


# Load data into R

lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# Dimensions of the data
# 183 obs. of  60 variables
dim(lrn14)

# Structure of the data
# Mostly likert scale (1-5) variables
# Includes also Age (positive integers) and gender (as a two level factor: F and M)
# Attitude = Global attitude toward statistics ~Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj
# Points=Exam points (0-33)
summary(lrn14)
str(lrn14)

# Let's check that Attitude is calculated as defined
library(dplyr)
lrn14 <- lrn14 %>% mutate(Asenne=Da+Db+Dc+Dd+De+Df+Dg+Dh+Di+Dj)
plot(lrn14$Attitude,lrn14$Asenne)

# Check the distribution of exam points
# Quite "normal" distribution, more zero values than expected
hist(lrn14$Points)


# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning, surface learning and strategic learning
     deep_columns <- lrn14 %>% select(one_of(deep_questions))
  surface_columns <- lrn14 %>% select(one_of(surface_questions))
strategic_columns <- lrn14 %>% select(one_of(strategic_questions))

# Create column 'attitude' by scaling the column "Attitude"
# Create columns 'deep', 'surf', and 'stra' by averaging
# Exclude observations where the exam point variable is zero
# Select variables gender, age, attitude, deep, stra, surf and points
learning2014 <- lrn14 %>% mutate(
  attitude=Attitude/10,
  deep=rowMeans(deep_columns,na.rm=TRUE),
  surf=rowMeans(surface_columns,na.rm=TRUE),
  stra=rowMeans(strategic_columns,na.rm=TRUE)
  ) %>%
  filter(Points!=0) %>%
  select(gender, age=Age, attitude, deep, stra, surf, points=Points)
  
# Change working directory to IODS-folder
setwd("~/IODS-project")

# Save created data to folder 'data' as an Excel worksheet
library(openxlsx)
write.xlsx(learning2014,file="~/IODS-project/data/learning2014.xlsx")


# Read the data back to R and check that structure and a few first observations look the same
library(readxl)

readtest <- read.xlsx("~/IODS-project/data/learning2014.xlsx")
readtest2 <- readxl::read_excel("~/IODS-project/data/learning2014.xlsx") %>%
  mutate_at(vars(gender), factor)

 
str(learning2014)
str(readtest)
str(readtest2)

head(learning2014)
head(readtest)

