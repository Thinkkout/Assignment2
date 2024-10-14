library(readr)
library(dplyr)
library(gtsummary)
library(knitr)
library(ggplot2)
train <- read.csv("train.csv")

print(head(train))#show the top 10 lines of train data frame

table1 <- train %>%
  select(Survived, Sex) %>%
  tbl_summary(by = Survived)

print(table1)

table2 <- train %>%
  select(Survived, Sex, Age) %>%
  filter(Survived==1) %>%
  filter(!is.na(Age))

print(table2)

survived_data <- table2%>%
  select(Sex,Age)%>%
  tbl_summary(by = Sex)


sumdata <- data.frame(
  Min = min(table2$Age),
  Max = max(table2$Age),
  Mean = mean(table2$Age),
  Median = median(table2$Age),
  SD = sd(table2$Age),
  IQR = IQR(table2$Age)
)

print(survived_data)
kable(sumdata)

Quantile = quantile(table2$Age)
kable(Quantile, caption = "Quantile of their Age")


qplot(factor(table2$Sex),table2$Age, data = table2, geom = c("boxplot","jitter"), fill =Sex, main= "graph", xlab = "sex" ,ylab = "age")

ggplot (table2, aes(x = as.factor(Sex), y = Age)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 7, outlier.size = 2) +
  labs(x = "Sex", y = "Age", title = "Outliers of member in sex and age bloxplot")

