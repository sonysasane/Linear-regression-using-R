
df <- read.csv("Salary_Data.csv")
head(df)

install.packages("caTools")

#splitting the data

split <- sample.split(df$Salary,SplitRatio = 2/3)
split
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

#applying linear model
regressor <- lm(formula = Salary ~ YearsExperience,data = training_set)

#predicting y of test set
y_pred <- predict(regressor, newdata = test_set)
y_pred

#visualizing training set results
install.packages("ggplot2")

ggplot(data = training_set) +
  geom_point(aes(x = YearsExperience, y = Salary), color = "Red") +
  geom_line(aes(x = YearsExperience, y = predict(regressor, newdata = training_set)), color = "blue") +
  
  xlab("Years of Experience")+ ylab("Salary") +
  ggtitle("Salary by Experience") + theme(
    axis.title.x = element_text( size=15),
    axis.title.y = element_text( size = 15),
    plot.title = element_text(color = "Dark Blue", size =20)
  )


# visualizing test set results
ggplot(data = test_set) +
  geom_point(aes(x = YearsExperience, y= Salary),color = "red") +
  geom_line(aes(x = YearsExperience, y = y_pred), color = "blue") + 
  xlab("Years of Experience") + ylab("Salary") +
  ggtitle("Salary by Experience") + theme(
    axis.title.x = element_text(size = 12, color = "dark blue"),
    axis.title.y = element_text(size = 12,color = "dark blue"),
    plot.title = element_text(size = 15)
  )






