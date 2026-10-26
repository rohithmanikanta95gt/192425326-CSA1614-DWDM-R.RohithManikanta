# Experiment 1: Addition of Two Numbers
num1 <- as.integer(readline(prompt = "Enter the first number: "))
num2 <- as.integer(readline(prompt = "Enter the second number: "))
result <- num1 + num2
print(result)


# Experiment 2: Subtraction of Two Numbers
num1 <- as.integer(readline(prompt = "Enter the first number: "))
num2 <- as.integer(readline(prompt = "Enter the second number: "))
result <- num1 - num2
print(result)


# Experiment 3: Multiplication of Two Numbers
num1 <- as.integer(readline(prompt = "Enter the first number: "))
num2 <- as.integer(readline(prompt = "Enter the second number: "))
result <- num1 * num2
print(result)


# Experiment 4: Division of Two Numbers
num1 <- as.numeric(readline(prompt = "Enter the first number: "))
num2 <- as.numeric(readline(prompt = "Enter the second number: "))

if (num2 != 0) {
  result <- num1 / num2
  print(result)
} else {
  print("Cannot divide by zero")
}


# Experiment 5: Checking Whether a Number Is Odd or Even
num <- as.integer(readline(prompt = "Enter a number: "))

if (num %% 2 == 0) {
  print("Number is even")
} else {
  print("Number is odd")
}


# Experiment 6: Calculating Mean, Median, and Mode
student_data <- data.frame(
  name = c("Arun", "Priya", "Kiran", "Meena", "Ravi"),
  age = c(21, 22, 22, 24, 25),
  marks = c(78, 85, 91, 85, 88)
)

find_mode <- function(x) {
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

print(mean(student_data$age))
print(median(student_data$age))
print(find_mode(student_data$age))

write.csv(student_data, "student_data.csv", row.names = FALSE)


# Experiment 7: Generating a Statistical Summary of Data
students <- data.frame(
  name = c("Anu", "Bala", "Charan", "Divya", "Rohan"),
  age = c(19, 20, 21, 22, 23),
  marks = c(76, 84, 90, 68, 95)
)

result <- summary(students)
print(result)

write.csv(students, "student_summary.csv", row.names = FALSE)


# Experiment 8: Finding the Greatest Among Three Numbers
first <- as.numeric(readline("Enter the first value: "))
second <- as.numeric(readline("Enter the second value: "))
third <- as.numeric(readline("Enter the third value: "))

values <- c(first, second, third)
greatest <- max(values)

cat("The greatest number is:", greatest, "\n")


# Experiment 9: Calculating the Interquartile Range (IQR)
student_data <- data.frame(
  name = c("Arun", "Priya", "Kiran", "Meena", "Ravi"),
  age = c(20, 21, 22, 23, 24),
  marks = c(55, 67, 78, 89, 95)
)

result <- IQR(student_data$marks)
print(result)

write.csv(student_data, "iqr_data.csv", row.names = FALSE)


# Experiment 10: Calculating Quantiles of Data
student_data <- data.frame(
  name = c("Asha", "Bala", "Charan", "Divya", "Rohan"),
  age = c(19, 20, 21, 22, 23),
  marks = c(65, 72, 81, 88, 96)
)

result <- quantile(student_data$marks, probs = c(0, 0.25, 0.5, 0.75, 1))
print(result)

write.csv(student_data, "quantile_data.csv", row.names = FALSE)


# Experiment 11: Calculating the Mid-Range of Data
scores <- c(45, 60, 72, 85, 95)

minimum <- min(scores)
maximum <- max(scores)
mid_range <- (minimum + maximum) / 2

print(minimum)
print(maximum)
print(mid_range)


# Experiment 12: Performing Z-Score Normalization
marks <- c(55, 65, 70, 80, 90, 95)

average <- mean(marks)
std_dev <- sd(marks)

z_scores <- (marks - average) / std_dev

print(z_scores)


# Experiment 13: Minimum, Maximum, Mean, and Min-Max Normalization
values <- c(10, 20, 30, 40, 50, 60)

average <- mean(values)
minimum <- min(values)
maximum <- max(values)

normalized <- (values - minimum) / (maximum - minimum)

print(average)
print(minimum)
print(maximum)
print(normalized)


# Experiment 14: Drawing Vertical and Horizontal Bar Plots
marks <- c(72, 85, 68, 90, 78)
students <- c("Arun", "Priya", "Kiran", "Meena", "Ravi")

barplot(marks, names.arg = students,
        main = "Student Marks",
        xlab = "Students",
        ylab = "Marks")

barplot(marks, names.arg = students,
        main = "Horizontal Bar Plot",
        xlab = "Marks",
        horiz = TRUE)


# Experiment 15: Visualizing Data Using a Box Plot
student_data <- data.frame(
  name = c("Arun", "Priya", "Kiran", "Meena", "Ravi", "Asha"),
  age = c(19, 20, 21, 22, 23, 24),
  marks = c(56, 72, 80, 85, 91, 96)
)

hist(student_data$marks, main = "Distribution of Marks",
     xlab = "Marks")

boxplot(student_data$marks, main = "Box Plot of Student Marks",
        ylab = "Marks", col = "lightblue")


# Experiment 16: Creating a Histogram in R
marks <- c(52, 65, 72, 78, 84, 88, 91, 95, 67, 73)

hist(marks,
     main = "Distribution of Student Marks",
     xlab = "Marks",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")


# Experiment 17: Performing Chi-Square Analysis of Data
set.seed(10)

age_group <- sample(c("Young", "Middle", "Older"), 90, replace = TRUE)
insulin_level <- sample(c("Low", "Normal", "High"), 90, replace = TRUE)

data <- data.frame(age_group, insulin_level)

result <- table(data$age_group, data$insulin_level)
print(result)

test <- chisq.test(result)
print(test)


# Experiment 18: Creating a Scatter Plot in R
set.seed(15)

hours <- rnorm(100, mean = 5, sd = 2)
marks <- 40 + 8 * hours + rnorm(100, mean = 0, sd = 10)

plot(hours, marks,
     main = "Study Hours vs Marks",
     xlab = "Study Hours",
     ylab = "Marks",
     pch = 19,
     col = "blue")

smoothScatter(hours, marks,
              main = "Smooth Scatter Plot")


# Experiment 19: Implementing Simple Linear Regression
health_data <- data.frame(
  Age = c(22, 25, 28, 30, 35, 38, 42, 45, 50, 55),
  BloodPressure = c(70, 72, 75, 78, 76, 80, 84, 85, 88, 92)
)

model <- lm(BloodPressure ~ Age, data = health_data)

print(summary(model))

plot(health_data$Age, health_data$BloodPressure,
     main = "Linear Regression Analysis",
     xlab = "Age",
     ylab = "Blood Pressure",
     pch = 19,
     col = "blue")

abline(model, col = "red", lwd = 2)


# Experiment 20: Implementing Multiple Linear Regression
health_data <- data.frame(
  Age = c(22, 25, 28, 30, 35, 38, 42, 45, 50, 55),
  BloodPressure = c(70, 72, 75, 78, 76, 80, 84, 85, 88, 92),
  Glucose = c(90, 95, 100, 105, 110, 115, 120, 125, 130, 140)
)

model <- lm(Age ~ BloodPressure + Glucose, data = health_data)

print(model)
print(coef(model))
print(summary(model))
