college = read.csv("C:\\Users\\Richy\\Downloads\\college_motivation.csv")
unique(college$residence)
unique(college$school_accreditation)
unique(college$interest)

# Removing outlier observations 93 and 405
college1 = college[-c(93,405),]

## 1st Analysis
#ttest of two interest level groups
college_interested <- college1[college1$interest=="Interested",]$average_grades
college_not_interested <- college1[college1$interest=="Not Interested",]$average_grades

# Creating Boxplot visualization for both groups
boxplot(college_interested, college_not_interested, ylab="Average High School Grade",
        main="Boxplot Distribution of Average High School Grade",
        xlab="College Level of Interest", 
        names = c("Interested", "Not Interested"), 
        ylim=c(75,100))
mean_interested <- mean(college_interested)
mean_not_interested <- mean(college_not_interested)
points(x = 1, y = mean_interested, col = "red", pch = 19)
points(x = 2, y = mean_not_interested, col = "red", pch = 19)

# Checking equality of variance for both groups and running ttest
var.test(college_interested, college_not_interested)
t.test(college_interested, college_not_interested, 
       alternative = "two.sided", var.equal = FALSE)

# Checking for normality in each individual group
shapiro.test(college_interested)
shapiro.test(college_not_interested)

# Students that are not interested in college have a higher average high school grade than students 
# who are interested in college.

## 2nd Analysis

college = read.csv("C:\\Users\\Richy\\Downloads\\college_motivation.csv")

# Creating Contingency table for both variables
college$parent_was_in_college = ifelse(college$parent_was_in_college =="True", "Yes", "No")
college$will_go_to_college = ifelse(college$will_go_to_college =="True", "Yes", "No")
cont_table = table(college$parent_was_in_college, college$will_go_to_college)
cont_table

# Chi-Square test results
chi_square_result = chisq.test(cont_table)
print(chi_square_result)
# Visualizing Contingency table
barplot(cont_table, legend.text= FALSE, xlab="Student College Decision", 
        col=c("tan2","gray"), 
        main="Bar Chart of Parents College Decision vs Students College Decision")
legend("topright", legend = rownames(cont_table), fill = c("tan2", "gray"), 
       title = "Parent College Decision")

::#Reject Null: There is no significant relationship between Students' College decision and
::#Parents' college decisions, meaning that there is no significant difference in the proportion
::#of students who said yes to college versus students who said no to college between parents
::##who went to college versus parents who didn't go to college.

# 3rd Analysis 
college = read.csv("C:\\Users\\Richy\\Downloads\\college_motivation.csv")

# Creating function to convert Indonesian currency to US dollars 
# and applying it to a new variable using mapply function
convert_IDR_to_USD = function(IDR){
  return(IDR = IDR / 14500)
}
college["Parent_Salary_USD"]= mapply(convert_IDR_to_USD, college$parent_salary)

# Assigning type_school as a binary variable so we can include it as an independent variable for multiple linear regression
college$type_school = ifelse(college$type_school == "Academic", 1, 0)

# Running a multiple linear regression model
mlm = lm(average_grades ~ Parent_Salary_USD + type_school, data=college)
print(summary(mlm)) 

# Creating scatterplot matrix of all variables included in MLR
pairs(college[,c(1,12,9)])
selected_vars = college[,c(1,12,9)]
corr = cor(selected_vars)
corr
# Visualizing line of best fit for binary and quantitative variable
plot(college$type_school, college$average_grades)
abline(mlm, "red")

# Creating Residual plot to check for homoscedasticity
residuals <- resid(mlm)
plot(fitted(mlm), residuals, 
     ylab="Residuals", xlab="Predicted Values of Average High School Grades", 
     main="Residual Plot for Regression Model")
abline(0,0)

# Calling car package to enable us to check for multicollinearity
library(car)
car::vif(mlm)


# For every one hundred dollar increase in the monthly salary of both parents, there is, on average,
# a 1.08 point increase in average high school grades for the students.

