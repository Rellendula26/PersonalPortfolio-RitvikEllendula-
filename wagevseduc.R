library(wooldridge)
data('wage1')
print("In this project, income will be the dependent variable, since it's depending on education")
#summary stats 
head(wage1) #first 6 vals for each variable 
summary(wage1) #summary stats for each variable 
str(wage1) #variables and types 
plot(wage1$wage ~ wage1$educ, main="Years of Education On Hourly Wage", ylab="Hourly Wage", xlab="Years of Education")
print("Seems to be a somewhat positive relationship and slight correlation.")

plot(hist(wage1$wage))
print("Wage seems skewed left")
plot(hist(wage1$educ))
print("Education seems skewed right")
print("This means that oftentimes, there seem to be more people educated, but having a lower wage right off the bat.")

plot(wage1$educ, wage1$wage, main = "Education vs. Wage",
     xlab = "Years of Education", ylab = "Hourly Wage")
abline(lm(wage ~ educ, data = wage1), col = "blue")
#abline fitted a line of best fit to this linear model and drew one with col being the color 

correlation <- cor(wage1$educ, wage1$wage, use = "complete.obs")
print(correlation)

educationModel <- lm(wage1$wage ~ wage1$educ)
summary(educationModel)

expandedModel <- lm(wage1$wage ~ wage1$educ + wage1$exper+wage1$female+wage1$nonwhite+wage1$numdep)
summary(expandedModel)
