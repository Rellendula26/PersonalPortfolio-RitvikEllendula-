library(gapminder)
library(dplyr)
library(ggplot2)
head(gapminder)
#Summarizing Life Expectancy

summary(gapminder$lifeExp)
str(gapminder$lifeExp)
meanlifeExp=mean(gapminder$lifeExp, na.rm=TRUE)
print(meanlifeExp)

#Relationship between Life Expectancy & GDP 
plot(gapminder$gdpPercap, gapminder$lifeExp, xlab="GDP/Capita", ylab="Life Expectancy", main="Life Expect Vs. GDP/Capita")
model <- lm (gapminder$lifeExp ~ gapminder$gdpPercap)
summary(model)
abline(model, col="red")

#Developing Lagged Variables 

data <- gapminder %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(Lagged_GDP = lag(gdpPercap, n = 2))

# View the first few rows of the dataset including the lagged variable
head(data)

ggplot(data = data %>% filter(country == "United States"), aes(x = year, y = lifeExp)) +
  geom_line() +
  ggtitle("Life Expectancy over Time in the United States")

model_interaction <- lm(lifeExp ~ gdpPercap * year + pop, data = data)
summary(model_interaction)
plot(model_interaction)

model_lagged <- lm(lifeExp ~ Lagged_GDP + pop + year, data = data)
summary(model_lagged)
plot(model_lagged)

model_nonlinear <- lm(lifeExp ~ poly(gdpPercap, 2) + pop, data = data)
summary(model_nonlinear)

library(plm)
model_fixed_effects <- plm(lifeExp ~ gdpPercap + pop, data = data, 
                           index = c("country", "year"), model = "within")
summary(model_fixed_effects)
plot(model_fixed_effects)


# Check for multicollinearity
library(car)
vif(model_interaction)

# Check for heteroscedasticity
library(lmtest)
bptest(model_interaction)

# Check for autocorrelation
dwtest(model_lagged)