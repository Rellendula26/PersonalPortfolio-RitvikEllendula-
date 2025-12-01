#Load in Data 
df=read.csv('~/Downloads/insurance.csv', header=TRUE)
num_cols <- unlist(lapply(df, is.numeric))
print(num_cols)
plot(df[,num_cols])

#Explanatory Data Analysis 
round(cor(df[,num_cols]),4)
names(df)
#Non-Numeric Columns 
boxplot(df$charges ~ df$sex, main='sex')
boxplot(df$charges ~ df$region, main='region')

#Model Creation 
model1=lm(df$charges ~. , data=df)
print(model1)
summary(model1)