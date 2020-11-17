library(caTools)

fullpath <- 'Cursos/Udemy/PythonR/dataset/machine-learning/50_Startups.csv'
dataset = read.csv(fullpath, encoding = 'UTF-8')
View(dataset)

dataset$State = factor(dataset$State,
                        levels = c('New York','California','Florida'),
                        labels = c(1,2,3))

regressor = lm(formula = Profit ~ .,
               data = dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)

y_pred = predict(regressor,newdata = dataset)
y_pred

regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)

y_pred = predict(regressor,newdata = dataset)
y_pred
