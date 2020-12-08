ggplot() + 
geom_point(aes(x = data_train$YearsExperience, y = data_train$Salary),
           colour = 'red') + 
geom_line(aes(x = data_train$YearsExperience, y = predict(regressor,newdata = data_train)),
           colour = 'blue') + 
ggtitle("Salary vs Experience (trainig model)") + 
xlab("Years of experience") + 
ylab("Salary")

ggplot() + 
  geom_point(aes(x = data_test$YearsExperience, y = data_test$Salary),
             colour = 'red') + 
  geom_line(aes(x = data_train$YearsExperience, y = predict(regressor,newdata = data_train)),
            colour = 'blue') + 
  ggtitle("Salary vs Experience (trainig model)") + 
  xlab("Years of experience") + 
  ylab("Salary")