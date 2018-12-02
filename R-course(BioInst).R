##########Data analysis with R##############

#############First week#################

#Variables
my_number <- 42
my_logical_var <- TRUE


var_2 <- var_1 * 10


result <- number_1 + number_2 > number_3


the_best_vector <-  c(1:5000, 7000:10000)


my_numbers_2 <- my_numbers[c(2, 5, 7, 9, 12, 16, 20)]


my_sum <- sum(my_vector[my_vector > 10])


my_vector_2 <- my_vector[abs(my_vector-mean(my_vector)) < sd(my_vector)]


#Working with Data Frame
mtcars$even_gear <- 0
mtcars$even_gear[c(mtcars$gear %% 2 != 1)] <- 1


mpg_4 <- mtcars$mpg[c(mtcars$cyl == 4)]


mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)),]


#Syntax elements
mtcars$new_var <- ifelse(mtcars$carb >= 4 | mtcars$cyl > 6, 1, 0)


if (mean(my_vector) > 20) {
  result <- 'My mean is great'
} else {
  result <- 'My mean is not so great'
} 


good_months <- c()
for(i in 2:144) {
  if(AirPassengers[i] > AirPassengers[i-1]) {
    good_months <- c(good_months, AirPassengers[i])
  }
}


moving_average <- numeric(135)
for(i in 1:135) {
  moving_average[i] <- mean(AirPassengers[c(i:(i+9))])
}


#Descriptive statistics
result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])


descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)


result <- aggregate(Ozone ~ Month, subset(airquality, Month%in%c(7,8,9), select = c(Ozone, Month)), length)


describeBy(x = airquality, group = airquality$Month, mat = T)


c(sd(iris$Sepal.Length), sd(iris$Sepal.Width), sd(iris$Petal.Length), sd(iris$Petal.Width))


c(median(iris$Sepal.Length), median(iris$Sepal.Width), median(iris$Petal.Length), median(iris$Petal.Width))


my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
m <- mean(my_vector, na.rm = T)
fixed_vector <- replace(my_vector, is.na(my_vector), m)


boxplot(Ozone ~ Month, airquality)


#Descriptive statistics. Graphics
plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, color = hp))+geom_point()


ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species, size = Petal.Length)) + geom_point()


#Saving results
save(my_sd, file = "my_sd.RData")


##############Second week##################

#Nominative data analysis
red_men <- prop.table(HairEyeColor[ , ,'Male'], 2)['Red','Blue']


sum(HairEyeColor[, 'Green','Female'])


mydata <- as.data.frame(HairEyeColor[, ,'Female'])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq)) + 
  geom_bar(aes(fill = Eye), stat="identity", position = position_dodge()) + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


chisq.test(HairEyeColor['Brown', ,'Female'])


diamods_table <- table(diamonds$cut, diamonds$color)    
chi_result <- chisq.test(diamods_table)    
main_stat <- chi_result$statistic


diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0))    
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))    
main_stat <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic


fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value


#Comparison of two groups
vc <- ToothGrowth$len[ToothGrowth$supp == "VC" & ToothGrowth$dose == 2]
oj <- ToothGrowth$len[ToothGrowth$supp == "OJ" & ToothGrowth$dose == 0.5]
t_stat <- t.test(vc, oj)$statistic


df <- read.csv(url('https://stepic.org/media/attachments/lesson/11504/lekarstva.csv'))
t.test(df$Pressure_before, df$Pressure_after, paired = T)$statistic


df <- read.table("dataset_11504_15.txt")
bartlett.test(V1 ~ V2, data = df)
wilcox.test(V1 ~ V2, data = df)
t.test(V1 ~ V2, data = df, var.equal = TRUE)


df <- read.table("dataset_11504_16.txt")
t.test(df$V1, df$V2)


#Application of analysis of variance
fit <- aov(yield ~ N + P + N:P, data=npk)
summary(fit)


fit <- aov(yield ~ N + P + K, data=npk)
summary(fit)

fit <- aov(Sepal.Width ~ Species, data=iris)
summary(fit)
TukeyHSD(fit)


df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))
df$patient <- as.factor(df$patient)
fit <- aov(temperature ~ pill + Error(patient/pill), data = df)
summary(fit)


df <- read.csv(url('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv'))
df$patient <- as.factor(df$patient)
fit <- aov(temperature ~ doctor * pill + Error(patient/(doctor * pill)), data = df)
summary(fit)


library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, color = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj

tg <- ToothGrowth


mydata <- read.csv(url("https://stepic.org/media/attachments/lesson/11505/shops.csv"))

pd = position_dodge(0.1)
ggplot(mydata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()


#Creating your own functions
NA.position <- function(x){
  y <- which(is.na(x))
  return(y)
}
my_vector <- c(1, 2, 3, NA, NA)
NA.position(my_vector)


NA.counter <- function(x){
  y <- sum(is.na(x))
  return(y)
}


filtered.sum <- function(x){
  y <- sum(x[x > 0], na.rm = T)
  return(y)
}


outliers.rm <- function(x){
  q <- quantile(x, 0.25) + quantile(x, 0.75)
  y <- x[abs(x - q/2) <= 2*IQR(x)]
  return(y)
}


###############Third week##################

#Correlation and simple linear regression
library(psych)
corr.calc <- function(x){
  tst <- corr.test(x[1], x[2])
  tst$r[1, 1]
  tst$p[1, 1]
  return(c(tst$r[1, 1], tst$p[1, 1]))
}


filtered.cor <- function(x){
  x <- x[,sapply(x, is.numeric)]
  cors <- cor(x)
  diag(cors) <- 0
  return(cors[which.max(abs(cors))])
}


smart_cor <- function(x){
  library(psych)
  sht1p <- shapiro.test(test_data[,1])$p
  sht2p <- shapiro.test(test_data[,2])$p
  if(sht1p > 0.05 & sht2p > 0.05) {
    return(corr.test(test_data, method = "pearson")$r[1, 2])
  } else {
    return(corr.test(test_data, method = "spearman")$r[1, 2])
  }
}
smart_cor <- function(x){    
  if (shapiro.test(x[[1]])$p < 0.05 | shapiro.test(x[[2]])$p < 0.05) {    
    return(cor.test(x[[1]], x[[2]], method = 'spearman')$estimate)    
  } else {    
    return(cor.test(x[[1]], x[[2]], method = 'pearson')$estimate)}
}


df <- read.table("dataset_11508_12.txt")
fit  <- lm(V1 ~ V2, df)
summary(fit)


df <- subset(diamonds, cut == 'Ideal' & carat == 0.46)    
fit <- lm(price ~ depth, df)    
fit_coef <- fit$coefficients


regr.calc <- function(df){
  if(cor.test(df[[1]], df[[2]], method = 'pearson')$p.value < 0.05) {
    fit <- lm(df[,1] ~ df[,2], df)
    df$fit <- predict(fit, df)
    return(df)
  } else {
    return("There is no sense in prediction")
  }
}


library(ggplot2)
my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = factor(Species)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")


#Multiple linear regression
fill_na <- function(x){
  fit <- lm(y ~ x_1+x_2, data = x)
  x$y_full <- ifelse(is.na(x$y), predict(fit, x), x$y)
  return(x)
}


summary(lm(rating ~ complaints*critical, attitude))


mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
summary(lm(mpg ~ wt*am, mtcars))


library(ggplot2)
mtcars$am <- factor(mtcars$am)
my_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col = am)) + 
  geom_smooth(method = 'lm')


#Multiple linear regression. Models selection
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
ideal_model <- step(model_full, scope = list(lower = model_null, upper = model_full), direction = 'backward')


anova(model_full, ideal_model)


LifeCycleSavings
sr 

model <- lm(sr ~ ., data = LifeCycleSavings)
lm(rating ~ complaints*critical, attitude)


#Model Diagnostics
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041,
               0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 
               0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 
               0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(sqrt(my_vector))
ggplot(swiss, aes(sqrt(my_vector[1:47])))+
  geom_histogram()


beta.coef <- function(x){
  x <-scale(x)    
  return(lm(x[,1] ~ x[,2])$coefficients)
}


normality.test  <- function(x){    
  return(sapply(x, shapiro.test)['p.value',])
}


#Model Diagnostics. Continuation
mydata <- read.csv(url("https://stepic.org/media/attachments/lesson/12088/homosc.csv"))
x <- gvlma(DV ~ IV, data = mydata)
summary(x)


resid.norm <- function(fit){
  res <- shapiro.test(fit$residuals)
  color <- ifelse(res$p.value<0.05, 'red', 'green')
  df <- data.frame(fit$residuals)
  return(ggplot(df, aes(fit.residuals))+
           geom_histogram(bins=30, fill=color))
}


high.corr <- function(x){
  cors <- cor(x)
  diag(cors) <- 0
  return(rownames(which(abs(cors)==max(abs(cors)),arr.ind=T)))
}


#Logistic regression
log_coef <- glm(am ~ disp + vs + mpg, mtcars, family = "binomial")$coefficients


library("ggplot2")
obj <- ggplot(data = ToothGrowth, aes(supp, len,  fill=factor(dose)))+
  geom_boxplot()


library(ROCR)
mydata <- read.csv(url("https://stepic.org/media/attachments/lesson/11478/data.csv"))  
fit  <- glm(admit ~ rank + gpa, mydata, family = "binomial")
summary(fit) 
head(predict(object = fit, type = "response"))
mydata$prob <- predict(object = fit, type = "response")


mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)
mydata$notna <- ifelse(is.na(mydata$admit),0,1)
fit <- glm(admit ~ rank*gpa, data = mydata, family = "binomial")
prob <- predict(object = fit, newdata = mydata[is.na(mydata$admit),], type = "response")
result=ifelse(prob>=0.4, 1, 0)
sum(result)


#Exporting analysis results from R
library(xtable)
library(stargazer)

fit1 <- lm(mpg ~ cyl+disp, mtcars)
fit2 <- aov(mpg ~ am*vs, mtcars)

fit_table1 <- xtable(fit1)
fit_table2 <- xtable(fit2)
print(fit_table1, type="html", file="fit_table1.html")
print(fit_table2, type="html", file="fit_table2.html")

stargazer(fit1, type="html",
          dep.var.labels = "mpg",
          covariate.labels = c("cyl", "disp"), out="models1,html")