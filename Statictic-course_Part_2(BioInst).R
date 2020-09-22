###########Second part###############

###########First week###############
smart_test(mtcars[1:20,c("am", "vs")])

smart_test <-  function(df){
  if(min(table(df)) < 5) {
    fit <- fisher.test(df[[1]], df[[2]])
    return(fit$p.value)
  } else {
    fit <- chisq.test(table(df))
    return(c(fit$statistic[[1]], 
             fit$parameter[[1]], 
             fit$p.value))
  }
}



test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)
most_significant(test_data)

most_significant  <- function(test_data){    
  chisq_tests <- sapply(test_data, function(col) chisq.test(table(col))$p.value)    
  min_p  <- which(chisq_tests == min(chisq_tests))    
  return(colnames(test_data)[min_p])
}



test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(test_data){
  importance_calc <- function(v1, v2, threshold=(floor(ncol(test_data)/2)+1)){    
    ifelse(sum(v1 > v2) >= threshold, 'Yes', 'No')}    
  test_data$important_cases <- factor(apply(test_data[1:ncol(test_data)], 1, 
    importance_calc, v2 = colMeans(test_data[, 1:ncol(test_data)])), 
    levels = c("No", "Yes"))
  return(test_data)
}

get_important_cases  <- function(d){    
  m <-  colMeans(d)    
  compare_to_means <- apply(d, 1, function(x) as.numeric(x > m))    
  is_important <- apply(compare_to_means, 2, sum) > ncol(d)/2    
  is_important <- factor(is_important, levels = c(FALSE, TRUE), labels = c('No', 'Yes'))    
  d$important_cases <- is_important    
  return(d)
}



v <- c(1, 2, 3, 3, 3, 4, 5)
v <- c(1, 1, 1, 2, 3, 3, 3)

stat_mode <- function(data){
  tabs<-rle(sort(as.integer(data)))
  tabs<-t(as.matrix(cbind(tabs$values,tabs$lengths)))
  nmode<-c(rbind(tabs[1,][tabs[2,]==max(tabs[2,])]))
  return(nmode)
}

stat_mode <- function(v){        
  mode_positions <- which(table(v) == max(table(v)))    
  as.numeric(names(table(v))[mode_positions])
}



test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")

max_resid <- function(test_data){
  table <- table(test_data$Drugs, test_data$Result)
  cors <- which(chisq.test(table)$stdres==max(chisq.test(table)$stdres), arr.ind=T)
  return(c(rownames(table)[cors[,1]], colnames(table)[cors[,2]]))
}

max_resid <- function(test_data){    
  d <- table(test_data)    
  chi <- chisq.test(d)    
  ind <- which(chi$stdres==max(chi$stdres), arr.ind = T)    
  return(c(row.names(d)[ind[1]],colnames(d)[ind[2]]))    
}



obj <- ggplot(diamonds, aes(color)) +
  geom_bar(aes(fill = cut), position = position_dodge())

obj <- ggplot(diamonds, aes(x=color, fill=cut)) +
  geom_bar(position='dodge')


##############Second week##################

get_coefficients <- function(df) {
  model <- glm(y ~ x, df, family = "binomial")
  return(exp(model$coefficients))
}

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data  <- transform(test_data, x = factor(x), y = factor(y))
get_coefficients(test_data)



centered <- function(df, var_names) {
  df[var_names] <- sapply(df[var_names], function(x) x - mean(x))  
  
  return(df)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")
test_data

var_names = c("X4", "X2", "X1")
centered(test_data, var_names)



get_features <- function(df) {
  fit <- glm(df$is_prohibited ~ ., family="binomial", df)
  result <- anova(fit, test = "Chisq")
  features = rownames(result[which(result[,5] < 0.05),])
  
  if (length(features) > 0) {
    return(features)
  } else {
    return('Prediction makes no sense')
  }
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
str(test_data)
get_features(test_data)

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")
str(test_data)
get_features(test_data)



most_suspicious <- function(train, test) {
  model <- glm(train$is_prohibited ~ ., family="binomial", data=train)
  result <- test[which.max(predict.glm(model, test)),][,5]
  levels(result)[as.integer(result)]
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
str(test_data)

data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")
str(data_for_predict)

most_suspicious(test_data, data_for_predict)



normality_test <- function(df) {
  unlist(sapply(df[sapply(df, is.numeric)], function(c) shapiro.test(c)$p.value))
}

normality_test(iris)

test <- read.csv("https://stepic.org/media/attachments/course/524/test.csv")
normality_test(test)



smart_anova <- function(test){  
  p_normal <- unlist(by(test[, 1], test[, 2], function(x) shapiro.test(x)$p.value))   
  sd_equal <- bartlett.test(x ~ y, test)$p.value  
  
  if (all(p_normal > 0.05) & sd_equal > 0.05){    
    fit <- aov(x ~ y, test)    
    result <- c(ANOVA = summary(fit)[[1]]$'Pr(>F)'[1])    
    
    return(result)  
  } else {    
    fit <- kruskal.test(x ~ y, test)    
    result <- c(KW = fit$p.value)    
    
    return(result)    
  }    
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")
str(test_data)
smart_anova(test_data)



install.packages("dplyr")
library(dplyr)
    
normality_by <- function(test_data){    
  result <- test_data %>% group_by(y, z) %>%     
    summarize(p_value = shapiro.test(x)$p.value)     
  return(result)    
}

get_p_value <- function(x){      
  shapiro.test(x)$p.value    
}    
normality_by <- function(test){    
  grouped <- test %>%    
    group_by_(.dots = colnames(.)[2:3]) %>%         
    summarise_each(funs(get_p_value))         
  names(grouped)[3] <- 'p_value'         
  return(grouped)         
}

normality_by(mtcars[, c("mpg", "am", "vs")])

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")
str(test_data)

normality_by(test_data)



obj <- ggplot(data=iris, aes(x=Sepal.Length, group=Species, fill=Species)) + geom_density(alpha=0.2)


###############Third week##################

smart_hclust <- function(df, n_clusters) {
  dist_matrix <- dist(df)
  model <- hclust(dist_matrix)
  clusters <- cutree(model, n_clusters)
  df['cluster'] = as.factor(clusters)
  
  df
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
str(test_data)
smart_hclust(test_data, 3)



get_difference <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)], function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  
  return(names(p_val)[p_val < 0.05])    
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
get_difference(test_data, 2)

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
get_difference(test_data, 2)



get_pc <- function(test){    
  fit <- prcomp(test)    
  test<- cbind(test, fit$x[,1:2])    
  
  return(test)    
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
test_data

get_pc(test_data)



get_pca2 <- function(df) {
  fit = prcomp(df)
  n_components = which(summary(fit)$importance['Cumulative Proportion',] > 0.9)[1]
  df = cbind(df, fit$x[, 1:n_components])
  df
}

fit <- prcomp(swiss)
summary(fit)

result  <- get_pca2(swiss)
str(result)



is_multicol <- function(df) {
  positions <- subset(as.data.frame(which(abs(cor(df)) > 0.9999, arr.ind=TRUE)), row < col)
  
  if (nrow(positions) > 0) {
    as.vector(sapply(positions, function(x) names(df)[x]))
  } else {
    "There is no collinearity in the data"
  }
}

test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
is_multicol(test_data)

#V1 = V2 + 1
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
is_multicol(test_data)

#V1 = V2 + 1 ?? V3 = V4 - 2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
is_multicol(test_data)



dist_matrix <- dist(swiss)    
fit <- hclust(dist_matrix)     
swiss$cluster <- as.factor(cutree(fit, 2))    
my_plot <- ggplot(swiss, aes(Education, Catholic, col = cluster)) + geom_point() + geom_smooth(method = 'lm')

