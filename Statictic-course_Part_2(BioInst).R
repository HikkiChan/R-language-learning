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
