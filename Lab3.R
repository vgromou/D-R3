library(ggplot2)
library(dplyr)
library(reshape2)
options(warn=-1)

# Variant 1

## Finds dependence Vn on x for a given time from data.frame "data"; returns ggplot (doesn't print it)
solveT = function(data, time = 0, Vn = "V4"){
  
  if(!checkNames(data, c(Vn))){
    print("Wrong column name")
    return()
  }
  
  nData = data %>% select(t, x, Vn) %>%
    filter(t == time)
  
  ttitle = paste(Vn,",   Time =", time)
  gplot = ggplot(nData, aes(x = x, y = nData[[Vn]])) + geom_point(color = "orange") + 
    theme_bw() + labs(y = "y", title = ttitle)

  return(gplot)
}
## Returns all dependences Vn on x from data.frame "data"; returns ggplot (doesn't print it)
solveAllT = function(data, time = 0, Vs = c()){
  
  if(length(Vs) == 0){
    print("There is no data to display")
    return()
  }
  if(!checkNames(data, Vs)){
    print("There is no data for one (or more) of colnames given")
    return()
  }
  
  nData = data %>% filter(t == time) %>%
    select("t", "n", "x", Vs)
  nData = divide(nData, Vs)
  nData <- melt(nData, id.vars = c("t", "n", "x"))
    
  gplot <- ggplot(nData, aes(x, value, color = variable)) + labs(y = "y", color = "V", title = paste("Time =", time)) + 
    geom_line(size = 2) +
    theme_bw() + ylim(-1.1, 1.1)
  
  return(gplot)
}


#Variant 2

## Finds dependence Vn on t for a given time from data.frame "data"; returns ggplot (doesn't print it)
solveX = function(data, X = 0, Vn = "V4"){
  
  if(!checkNames(data, c(Vn))){
    print("Wrong column name")
    return()
  }
  
  nData = data %>% select(t, x, Vn) %>%
    filter(x == X)
  
  ttitle = paste(Vn,",   X =", X)
  gplot = ggplot(nData, aes(x = t, y = nData[[Vn]])) + geom_point(color = "orange") + 
    theme_bw() + labs(y = "y", title = ttitle)
  
  return(gplot)
}
## Returns all dependences Vn on t from data.frame "data"; returns ggplot (doesn't print it)
solveAllX = function(data, X = 0, Vs = c()){
  
  if(length(Vs) == 0){
    print("There is no data to display")
    return()
  }
  if(!checkNames(data, Vs)){
    print("There is no data for one (or more) of colnames given")
    return()
  }
  
  nData = data %>% filter(x == X) %>%
    select("t", "n", "x", Vs)
  nData = divide(nData, Vs)
  nData <- melt(nData, id.vars = c("t", "n", "x"))
  
  gplot <- ggplot(nData, aes(t, value, color = variable)) + labs(y = "y", color = "V", title = paste("X=", X)) + 
    geom_line(size = 2) +
    theme_bw() + ylim(-1.1, 1.1)
  
  return(gplot)
}


#Auxiliary Necessary in both variants

## Checks if is there a column with specified name
checkNames = function(data, names){
  Vs = names(data)
  for(i in 1: length(names)){
    if(!names[i] %in% Vs) return(FALSE)
  }
  return(TRUE)
}
## Finds maximum by absolute value in each column given
findMax = function(data, names){
  res = c()
  for(i in 1: length(names)){
    if(abs(max(data[[names[i]]])) > abs(min(data[[names[i]]])))
      res = c(res, abs(max(data[[names[i]]])))
    else
      res = c(res, abs(min(data[[names[i]]])))
  }
  return(res)
}
## Additional. Normalizes all of given columns (makes |values| < 1)
divide = function(data, names){
  maxes = findMax(data, names)
  for(i in 1: length(names)){
    if(maxes[i] > 1) 
      data[[names[i]]] = data[[names[i]]] / maxes[i]
  }
  return(data)
}
## Reads data from source (.csv file); needs path to file (string type)
readData = function(source){
  data <- read.csv(source)
  return(data)
}
## Renames columns
renameV = function(data){
  colnames(data)[1] <- "t"
  colnames(data)[2] <- "n"
  colnames(data)[3] <- "x"
  return(data)
}