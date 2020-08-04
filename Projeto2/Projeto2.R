library(scatterplot3d)

leitura_dataset <- function() {

  bf <- read.csv(file = 'breast-cancer-wisconsin.data', header = FALSE, sep = ",")
  
  data <- bf[, -1]
  
  for(i in 1:699) {
    
    if(isTRUE(data[i, 6] == '?')) {
      
      data <- data[-i,]
    }
  }
  
  return(data);
}

print_3dplot <- function(data) {

  for(i in 1:9) {
    
    for(j in 1:9) {
      
      for(k in 1:9) {
        
        if(i != j && i != k && j != k) {
         
          scatterplot3d(data[, i], data[, j], data[, k], pch = 16, color = data$V11)
          readline(prompt = "Precione enter para o proximo plot") 
        }
      } 
    } 
  }
}

histograma_func <- function(data) {
  
  data <- data[-10 ,]
  for(i in 1:9) {
    hist(as.numeric(data[, i]))
    readline(prompt = "Precione enter para o proximo plot")
  }
}

boxPlot_func <- function(data) {
  
  data <- data[, -10]
  boxplot(data)
}

cluster_func <- function(data) {
  
  classes <- data[,10]
  data <- data[,-10]
  clusters <- kmeans(data, 2)
  
  table(clusters$cluster)
  table(classes)
  
  for(i in 1:683) {
    
    if(classes[i] == 4) {
      
      classes[i] <- 1
    }
    else {
      
      classes[i] <- 2
    }
  }
  
  probabilidade <- mean(clusters$cluster == classes)
  
  if(probabilidade < 0.05)
    sprintf("Probabilidade de acerto: %.2f%%", (1-probabilidade)*100)
  else
    sprintf("Probabilidade de acerto: %.2f%%", probabilidade*100)
}

elbow_func <- function(data) {
  
  set.seed(123)
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  for (i in 2:15)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  
  plot(1:15, wss, type="b", xlab="Numero de clusters", ylab="soma quadrado dos grupos")
}

data <- leitura_dataset()
print_3dplot(data)
histograma_func(data)
boxPlot_func(data)
cluster_func(data)
elbow_func(data)
