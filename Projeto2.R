library(scatterplot3d)

leitura_dataset <- function() { #leitura do dataset

  bf <- read.csv(file = 'breast-cancer-wisconsin.data', header = FALSE, sep = ",")
  
  data <- bf[, -1] #retirada do id
  
  for(i in 1:699) { #retira as linhas onde possuem valores faltantes
    
    if(isTRUE(data[i, 6] == '?')) {
      
      data <- data[-i,]
    }
  }
  
  return(data); #retorna o dataset
}

print_3dplot <- function(data) { #print de todos os plots 3d

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

print_3dplot_trabalho <- function(data) {
  
  scatterplot3d(data$V4, data$V3, data$V2, pch = 16, color = data$V11)
}

histograma_func <- function(data) { #printa todos os histogramas

  for(i in 1:9) {
    hist(as.numeric(data[, i]))
    readline(prompt = "Precione enter para o proximo plot")
  }
}

boxPlot_func <- function(data) { #realiza o boxplot de todas as dimensoes
  
  data <- data[, -10]
  boxplot(data)
}

cluster_func <- function(data) { #funcao calculo do cluster
  
  classes <- data[,10]
  data <- data[,-10]
  clusters <- kmeans(data, 2) #funcao cluster
  
  table(clusters$cluster)
  table(classes)
  
  for(i in 1:683) { #altera o indice para 1 e 2, para ser possivel ser comparado com o cluster
    
    if(classes[i] == 4) {
      
      classes[i] <- 1
    }
    else {
      
      classes[i] <- 2
    }
  }
  
  probabilidade <- mean(clusters$cluster == classes) #realiza o calculo da probabilidade de acerto
  
  if(probabilidade < 0.05) #caso o 1 e 2 estejam invertidos, realiza a "desinversao"
    sprintf("Probabilidade de acerto: %.2f%%", (1-probabilidade)*100)
  else
    sprintf("Probabilidade de acerto: %.2f%%", probabilidade*100)
}

elbow_func <- function(data) { #metodo do cotovelo
  
  data <- data[,-10]
  
  set.seed(123)
  
  wss <- (nrow(data))*sum(apply(data,2,var)) #preparacao do data, sem o indice
  
  for (i in 2:10)
    wss[i] <- sum(kmeans(data, centers=i)$withinss) #realizacao de multiplos kmeans
  
  plot(1:10, wss, type="b") 
  #plot do grafico, para verificacao do local do cotovelo
}

data <- leitura_dataset()
print_3dplot(data)
print_3dplot_trabalho(data)
histograma_func(data)
boxPlot_func(data)
cluster_func(data)
elbow_func(data)
