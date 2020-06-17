#Observação todos os plots foram comentados no código para uma execução mais rápida

library(nnet)
library(neuralnet)
library(keras)
library(e1071)
library(rpart)
library(rpart.plot)
library(class)

leitura_dataset <- function() { #leitura dataset
  
  df1 <- read.table('datatraining.txt', header = TRUE, sep = ',', stringsAsFactors = FALSE)
  df2 <- read.table('datatest.txt', header = TRUE, sep = ',', stringsAsFactors = FALSE)
  df3 <- read.table('datatest2.txt', header = TRUE, sep = ',', stringsAsFactors = FALSE)
  df <- rbind(df1, df2, df3) 
  return(df)
}

gera_dados <- function() { #gera treino e teste
  
  set.seed(123)
  indiceTreino <- sample(1:nrow(df), 0.8*nrow(df)) 
  return(indiceTreino)
}

prints <- function(df) { #plot e boxplot
  
  summary(df)
  which(is.na(df) == TRUE)
  dim(df)
  boxplot.stats(df$Humidity)
  df <- df[-1]
  plot(df)
  class(df)
  boxplot(df$Temperature, df$Humidity, df$Light, df$CO2, df$HumidityRatio)
}

rede_neural <- function(df, indiceTreino) { #rede neural neuralnet
  
  dadosTreino <- df[indiceTreino, ]
  dadosTeste <- df[-indiceTreino, ]
  
  #chamada neuralnet
  neural = neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio, data=dadosTreino,
                   linear.output = FALSE, hidden = c(3,2))
  
  #probabilidade de acerto e preparacao dos dados
  teste <- dadosTeste[,7]
  dadosTeste <- dadosTeste[,-1]
  dadosTeste <- dadosTeste[,-6]
  pred <- compute(neural, dadosTeste)
  pred <- pred$net.result
  pred[pred>=0.7] <- 1
  pred[pred<0.7] <- 0
  pred <- factor(pred)
  teste <- factor(teste)
  matrizConf <- table(teste, pred)
  probabilidade <- sum(diag(matrizConf))/sum(matrizConf)
  
  #tabela dados
  #table(pred)
  #table(teste)
  
  #plots dados
  #plot(pred)
  #plot(teste)
  #plot(neural)
  
  sprintf('Rede Neural - Probabilidade de acerto: %.2f%%', probabilidade*100)
}

rede_neural_keras <- function(df, indiceTreino) { #rede neural keras
  
  #divisao dados
  dadosTreino <- df[indiceTreino, ]
  dadosTeste <- df[-indiceTreino, ] 
  
  #preparacao dados
  x_train <- dadosTreino[,-7]
  x_train <- x_train[,-1]
  x_train <- as.matrix(x_train)
  y_train <- dadosTreino[,7]
  y_train <- as.matrix(y_train)
  
  x_test <- dadosTeste[,-7]
  x_test <- x_test[,-1]
  x_test <- as.matrix(x_test)
  y_test <- dadosTeste[,7]
  y_test <- as.matrix(y_test)
  
  #preparacao modelo
  model = keras_model_sequential()
  model %>%
    layer_dense(units = 128, input_shape = c(5), activation = 'selu') %>%
    layer_dense(units = 64, activation = 'tanh') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  
  #compilacao do modelo
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )

  #ajuste do modelo
  history = model %>% fit(
    x_train,y_train,
    epochs = 45, batch_size = 32,
    validation_split = 0.2
  )
  
  #plot grafico treinamento
  #plot(history)
  
  #probabilidade de acerto
  acerto <- model %>% evaluate(x_test, y_test)
  
  sprintf('Rede Neural Keras - Probabilidade de acerto: %.2f%%', acerto[2]*100)
}

func_vetor <- function(df, indiceTreino) { #funcao SVM
  
  #divisao dataset
  train <- df[indiceTreino, ]
  test <- df[-indiceTreino, ]
  
  #preparacao dados
  train <- train[,-1]
  testClass <- test[,7]
  test <- test[,-7]
  test<-test[,-1]
  
  classifier = svm(formula = Occupancy~. , data = train,
                   type = 'C-classification', kernel = 'polynomial', scale = FALSE, 
                  degree = 2, coef0 = 100, gamma = 10, tolerance = 0.75) #chama do algoritmo SVM
  #modificacoes: kernel = polynomial, scale: retirar warning,
  #parametro degree: "graus" da linha para a separacao, coef0: usado para polynomial, 
  #gamma: influencia o quanto um treinamento influencia em seus resultados, 
  #tolerance: criterio de tolerancia para a classificacao
  
  pred = predict(classifier, newdata = test) #teste
  
  #probabilidade
  matrizConf <- table(testClass, pred)
  probabilidade <- sum(diag(matrizConf))/sum(matrizConf)
  sprintf('Vetor de suporte - Probabilidade %.2f%%', probabilidade*100)
}

func_arvore <- function(df, indiceTreino) {

  #divisao dataset
  train <- df[indiceTreino, ]
  test <- df[-indiceTreino, ]
  
  #preparacao dados
  train <- train[,-1]
  testClass <- test[,7]
  test <- test[,-7]
  test<-test[,-1]
  
  modelo <- rpart(Occupancy~., train, method = "class", control = rpart.control(minsplit = 1, cp = -1))
  #gera o modelo
  
  #modelo <- rpart(Occupancy~., train, method = "class", control = rpart.control(minsplit = 1))
  
  pred <- predict(modelo, test, type="class") #realiza o teste para validacao da arvore
  
  #plot <- rpart.plot(modelo, type = 3)

  matrizConf <- table(testClass, pred) #gera a matriz de confusao
  # diag(matrizConf) #mostra a matriz
  probabilidade <- sum(diag(matrizConf))/sum(matrizConf) 
  #realiza conta vara verificar a probabilidade porcentagem de acerto
  sprintf('Arvore - Probabilidade %.2f%%', probabilidade*100)
}

df <- leitura_dataset()
indiceTreino <- gera_dados()
#prints(df)
rede_neural(df, indiceTreino)
rede_neural_keras(df, indiceTreino)
func_vetor(df, indiceTreino)
func_arvore(df, indiceTreino)
