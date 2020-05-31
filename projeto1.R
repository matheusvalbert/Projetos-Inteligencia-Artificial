#importacao bibliotecas que serao utilizadas para classificacao
library(rpart)
library(rpart.plot)
library(class)
library(e1071)
library(doMC)

leitura_do_dataset <- function(){ #funcao de leitura
  
  nomeArquivos <- list.files(path='.') #leitura de todos os arquivos da raiz
  
  indice <- sapply(strsplit(nomeArquivos, "_"), `[`,1) #gera o indice a partir do nome do arquivo e coloca em um vetor
  
  df<-data.frame(matrix(ncol = 4097, nrow = 2000)) #criacao do dataset com a declaracao de uma matriz
  
  for(i in 1:2000){ #loop para ler todos os arquivos
    
    arq <- read.csv(file = nomeArquivos[i], header = FALSE, sep = " ") #leitura do arquivo
    arq <- arq[-(1:3),] #retira as linhas 1 a 3
    arq <- arq[,-18] #retira a coluna 18
    v <- as.vector(t(arq)) #converte a matriz lida para um vetor
    v[4097] <- indice[i] #altera a posicao 4097 do vetor, para o valor do respectivo indice 0-9
    v <- as.numeric(v) #converte o vetor para numero
    v <- v[1:4097] #retira warning, caso o vetor tenha mais de 4097 posicoes
    df[i, ] <- v #copia o vetor para a respectiva linha do dataset 1-2000
  }
  
  return(df) #retorna o dataset
}

func_KNN <- function(df, valK) { #funcao KNN, recebe o dataset e o valor do K
  
  set.seed(123) #criacao da aleatoriedade para divisao do dataset entre treino e teste
  indiceTreino <- sample(1:nrow(df), 0.8*nrow(df)) #divide o dataset entre treino e teste 80% treino e 20% teste
  
  dadosTreino <- df[indiceTreino, ] #copia os dados de treino para a variavel
  dadosTeste <- df[-indiceTreino, ] #copia os dados de teste para a variavel
  
  especiesTreino <- dadosTreino[ ,4097] #copia a coluna 4097 para a varial
  dadosTreino <- dadosTreino[ ,-4097] #retira a coluna 4097 dos dados de treino
  especiesTeste <- dadosTeste[ ,4097] #copia a coluna 4097 para a varial
  dadosTeste <- dadosTeste[, -4097] #retira a coluna 4097 dos dados de teste
  
  modelo <- knn(train = dadosTreino, test = dadosTeste, cl = especiesTreino, k = valK) #chamada do algoritmo KNN
  
# table(modelo) #gera tabela NAO NECESSARIO PARA EXIBIR % de acerto
# table(especiesTeste) #gera tabela NAO NECESSARIO PARA EXIBIR % de acerto
# table(especiesTeste, modelo) #gera tabela NAO NECESSARIO PARA EXIBIR % de acerto
  
  probabilidade <- mean(modelo == especiesTeste) #calculo da probabilidade de acerto
  sprintf('KNN = %d Probabilidade = %.2f%%', valK, probabilidade*100) #print formatado da probabilidade de acerto
}

func_arvore <- function(df) { #funcao arvore
  
  set.seed(123) #criacao da aleatoriedade para divisao do dataset entre treino e teste
  indiceTreino <- sample(1:nrow(df), 0.8*nrow(df)) #divide o dataset entre treino e teste 80% treino e 20% teste
  
  dadosTreino <- df[indiceTreino, ] #copia dados treino para a variavel
  dadosTeste <- df[-indiceTreino, ] #copia dados teste para a variavel
  
  modelo <- rpart(X4097~., dadosTreino, method="class", control = rpart.control(minsplit = 1, cp = -1)) #chamada da funcao da arvore de decisao
  #parametro cp = -1 faz com que a arvore crie mais "folhas" de decisao, assim tornando mais preciso

# summary(modelo) #gera sumario NAO NECESSARIO PARA EXIBIR % de acerto
# plot <- rpart.plot(modelo, type = 3) #plot da arvore

  test <- dadosTeste[, -4097] #retira a coluna 4097 do treino
  pred <- predict(modelo, test, type="class") #realiza o teste para validacao da arvore
  
  matrizConf <- table(dadosTeste[,4097], pred) #gera a matriz de confusao
# diag(matrizConf) #mostra a matriz
  probabilidade <- sum(diag(matrizConf))/sum(matrizConf) #realiza conta vara verificar a probabilidade porcentagem de acerto
  sprintf('Arvore - Probabilidade %.2f%%', probabilidade*100) #print formatada probabilidade de acerto da arvore
}

func_vetor <- function(df) { #funcao SVM
  
  set.seed(123) #criacao da aleatoriedade para divisao do dataset entre treino e teste
  smp_size <- floor(0.8 *nrow(df)) #encontra o numero de 80% dos exemplares, no caso 1600
  train_ind <- sample(seq_len(nrow(df)), size = smp_size) #divide o dataset entre treino e teste 80% treino e 20% teste
  
  train <- df[train_ind, ] #copia para a matriz treino
  test <- df[-train_ind, ] #copia para a matriz teste
  
  testClass <- test[,4097] #copia indice para a classe test
  test<-test[,-4097] #retirada do indice da matriz test
  
  classifier = svm(formula = X4097~. , data = train, type = 'C-classification', kernel = 'polynomial', scale = FALSE, degree = 2, coef0 = 100, gamma = 10, tolerance = 0.75) #chama do algoritmo SVM
  #modificacoes: kernel = polynomial, scale: retirar warning, parametro degree: "graus" da linha para a separacao, coef0: usado para polynomial, gamma: influencia o quanto um treinamento influencia em seus resultados, tolerance: criterio de tolerancia para a classificacao
  
  pred = predict(classifier, newdata = test) #teste do treinamento realizado a partir do dataset
  
  matrizConf <- table(testClass, pred) #gera matriz de confusao
  probabilidade <- sum(diag(matrizConf))/sum(matrizConf) #calcula a probabilidade de acerto
  sprintf('Vetor de suporte - Probabilidade %.2f%%', probabilidade*100) #print formatado da probabilidade de acerto do algoritmo SVM
}

#chamada das funcoes

df <- leitura_do_dataset() #chamada para a criacao do dataset

#chamada do KNN
func_KNN(df, 1)
func_KNN(df, 3)
func_KNN(df, 7)
func_KNN(df, 9)

#chamada arvore de decisao
func_arvore(df)

#chamada SVM
func_vetor(df)
