# Pacotes

library(tidyverse)
library(future.apply)
library(furrr)
library(caret)

plan(multiprocess, workers = 4)



# Dados

data(iris)


# Definir seed para reproducibilidade

set.seed(123)


# Definir dados de treinamento (90%) e teste (10%) usando amostra 
# estratificada para manter a proporção de observações por classes

iris <- iris %>% 
  mutate(indice_linha = row_number())

treinamento <- iris %>% 
  group_by(Species) %>% 
  sample_frac(., size = 0.9)

teste <- iris %>% 
  filter(!indice_linha %in% treinamento$indice_linha)

treinamento <- treinamento %>% select(-indice_linha)
teste <- teste %>% select(-indice_linha)


# treinar random forest 

seeds <- vector(mode = "list", length = 31)
for(i in 1:30) seeds[[i]] <- sample.int(1000, 27)
seeds[[31]] <- sample.int(1000, 1)


fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            seeds = seeds)


grid_completo <- expand.grid(mtry = c(1, 2, 3),
                             ntree = c(300, 400, 500),
                             nodesize = c(5, 10, 15))


rf_cv <- function(mtry_val, ntree_val, nodesize_val) {

  mtry_temp <- data.frame(.mtry = mtry_val)
  
  set.seed(123)

  rf_fit <- train(Species ~ ., 
                  data = treinamento,
                  method = "rf",
                  #metric = "Accuracy",
                  trControl = fit_control,
                  tuneGrid = mtry_temp,
                  ntree = ntree_val,
                  nodesize = nodesize_val)

  resultados <- rf_fit$results %>% 
    mutate(ntree = ntree_val,
           nodesize = nodesize_val) %>% 
    select(mtry, ntree, nodesize, Accuracy)
  
  return(resultados)
  
}


# PURRR (serial)

purrr_tempo <- system.time(
  
  a <- pmap_df(grid_completo, rf_cv)
  
)


# FURRR (parallel)

furrr_tempo <- system.time(

  b <- future_pmap_dfr(grid_completo, rf_cv)
  
)


# APPLY (serial)

apply_tempo <- system.time({
  
  d <- mapply(
    rf_cv, 
    mtry_val = grid_completo$mtry, 
    ntree_val = grid_completo$ntree,
    nodesize_val = grid_completo$nodesize
  )
  
  mtry_val <- unlist(d)[c(seq(from = 1, to = 108, by = 4))]
  ntree_val <- unlist(d)[c(seq(from = 2, to = 108, by = 4))]
  nodesize_val <- unlist(d)[c(seq(from = 3, to = 108, by = 4))]
  accuracies <- unlist(d)[c(seq(from = 4, to = 108, by = 4))]
  
  d  <-  data.frame(mtry = mtry_val, ntree = ntree_val, 
                     nodesize = nodesize_val, Accuracy = accuracies)
})


# FUTURE APPLY (parallel)

future_apply_tempo <- system.time({
  
  e <- future_mapply(
    rf_cv, 
    mtry_val = grid_completo$mtry, 
    ntree_val = grid_completo$ntree,
    nodesize_val = grid_completo$nodesize
  )
  
  mtry_val <- unlist(e)[c(seq(from = 1, to = 108, by = 4))]
  ntree_val <- unlist(e)[c(seq(from = 2, to = 108, by = 4))]
  nodesize_val <- unlist(e)[c(seq(from = 3, to = 108, by = 4))]
  accuracies <- unlist(e)[c(seq(from = 4, to = 108, by = 4))]
  
  e  <-  data.frame(mtry = mtry_val, ntree = ntree_val, 
                    nodesize = nodesize_val, Accuracy = accuracies)
})






all_equal(a, b)
all_equal(b, d)
all_equal(d, e)

print(purrr_tempo)
print(furrr_tempo)
print(apply_tempo)
print(future_apply_tempo)
