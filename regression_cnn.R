rm(list = ls())
library(keras)
library(tidyverse)

## Loading the dataset and filtering only damage of size 0.007 and 0
load(file = 'D:/datasets/bearing_fault_cwru/processed/CWRU.dat')
data <- data %>% filter(type == 'Inner' | size == 0) %>% unnest()
data <- 
  bind_rows(
    data %>% group_by(size, load) %>% slice(00001:50000) %>% ungroup() %>% mutate(source = 'train'), 
    data %>% group_by(size, load) %>% slice(50001:55000) %>% ungroup() %>% mutate(source = 'validation'),
    data %>% group_by(size, load) %>% slice(55001:60000) %>% ungroup() %>% mutate(source = 'test')
  )

## Calculate the mean and sd value of DE, FE and BA channels, only for training data
mean_train <- data %>% filter(source == 'train') %>% select(DE, FE, BA) %>% map_dbl(mean, na.rm = TRUE)
sd_train <- data %>% filter(source == 'train') %>% select(DE, FE, BA) %>% map_dbl(sd, na.rm = TRUE)
mean_y_train <- data %>% filter(source == 'train') %>% select(size) %>% map_dbl(mean, na.rm = TRUE)
sd_y_train <- data %>% filter(source == 'train') %>% select(size) %>% map_dbl(sd, na.rm = TRUE)

## Scaling data and transforming dataset into tensors
data <- data %>% 
  mutate(
    DE = scale(DE, mean_train['DE'], sd_train['DE']),
    FE = scale(FE, mean_train['FE'], sd_train['FE']),
    BA = scale(BA, mean_train['BA'], sd_train['BA']),
    size = scale(size, mean_y_train['size'], sd_y_train['size'])
  )
summary(data)
x <- as.matrix(select(data, DE)) %>% unname()
y <- data$size %>% as.numeric %>% array_reshape(c(length(.),1))

## generator function
generator <- function(x, y, min_index, max_index, window = 400, stride = 1, batch_size = 128) {
  
  samples <- array(0, c(batch_size, window, dim(x)[2]))
  targets <- array(0, c(batch_size, dim(y)[2]))
  function() {
    for(s in 1:batch_size){
      i <- sample(min_index:(max_index - (window - 1)*stride), 1)
      idx <- seq(i, length.out = window, by = stride)
      samples[s,,] <- x[idx,]
      targets[s,] <- apply(y[idx,,drop = FALSE], 2, median)
    }
    list(samples, targets)
  }
}
train_index <- range(which(data$source == 'train'))
val_index <- range(which(data$source == 'validation'))
test_index <- range(which(data$source == 'test'))
rm(data)
test_gen <- generator(x, y, test_index[1], test_index[2])
val_gen <- generator(x, y, val_index[1], val_index[2])
train_gen <- generator(x, y, train_index[1], train_index[2])


## setting the number of batches will be run per epoch
train_steps <- (diff(train_index)+1)/128/100
val_steps <- (diff(val_index)+1)/128/100
test_steps <- (diff(test_index)+1)/128/100

## model construction
model <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
                input_shape = list(NULL, 1)) %>%
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 1)

## compiling model
model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = "mae"
)

## defining stopping callbacks
callbacks_list <- list(
  callback_early_stopping(
    monitor = 'mse',
    patience = 2
  ),
  callback_model_checkpoint(
    filepath = 'D:/datasets/bearing_fault_cwru/models/reg_model.h5',
    monitor = 'val_loss',
    save_best_only = TRUE
  )
)

## training the model
history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = train_steps,
  epochs = 30,
  validation_data = val_gen,
  validation_steps = val_steps,
  callbacks = callbacks_list
)

## evaluating the model accuracy with test data
model %>% evaluate_generator(test_gen, steps = test_steps)

## generating confusion matrix
c(x_test, y_test) %<-% test_gen()
y_test <- y_test * sd_y_train + mean_y_train
y_fit <- model %>% predict(x_test) %>% (function(x) x * sd_y_train + mean_y_train)
plot(y_fit - y_test)
abline(h = 0, col = 'red')
