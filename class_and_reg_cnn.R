rm(list = ls())
library(keras)
library(tidyverse)

## Loading the dataset and filtering only damage of size 0.007 and 0
load(file = 'D:/datasets/bearing_fault_cwru/processed/CWRU.dat')
data <- data %>% unnest()
data <- 
  bind_rows(
  data %>% group_by(type, load, size) %>% slice(00001:50000) %>% ungroup() %>% mutate(source = 'train'), 
  data %>% group_by(type, load, size) %>% slice(50001:55000) %>% ungroup() %>% mutate(source = 'validation'),
  data %>% group_by(type, load, size) %>% slice(55001:60000) %>% ungroup() %>% mutate(source = 'test')
) %>% select(-BA, - RPM)

## Calculate the mean and sd value of DE, FE and BA channels, only for training data
mean_train <- data %>% filter(source == 'train') %>% select(DE, FE) %>% map_dbl(mean, na.rm = TRUE)
sd_train <- data %>% filter(source == 'train') %>% select(DE, FE) %>% map_dbl(sd, na.rm = TRUE)
mean_y_train <- data %>% filter(source == 'train') %>% select(size) %>% map_dbl(mean, na.rm = TRUE)
sd_y_train <- data %>% filter(source == 'train') %>% select(size) %>% map_dbl(sd, na.rm = TRUE)

## Scaling data and transforming dataset into tensors
data <- data %>% 
  mutate(
    DE = scale(DE, mean_train['DE'], sd_train['DE']),
    FE = scale(FE, mean_train['FE'], sd_train['FE']),
    size = scale(size, mean_y_train['size'], sd_y_train['size'])
    )
summary(data)
x <- as.matrix(select(data, DE, FE)) %>% unname()
y_class <- data$type %>% as.numeric %>% unname() %>% `-`(1) %>% to_categorical()
y_size <- data$size %>% as.numeric %>% array_reshape(c(length(.),1))
classes <- levels(data$type)

## generator function
generator <- function(x, y1, y2, min_index, max_index, window = 400, stride = 1, batch_size = 128) {

  samples <- array(0, c(batch_size, window, dim(x)[2]))
  targets1 <- array(0, c(batch_size, dim(y1)[2]))
  targets2 <- array(0, c(batch_size, dim(y2)[2]))
  function() {
    for(s in 1:batch_size){
      i <- sample(min_index:(max_index - (window - 1)*stride), 1)
      idx <- seq(i, length.out = window, by = stride)
      samples[s,,] <- x[idx,]
      targets1[s,] <- apply(y1[idx,,drop = FALSE], 2, median)
      targets2[s,] <- apply(y2[idx,,drop = FALSE], 2, median)
    }
  list(samples, list(targets1, targets2))
  }
}
train_index <- range(which(data$source == 'train'))
val_index <- range(which(data$source == 'validation'))
test_index <- range(which(data$source == 'test'))
rm(data)
train_gen <- generator(x, y_class, y_size, train_index[1], train_index[2])
val_gen <- generator(x, y_class, y_size, val_index[1], val_index[2])
test_gen <- generator(x, y_class, y_size, test_index[1], test_index[2], batch_size = 1024)

## setting the number of batches will be run per epoch
train_steps <- (diff(train_index)+1)/128/100
val_steps <- (diff(val_index)+1)/128/100
test_steps <- (diff(test_index)+1)/1024

input_model <- layer_input(shape = list(NULL, 2))

## model construction
conv_model <- input_model %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_global_max_pooling_1d()

class_predictor <- conv_model %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = ncol(y_class), activation = 'softmax')

size_predictor <-  conv_model %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 1)

model <- keras_model(input_model, list(class_predictor, size_predictor))

## compiling model
model %>% compile(
  optimizer = "rmsprop",
  loss = list("categorical_crossentropy", "mse"),
  metrics = list("accuracy", "mae")
)

## defining stopping callbacks
callbacks_list <- list(
  callback_early_stopping(
    monitor = 'acc',
    patience = 2
  ),
  callback_model_checkpoint(
    filepath = 'D:/datasets/bearing_fault_cwru/models/class_and_reg_model.h5',
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
model %>% evaluate_generator(test_gen, steps = 10)

## generating confusion matrix
c(x_test, y_test) %<-% test_gen()
y_fit <- model %>% predict(x_test)
table(apply(y_test, 1, function(y) which(as.logical(y))), y_fit)
