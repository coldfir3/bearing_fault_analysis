rm(list = ls())
library(keras)
library(tidyverse)

## Data cleaning and scaling
load(file = 'D:/datasets/bearing_fault_cwru/processed/CWRU.dat')
data <- bind_rows(
  data %>% unnest() %>% group_by(type, load, size) %>% slice(00001:20000) %>% ungroup() %>% mutate(source = 'train'), 
  data %>% unnest() %>% group_by(type, load, size) %>% slice(20001:25000) %>% ungroup() %>% mutate(source = 'validation'),
  data %>% unnest() %>% group_by(type, load, size) %>% slice(25001:30000) %>% ungroup() %>% mutate(source = 'test')
) %>% filter(size == 0.007)
mean_train <- data %>% filter(source == 'train') %>% select(DE, FE, BA) %>% map_dbl(mean)
sd_train <- data %>% filter(source == 'train') %>% select(DE, FE, BA) %>% map_dbl(sd)

train_index <- range(which(data$source == 'train'))
val_index <- range(which(data$source == 'validation'))
test_index <- range(which(data$source == 'test'))

data <- data %>% 
  mutate(
    size = as.factor(size),
    DE = scale(DE, mean_train['DE'], sd_train['DE']),
    FE = scale(FE, mean_train['FE'], sd_train['FE']),
    BA = scale(BA, mean_train['BA'], sd_train['BA'])
    )
summary(data)
x <- as.matrix(select(data, DE, FE, BA))[,1, drop=FALSE ] %>% unname()
y <- to_categorical(data$type) %>% unname()
classes <- levels(data$type)
rm(data)

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

test_gen <- generator(x, y, test_index[1], test_index[2])
train_gen <- generator(x, y, train_index[1], train_index[2])
val_gen <- generator(x, y, val_index[1], val_index[2])

train_steps <- (diff(train_index)+1)/128/400
val_steps <- (diff(val_index)+1)/128/400
test_steps <- (diff(test_index)+1)/128/400


## model construction
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(400, 1)) %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = ncol(y), activation = 'softmax')

model <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu",
                input_shape = list(NULL, 1)) %>%
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = ncol(y), activation = 'softmax')

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = train_steps,
  epochs = 30,
  validation_data = val_gen,
  validation_steps = val_steps
)

model %>% evaluate_generator(test_gen, steps = test_steps)

c(x_test, y_test) %<-% test_gen()
y_fit <- model %>% keras::predict_classes(x_test)

table(apply(y_test, 1, function(y) which(as.logical(y))), y_fit)
