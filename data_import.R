rm(list = ls())
library(tidyverse)

datadir <- 'D:/datasets/bearing_fault_cwru/12k Drive End Bearing Fault Data'
folders <- dir(datadir, full.names = TRUE)

.read <- function(file, nam = c('DE', 'FE', 'BA', 'RPM'))
  R.matlab::readMat(file) %>% 
  as.data.frame() %>% 
  as.tibble %>% 
  `names<-`(nam) %>% 
  mutate(file = basename(file))
.read2 <- function(file, nam = c('DE', 'FE', 'BA', 'RPM')){
  kk <- R.matlab::readMat(file)
  kk1 <- as.data.frame(kk[2:3]) %>% as.tibble %>% unname() %>% `names<-`(nam)
  kk2 <- as.data.frame(kk[4:5]) %>% as.tibble %>% unname() %>% `names<-`(nam)
  kk <- rbind(kk1,kk2) %>% mutate(file = basename(file))
  kk
}

data <- NULL
data <- 
  dir(folders[1], full.names = TRUE) %>%
  map(.read) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Ball', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12'))) %>%
  bind_rows(data)
data <- 
  dir(folders[2], full.names = TRUE) %>%
  map(.read) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Inner', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12'))) %>%
  bind_rows(data)
data <- 
  dir(folders[3], full.names = TRUE)[c(1,4)] %>%
  map(.read, nam = c('DE', 'FE', 'RPM')) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Normal', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12')),
         BA = as.numeric(NA)) %>%
  bind_rows(data)
data <- 
  dir(folders[3], full.names = TRUE)[2] %>%
  map(.read, nam = c('DE', 'FE')) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Normal', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12')),
         BA = as.numeric(NA)) %>%
  bind_rows(data)
data <- 
  dir(folders[3], full.names = TRUE)[3] %>%
  map(.read2, nam = c('DE', 'FE')) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Normal', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12')),
         BA = as.numeric(NA)) %>%
  bind_rows(data)
data <- 
  dir(folders[4], full.names = TRUE) %>%
  map(.read) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Outer12', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12'))) %>%
  bind_rows(data)
data <- 
  dir(folders[5], full.names = TRUE) %>%
  map(.read) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Outer3', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12'))) %>%
  bind_rows(data)
data <- 
  dir(folders[6], full.names = TRUE) %>%
  map(.read) %>% 
  Reduce(bind_rows, .) %>%
  separate(file, c('size', 'load', 'extension')) %>% select(-extension) %>%
  mutate(size = Vectorize(function(x) switch(x, '0' = 0, '1' = 0.007, '2' = 0.014, '3' = 0.021))(size),
         load = as.numeric(load),
         type = factor('Outer6', levels = c('Normal', 'Ball', 'Inner', 'Outer3', 'Outer6', 'Outer12'))) %>%
  bind_rows(data)

data <- nest(data, DE, FE, BA)
save(data, file = 'D:/datasets/bearing_fault_cwru/processed/CWRU.dat')

data <- unnest(data)
write.csv(data, file = 'D:/datasets/bearing_fault_cwru/processed/CWRU.csv')
