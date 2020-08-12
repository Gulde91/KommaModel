### Her fittes model ###

train_word_class_model <- function(data_class) {

# data hentes
data <- prepare_training_data()
data_class <- format_input_word_class(data$sentences, create_word_class_vocab = TRUE)

# danner ord med tilhørende ordklasse
ordklasse <- kontruer_ordklasse() # Skal ordklasser grupperes?

### data in array ----
mapped_data <- map_sentence_to_word_class(data$sentence, ordklasse)

data_sentence_split <- mapped_data$mapped_sentences
head(data_sentence_split, 3)

data_missing <- mapped_data$data_missing

## håndterer NA


## imputerer for NA
data_sentence_split <- lapply(data_sentence_split,
                              function(x) {x[is.na(x)] <- "ukendt_klasse"; return(x)})


# ordklasser
#ordklasser <- c(unique(ordbog$Klasse), "ukendt")
ordklasser <- unique(unlist(data_sentence_split))

# Finder index til train og test
set.seed(679)
idx <- sample(1:2, size = length(data_sentence_split), replace = TRUE, prob = c(0.8, 0.2))

# padding length
maxlen <- do.call(max, lapply(data_sentence_split[idx == 1], length))


# udfylder arrays (og padding)
data_array <- fill_3d_array(data_sentence_split, maxlen, ordklasser)
label_array <- keras::pad_sequences(data$komma_ord_index, maxlen, padding = "post")

# opdeler i trænings og test data
train_data_array <- data_array[idx == 1,,]
test_data_array <- data_array[idx == 2,,]

label_array_train <- label_array[idx == 1,]
label_array_test <- label_array[idx == 2,]

### fitter model ----

# model med array (uden embedding) ----
model_ordklasser <- keras_model_sequential() %>%
  bidirectional(
     layer_lstm(units = 16, return_sequences = TRUE,
                input_shape = c(maxlen, length(ordklasser))
      )
    ) %>%
  bidirectional(
     layer_lstm(units = 16, return_sequences = FALSE)
    ) %>%
  layer_flatten() %>%
  # layer_dense(units = 60, activation = "relu") %>%
  layer_dense(units = maxlen, activation = "softmax")

model_ordklasser %>% compile(
    optimizer = "rmsprop", # optimizer_rmsprop()
    loss = "mse", # categorical_crossentropy
    metric = c("mae") # accuracy
)

model_ordklasser %>% fit(
  train_data_array,
  label_array_train,
  epochs = 10,
  batch_size = 64,
  validation_split = 0.2
)

### test af model ----
results <- model_ordklasser %>% evaluate(test_data_array, label_array_test) # test_label_one_hot
results

# tilfældig sætning
test <- c("først gik jeg en vej men så gik jeg en anden vej") # men
#test <- c("jeg er hjemme i det hus jeg bor i") #

test <- map_sentence_to_word_class(test, ordbog)$mapped_sentences # tjek lige op på denne
test <- fill_3d_array(test, maxlen, ordklasser)

model_ordklasser %>% predict(test)

}
