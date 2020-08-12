### Her fittes model ###
library(dplyr)
library(keras)

tmp <- function() {
# data hentes
data <- get_data("./data", "clean", ".rda")

  ### Ordbog ----
  #ordbog <- konstruer_ordbog("keras", sentences = data$sentences, max_words = 11000)
  ordbog <- konstruer_ordbog("data", sentences = data$sentences, antal_ord = 500)
  #ordbog <- konstruer_ordbog("dansk_ordbog")


  ### Mapper data til ordbog ----
  # Mapper ord til indices i ordbog
  data_indices <- strsplit(data$sentences, "(?=[ )(])", perl = TRUE)
  data_indices <- lapply(data_indices, function(x) x[x != " "])
  data_indices <- lapply(data_indices, function(x) match(x, ordbog))
  head(data_indices)

  # replacer NA's
  data_indices <- lapply(data_indices,
                         function(x) {x[is.na(x)] <- length(ordbog)+1; return(x)})
  head(data_indices)

  # keras metode
  #sequences <- texts_to_sequences(tokenizer, data$sentences)
  #head(sequences)


  # opdeler i train og test
  set.seed(679)
  idx <- sample(1:2, size = length(data_indices), replace = TRUE, prob = c(0.8, 0.2))

  train_data <- data_indices[idx == 1]
  test_data <- data_indices[idx == 2]
  #sec_train <- sequences[idx == 1]
  #sec_test <- sequences[idx == 2]

  #train_label <- data$antal_komma[idx == 1]
  #test_label <- data$antal_komma[idx == 2]

  train_label_full <- data$komma_ord_index[idx == 1]
  test_label_full <- data$komma_ord_index[idx == 2]
  head(train_label_full)

  # padding
  do.call(max, lapply(train_data, function(x) length(x)))
  maxlen <- 60
  train_data <- pad_sequences(train_data, maxlen = maxlen, padding = "post")
  test_data <- pad_sequences(test_data, maxlen = maxlen, padding = "post")

  #sec_train <- pad_sequences(sec_train, maxlen = maxlen, padding = "post")
  #sec_test <- pad_sequences(sec_test, maxlen = maxlen, padding = "post")

  train_label_full <- pad_sequences(train_label_full, maxlen = maxlen, padding = "post")
  test_label_full <- pad_sequences(test_label_full, maxlen = maxlen, padding = "post")

  ### data in array ----
  # array dim -> samples * maxlen * længde af ordbog
  data_sentence_split <- strsplit(data$sentences, "(?=[ )(])", perl = TRUE)
  data_sentence_split <- lapply(data_sentence_split, function(x) x[x != " "])

  pad_vec <- rep("", maxlen)
  data_sentence_split <- lapply(data_sentence_split, function(x) c(x, pad_vec)[1:maxlen])

  data_array <- array(0, dim = c(length(data_sentence_split), maxlen, length(ordbog)))
  label_array <- array(0, dim = c(length(data_sentence_split), maxlen))

  label_array[1:length(data_sentence_split),] <-
    pad_sequences(data$komma_ord_index, maxlen = maxlen, padding = "post")

  for (i in 1:length(data_sentence_split)) {

    data_array[i,,] <- sapply(ordbog$ord, function(p){
      as.integer(p == data_sentence_split[[i]])
    })

  }

  train_data_array <- data_array[idx == 1,,]
  test_data_array <- data_array[idx == 2,,]

  label_array_train <- label_array[idx == 1,]
  label_array_test <- label_array[idx == 2,]

  ### fitter model ----
  library(keras)

  # model med embedding ---
  model_embed <- keras_model_sequential() %>%
    layer_embedding(input_dim = 550, output_dim = 16) %>% # input_length = maxlen?
    bidirectional(
      layer_lstm(units = 16, return_sequences = TRUE)
    ) %>%
    bidirectional(
      layer_lstm(units = 16, return_sequences = FALSE)
    ) %>%
    layer_flatten() %>%
    # layer_dense(units = 60, activation = "softmax") %>% # relu
    layer_dense(units = 60, activation = "softmax")

  model_embed %>% compile(
    optimizer = "rmsprop", # optimizer_rmsprop()
    loss = "mse", # categorical_crossentropy
    metric = c("mae") # accuracy
  )

  model_embed %>% fit(
    train_data, #sec_train
    train_label_full,
    epochs = 10,
    batch_size = 64,
    validation_split = 0.2
  )

  # model med array (uden embedding) ----
  model_u_embed <- keras_model_sequential() %>%
    bidirectional(
      layer_lstm(units = 16, return_sequences = TRUE,
                 input_shape = c(maxlen, nrow(ordbog))
      )
    ) %>%
    bidirectional(
      layer_lstm(units = 16, return_sequences = FALSE)
    ) %>%
    layer_flatten() %>%
    # layer_dense(units = 60, activation = "softmax") %>% # relu
    layer_dense(units = 60, activation = "softmax")

  model_u_embed %>% compile(
    optimizer = "rmsprop", # optimizer_rmsprop()
    loss = "mse", # categorical_crossentropy
    metric = c("mae") # accuracy
  )

  model_u_embed %>% fit(
    train_data_array,
    label_array_train,
    epochs = 10,
    batch_size = 64,
    validation_split = 0.2
  )

  ### test af model ----
  results <- model %>% evaluate(test_data, test_label_full) # test_label_one_hot
  results

  # baseline
  mean(rep(length(which(test_label == sample(test_label))) / length(test_label), 100))
  prop.table(table(test_label))

  pred <- model %>% predict(test_data) %>% round(digits = 4)
  pred <- cbind.data.frame(pred, pred_round = round(pred), test_label)

  length(which(pred$pred_round == pred$test_label)) / nrow(pred)

  # tilfældig sætning
  # test med embedding
  test <- c("først gik jeg en vej men så gik jeg en anden vej")
  test <- c("jeg er hjemme i det hus jeg bor i")
  test <- strsplit(test, " ")
  test <- lapply(test, function(x) match(x, ordbog))
  test <- lapply(test, function(x) {x[is.na(x)] <- length(ordbog)+1; return(x)})
  test <- pad_sequences(test, maxlen = maxlen, padding = "post")
  model_embed %>% predict(test)

  # test med array
  test <- c("først gik jeg en vej men så gik jeg en anden vej")
  test <- c("jeg er hjemme i det hus jeg bor i")
  test <- strsplit(test, "(?=[ )(])", perl = TRUE)
  test <- lapply(test, function(x) x[x != " "])
  pad_vec <- rep("", maxlen)
  test <- lapply(test, function(x) c(x, pad_vec)[1:maxlen])
  test_model_array <- array(0, dim = c(1, maxlen, nrow(ordbog)))
  test_model_array[1,,] <- sapply(ordbog$ord, function(p){
    as.integer(p == test[[1]])
  })
  model %>% predict(test_model_array)

}
