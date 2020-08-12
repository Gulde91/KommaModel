### Her fittes model ###
library(dplyr)
library(keras)

tmp <- function() {

  # data hentes
  data <- get_data("./data", "clean", ".rda")

  ## Ordbog ----
  #ordbog <- konstruer_ordbog("keras", sentences = data$sentences, max_words = 11000)
  #ordbog <- konstruer_ordbog("data", sentences = data$sentences, antal_ord = 1000)
  ordbog <- konstruer_ordbog("dansk_ordbog")

  ### Mapper data til ordbog ----
  # Mapper ord til indices i ordbog
  # (?=[ )(]) => splitter på mellemrum ), ( og '
  splitted_data <- strsplit(data$sentences, "(?=[ )('])", perl = TRUE)
  splitted_data <- lapply(splitted_data, function(x) x[x != " "])

  ####### tjekker om ord findes i ordbog ##############

  ### Mapper ord til indices ----

  # identificere ord i ordbog
  tictoc::tic()
  ord <- tolower(dansk_ordbog$Ord)
  match_data <- lapply(splitted_data[1:1000], function(x) ifelse(x %in% ord, x, NA))
  tictoc::toc()

  sum(missing_data(splitted_data[1:1000], match_data)$antal)

  # identificere person navne
  tictoc::tic()
  match_data <- replace_na(match_data, splitted_data[1:1000],
                           tolower(danske_navne$navn),
                           replace_value = "person_navn")
  tictoc::toc()

  # imputerer for de restende manglende værdier
  match_data <- lapply(match_data, function(x) {x[is.na(x)] <- "ukendt_ord"; return(x)})

  # Opretter word vocabulary
  word_vocabulary <- unique(unlist(match_data)) %>% sort()

  # mapper ord til word vocabulary index
  data_indices <- lapply(match_data, function(x) match(x, word_vocabulary))


  #####################################################


  data_indices <- lapply(splitted_data, function(x) match(x, ordbog))
  head(data_indices)
  head(splitted_data)

  no_match <- missing_data(splitted_data, data_indices)

  # data_indices_val <-
  #   map_sentence_to_dictionary_index(data$sentences, "(?=[ )(])",
  #                                    ordbog, length(ordbog)+1)


  ### replace NA's ----
  # identificere NA pga navn
  tictoc::tic()
  data_indices <- replace_na(data_indices, splitted_data,
                             tolower(danske_navne$navn),
                             replace_value = 400000) # find ud af bedre værdi
  tictoc::toc()
  head(data_indices)

  no_match_1 <- missing_data(splitted_data, data_indices)

  sum(no_match$antal)
  sum(no_match_1$antal)

  # Imputerer for de restende manglende værdier
  data_indices <- lapply(data_indices,
                         function(x) {x[is.na(x)] <- length(ordbog)+1; return(x)})
  head(data_indices)

  # opdeler i train og test
  set.seed(679)
  idx <- sample(1:2, size = length(data_indices),
                replace = TRUE, prob = c(0.8, 0.2))

  train_data <- data_indices[idx == 1]
  test_data <- data_indices[idx == 2]

  train_label_full <- data$komma_ord_index[idx == 1]
  test_label_full <- data$komma_ord_index[idx == 2]
  head(train_label_full)

  # padding
  do.call(max, lapply(train_data, function(x) length(x)))
  maxlen <- 60

  train_data <- pad_sequences(train_data, maxlen = maxlen, padding = "post")
  test_data <- pad_sequences(test_data, maxlen = maxlen, padding = "post")
  train_data[1, ]

  train_label_full <- pad_sequences(train_label_full, maxlen = maxlen,
                                    padding = "post")
  test_label_full <- pad_sequences(test_label_full, maxlen = maxlen,
                                   padding = "post")
  train_label_full[1, ]

  ### fitter model ----
  library(keras)

  # model med embedding ---
  model_embed <- keras_model_sequential() %>%
    layer_embedding(input_dim = 400001, output_dim = 128) %>% # input_length = maxlen?
    bidirectional(
      layer_lstm(units = 32, return_sequences = TRUE)
    ) %>%
    bidirectional(
      layer_lstm(units = 32, return_sequences = FALSE)
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
    train_data,
    train_label_full,
    epochs = 10,
    batch_size = 256,
    validation_split = 0.2
  )


  ### test af model ----
  results <- model_embed %>% evaluate(test_data, test_label_full) # test_label_one_hot
  results

  # random test
  data_test_full <- data[idx == 2, ]
  x <- sample(1:nrow(data_test_full), 1)

  test <- strsplit(data_test_full$sentences[x], " ")
  test <- lapply(test, function(x) match(x, ordbog))
  test <- lapply(test, function(x) {x[is.na(x)] <- length(ordbog)+1; return(x)})
  test <- pad_sequences(test, maxlen = maxlen, padding = "post")
  data_test_full$data[x]
  model_embed %>% predict(test) %>% round(3)


  # tilfældig sætning
  # test med embedding
  test <- c("først gik jeg en vej men så gik jeg en anden vej")
  test <- c("jeg er hjemme i det hus jeg bor i") # kommer efter hus
  test <- strsplit(test, " ")
  test <- lapply(test, function(x) match(x, ordbog))
  test <- lapply(test, function(x) {x[is.na(x)] <- length(ordbog)+1; return(x)})
  test <- pad_sequences(test, maxlen = maxlen, padding = "post")
  model_embed %>% predict(test)

}
