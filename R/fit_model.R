
#' @title fit_model
#'
#' @param word_data ...
#' @param word_class_data ...
#' @param comma_list_index ...
#' @param comma_list_number ...
#'
#' @import keras
#' @importFrom rmarkdown render
#'
#' @return ...
#'
fit_model <- function(word_data, word_class_data,
                      comma_list_index, comma_list_number, model_nyt = "") {

  # data tjek
  stopifnot(length(word_data) == length(comma_list_index),
            length(word_class_data) == length(comma_list_index))

  # creating training and test split
  num_test_samples <- min(length(word_data) * 0.2, 10000)
  p <- num_test_samples / length(word_data)
  set.seed(9756)
  idx <- sample(1:3, size = length(word_data),
                replace = TRUE, prob = c(1-2*p, p, p))

  # word vocabulary (created on training data)
  word_vocabulary <- unique(unlist(word_data[idx == 1]))
  usethis::use_data(word_vocabulary, overwrite = TRUE)

  # word class vocabulary (created on training data)
  word_classes_vocabulary <- unique(unlist(word_class_data))
  usethis::use_data(word_classes_vocabulary, overwrite = TRUE)

  # padding length
  pad_len <- do.call(max, lapply(word_data[idx == 1], function(x) length(x)))

  ## forbereder word data ----
  train_word <- lapply(word_data[idx == 1], function(x) match(x, word_vocabulary))
  val_word <- lapply(word_data[idx == 2], function(x) match(x, word_vocabulary))
  test_word <- lapply(word_data[idx == 3], function(x) match(x, word_vocabulary))
  #train_word[1]

  ukendt_ord_idx <- match("ukendt_ord", word_vocabulary)
  val_word <- lapply(val_word, function(x) {x[is.na(x)] <- ukendt_ord_idx; return(x)})
  test_word <- lapply(test_word, function(x) {x[is.na(x)] <- ukendt_ord_idx; return(x)})

  train_word <- pad_sequences(train_word, pad_len, padding = "post")
  val_word <- pad_sequences(val_word, pad_len, padding = "post")
  test_word <- pad_sequences(test_word, pad_len, padding = "post")
  #train_word[1, ]

  ## forbereder word class data ----
  train_word_class <- lapply(word_class_data[idx == 1], function(x) match(x, word_classes_vocabulary))
  val_word_class <- lapply(word_class_data[idx == 2], function(x) match(x, word_classes_vocabulary))
  test_word_class <- lapply(word_class_data[idx == 3], function(x) match(x, word_classes_vocabulary))

  ukendt_klasse_idx <- match("ukendt_klasse", word_classes_vocabulary)
  val_word_class <- lapply(val_word_class, function(x) {x[is.na(x)] <- ukendt_klasse_idx; return(x)})
  test_word_class <- lapply(test_word_class, function(x) {x[is.na(x)] <- ukendt_klasse_idx; return(x)})

  train_word_class <- pad_sequences(train_word_class, pad_len, padding = "post")
  val_word_class <- pad_sequences(val_word_class, pad_len, padding = "post")
  test_word_class <- pad_sequences(test_word_class, pad_len, padding = "post")

  ## respons ----
  index_target_train <- pad_sequences(comma_list_index[idx == 1], pad_len, padding = "post")
  index_target_val <- pad_sequences(comma_list_index[idx == 2], pad_len, padding = "post")
  index_target_test <- pad_sequences(comma_list_index[idx == 3], pad_len, padding = "post")

  num_target_train <- comma_list_number[idx == 1]
  num_target_val <- comma_list_number[idx == 2]
  num_target_test <- comma_list_number[idx == 3]

  ## model ----
  # encoder word model
  word_input <- layer_input(shape = list(NULL), dtype = "int32", name = "word")

  encoded_word <- word_input %>%
    layer_embedding(input_dim = range(train_word)[2]+1, output_dim = 512) %>% # input_length = maxlen?
    bidirectional(
      layer_lstm(units = 128, return_sequences = TRUE)
    ) %>%
    bidirectional(
      layer_lstm(units = 64, return_sequences = TRUE)
    ) %>%
    bidirectional(
      layer_lstm(units = 64, return_sequences = FALSE)
    )

  # encoder word_class model
  word_class_input <- layer_input(shape = list(NULL), dtype = "int32", name = "word_class")

  encoded_word_class <- word_class_input %>%
    layer_embedding(input_dim = range(train_word_class)[2]+1,
                    output_dim = min(range(train_word_class)[2], 32)) %>% # input_length = maxlen?
    bidirectional(
      layer_lstm(units = 32, return_sequences = TRUE
      )
    ) %>%
    bidirectional(
      layer_lstm(units = 16, return_sequences = FALSE
      )
    )


  # concatenated layer
  concat <- layer_concatenate(list(encoded_word, encoded_word_class))

  # multi output
  index_pred <- concat %>% layer_dense(units = pad_len, activation = "sigmoid")
  num_pred <- concat %>% layer_dense(units = 1) # regression activation

  model <- keras_model(list(word_input, word_class_input),
                       list(index_pred, num_pred))

  # callback
  callbacks_list <- list(
    callback_early_stopping(
      monitor = "val_loss",
      patience = 2
    ),
    callback_model_checkpoint(
      filepath = "test_model.h5",
      monitor = "val_loss",
      save_best_only = TRUE
    )
  )

  # compile
  model %>% compile(
    optimizer = "adam", # optimizer_rmsprop(), rmsprop
    loss = list("categorical_crossentropy", "mse"),
    loss_weights = list(1, 0.5),
    metric = c("accuracy") # mae
  )

  history <- model %>% fit(list(train_word, train_word_class),
                           list(index_target_train, num_target_train),
                           epochs = 10,
                           batch_size = 64, #512,
                           callbacks = callbacks_list,
                           validation_data = list(list(val_word, val_word_class),
                                                  list(index_target_val, num_target_val))
                           )


  model_params <- list(test_word = test_word,
                       test_word_class = test_word_class,
                       index_target_test = index_target_test,
                       num_target_test = num_target_test,
                       model_nyt = model_nyt,
                       model_history = history)

  message("Gemmer model")
  sti <- paste0(getwd(), "/data/")
  #save_model_hdf5(model, filepath = paste0(sti, "model.h5"))
  save(model_params, file = paste0(sti, "model_params.rda"))

  tictoc::tic()
  rmarkdown::render(input = "dev/model evaluation/Model evaluation.rmd",
                    output_file = paste0("Model evaluering ", Sys.Date(),".pdf"),
                    quiet = TRUE,
                    output_dir = "dev/Model evaluation",
                    params = model_params
                   )
  tictoc::toc()
}



fit_word_model <- function(independent, response, data_split, pad_len) {

  # splitter data
  train_data <- independent[data_split == 1]
  test_data <- independent[data_split == 2]
  #train_data[1]

  train_label <- response[data_split == 1]
  test_label <- response[data_split == 2]
  # train_label[1]

  # padding
  train_data <- pad_sequences(train_data, maxlen = pad_len, padding = "post")
  test_data <- pad_sequences(test_data, maxlen = pad_len, padding = "post")
  # train_data[1, ]

  train_label <- pad_sequences(train_label, maxlen = pad_len, padding = "post")
  test_label <- pad_sequences(test_label, maxlen = pad_len, padding = "post")
  #train_label[1, ]

  # model
  word_model <- keras_model_sequential() %>%
    layer_embedding(input_dim = 16000, output_dim = 128) %>% # input_length = maxlen?
    bidirectional(
      layer_lstm(units = 32, return_sequences = TRUE)
    ) %>%
    bidirectional(
      layer_lstm(units = 32, return_sequences = FALSE)
    ) %>%
    layer_flatten() %>%
    # layer_dense(units = 60, activation = "softmax") %>% # relu
    layer_dense(units = 60, activation = "softmax")


  word_model %>% compile(
    optimizer = "rmsprop", # optimizer_rmsprop()
    loss = "mse", # categorical_crossentropy
    metric = c("mae") # accuracy
  )

  word_model %>% fit(
    train_data,
    train_label,
    epochs = 15,
    batch_size = 256,
    validation_split = 0.2
  )

  message("Gemmer model")
  sti <- paste0(getwd(), "/data/")
  keras::save_model_hdf5(word_model, filepath = paste0(sti, "word_model.h5"))

}

fit_word_class_model <- function(independent, response, data_split, pad_len) {

  # udfylder arrays (og padding)
  data_array <- fill_3d_array(independent, pad_len, word_classes_vocabulary)
  label_array <- keras::pad_sequences(response, pad_len, padding = "post")

  # opdeler i trænings og test data
  train_data <- data_array[data_split == 1,,]
  test_data <- data_array[data_split == 2,,]

  train_label <- label_array[data_split == 1,]
  test_label <- label_array[data_split == 2,]

  # model med array (uden embedding) ----
  word_class_model <- keras_model_sequential() %>%
    bidirectional(
      layer_lstm(units = 16, return_sequences = TRUE,
                 input_shape = c(pad_len, length(word_classes_vocabulary))
      )
    ) %>%
    bidirectional(
      layer_lstm(units = 16, return_sequences = FALSE)
    ) %>%
    layer_flatten() %>% # input_shape ?
    # layer_dense(units = 60, activation = "relu") %>%
    layer_dense(units = pad_len, activation = "softmax")

  word_class_model %>% compile(
    optimizer = "rmsprop", # optimizer_rmsprop()
    loss = "mse", # categorical_crossentropy
    metric = c("mae") # accuracy
  )

  word_class_model %>% fit(
    train_data,
    train_label,
    epochs = 15,
    batch_size = 256,
    validation_split = 0.2
  )

  message("Gemmer word_class model")
  sti <- paste0(getwd(), "/data/")
  keras::save_model_hdf5(word_class_model, filepath = paste0(sti, "word_class_model.h5"))
}



