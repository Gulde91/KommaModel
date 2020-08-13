library(keras)
library(cloudml)
library(readr)


message("Getting data from bucket")
#data_dir <- "dev/cloud_data/"
data_dir <- gs_data_dir_local("gs://komma_model")


message("Loading train data")
train_word <- suppressMessages(read_csv(file.path(data_dir, "train_word.csv")))
train_word_class <- suppressMessages(read_csv(file.path(data_dir, "train_word_class.csv")))

train_word <- as.matrix(train_word)
train_word_class <- as.matrix(train_word_class)

message("Loading val data")
val_word <- suppressMessages(read_csv(file.path(data_dir, "val_word.csv")))
val_word_class <- suppressMessages(read_csv(file.path(data_dir, "val_word_class.csv")))

val_word <- as.matrix(val_word)
val_word_class <- as.matrix(val_word_class)

message("Loading respons data")
index_target_train <- suppressMessages(read_csv(file.path(data_dir, "index_target_train.csv")))
num_target_train <- suppressMessages(read_csv(file.path(data_dir, "num_target_train.csv")))

index_target_train <- as.matrix(index_target_train)
num_target_train <- as.matrix(num_target_train)

index_target_val <- suppressMessages(read_csv(file.path(data_dir, "index_target_val.csv")))
num_target_val <- suppressMessages(read_csv(file.path(data_dir, "num_target_val.csv")))

index_target_val <- as.matrix(index_target_val)
num_target_val <- as.matrix(num_target_val)


message("Setting pad length")
pad_len <- ncol(train_word)

### model ----
message("training model")
word_input <- layer_input(shape = list(NULL), dtype = "int32", name = "word")

encoded_word <- word_input %>%
  layer_embedding(input_dim = range(train_word)[2]+1, output_dim = 512) %>% # input_length = maxlen?
  bidirectional(
    layer_lstm(units = 256, return_sequences = TRUE)
  ) %>%
  bidirectional(
    layer_lstm(units = 128, return_sequences = TRUE)
  ) %>%
  bidirectional(
    layer_lstm(units = 128, return_sequences = FALSE)
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
    filepath = "model.h5",
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
                         epochs = 20,
                         batch_size = 64,
                         callbacks = callbacks_list,
                         validation_data = list(list(val_word, val_word_class),
                                                list(index_target_val, num_target_val))
                         )

# best_model <- keras::load_model_hdf5("model.h5")

### model params ----
# model_params <- history$params[c("batch_size", "epochs", "steps", "samples")]
# model_params <- dplyr::bind_rows(model_params)
#
# model_metrics <- history$metrics
# model_metrics <- dplyr::bind_rows(model_metrics)

# gemmer model params og kopierer det til lokal folder
# write.csv(model_params, row.names = FALSE, file = "model_params.csv")
# write.csv(model_metrics, row.names = FALSE, file = "model_metrics.csv")
#
# gs_copy("model_params.csv", "gs://komma_model")
# gs_copy("model_metrics.csv", "gs://komma_model")


### evaluation ----
# message("Getting test data")
# test_word <- suppressMessages(read_csv(file.path(data_dir, "test_word.csv")))
# test_word_class <- suppressMessages(read_csv(file.path(data_dir, "test_word_class.csv")))
#
# test_word <- as.matrix(test_word)
# test_word_class <- as.matrix(test_word_class)
#
# message("scorer test data")
# test_scores <- best_model %>% predict(list(test_word, test_word_class))
# test_scores_index <- test_scores[[1]]
# test_scores_number <- test_scores[[2]]

# gemmer test data og kopierer det til lokal folder
# write.csv(test_scores_index, row.names = FALSE,
#           file = "test_scores_index.csv")
# write.csv(test_scores_number, row.names = FALSE,
#           file = "test_scores_number.csv")
#
# gs_copy("test_scores_index.csv", "gs://komma_model")
# gs_copy("test_scores_number.csv", "gs://komma_model")



