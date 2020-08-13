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
index_target_train <- as.matrix(index_target_train)

index_target_val <- suppressMessages(read_csv(file.path(data_dir, "index_target_val.csv")))
index_target_val <- as.matrix(index_target_val)

num_target_train <- suppressMessages(read_csv(file.path(data_dir, "num_target_train.csv")))
num_target_train <- as.matrix(num_target_train)

num_target_val <- suppressMessages(read_csv(file.path(data_dir, "num_target_val.csv")))
num_target_val <- as.matrix(num_target_val)


message("Setting pad length")
pad_len <- ncol(train_word)

### Tune flags
FLAGS <- flags(
  flag_integer("embeddin_word", 128),
  flag_integer("layer_word_lstm_1", 64),
  flag_integer("layer_word_lstm_2", 64),
  flag_integer("layer_word_lstm_3", 64),
  flag_integer("embeddin_word_class", 32),
  flag_integer("layer_word_class_lstm_1", 16),
  flag_integer("layer_word_class_lstm_2", 16)
  )

### model ----
message("training model")

# encoder word model
word_input <- layer_input(shape = list(NULL), dtype = "int32", name = "word")

encoded_word <- word_input %>%
  layer_embedding(input_dim = range(train_word)[2]+1,
                  output_dim = FLAGS$embeddin_word) %>% # input_length = maxlen?
  bidirectional(
    layer_lstm(units = FLAGS$layer_word_lstm_1, return_sequences = TRUE)
  ) %>%
  bidirectional(
    layer_lstm(units = FLAGS$layer_word_lstm_2, return_sequences = TRUE)
  ) %>%
  bidirectional(
    layer_lstm(units = FLAGS$layer_word_lstm_3, return_sequences = FALSE)
  )

# encoder word_class model
word_class_input <- layer_input(shape = list(NULL), dtype = "int32", name = "word_class")

encoded_word_class <- word_class_input %>%
  layer_embedding(input_dim = range(train_word_class)[2]+1,
                  output_dim = min(range(train_word_class)[2],
                                   FLAGS$embeddin_word_class)) %>% # input_length = maxlen?
  bidirectional(
    layer_lstm(units = FLAGS$layer_word_class_lstm_1, return_sequences = TRUE)
  ) %>%
  bidirectional(
    layer_lstm(units = FLAGS$layer_word_class_lstm_2, return_sequences = FALSE)
  )

# concatenated layer
concat <- layer_concatenate(list(encoded_word, encoded_word_class))

# output
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
                         epochs = 3,
                         batch_size = 64,
                         callbacks = callbacks_list,
                         validation_data = list(list(val_word, val_word_class),
                                                list(index_target_val, num_target_val))
                         )

#keras::save_model_hdf5(model)
