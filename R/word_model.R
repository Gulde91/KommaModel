### Her fittes model ###

train_word_model <- function() {

# data hentes
#data <- get_data("./data", file_pattern = "^clean_", gsub_pattern = ".rda")
#data <- dplyr::bind_rows(data)

# data <- prepare_training_data()[1:500,]
# data_indices <- format_input_word(data$sentences, create_word_vocab = TRUE)
# sample(data_indices, 3)
#
#
# # tjekker at længden på hver sætning er den samme som længden på data$sentence
# invisible(
#   mapply(x = data_indices, y = data$komma_ord_index,
#          FUN = function(x, y) {
#            if (length(x) != length(y)) print(x)
#          })
# )


# opdeler i train og test
set.seed(9756)
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

# model med embedding ---
model_embed <- keras_model_sequential() %>%
  layer_embedding(input_dim = 5000, output_dim = 128) %>% # input_length = maxlen?
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

test <- format_input_word(data_test_full[x, ])
test <- pad_sequences(test, maxlen = maxlen, padding = "post")

data_test_full$data[x]
model_embed %>% predict(test) %>% round(3)



# tilfældig sætning
test <- c("først gik jeg en vej men så gik jeg en anden vej")
#test <- c("jeg er hjemme i det hus jeg bor i") # komma efter hus

test <- format_input_word(test)
test <- pad_sequences(test, maxlen = maxlen, padding = "post")
model_embed %>% predict(test) %>% round(4)

}
