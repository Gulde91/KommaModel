
run_v3 <- function(sentence, pad_len) {

  # sentence <- c("først gik jeg en vej men så gik jeg en anden vej",
  #               "jeg er hjemme i det hus jeg bor i")

  message("Formatterer input")
  sentence_formatted <- suppressMessages(format_input(sentence))
  sentence_word <- suppressMessages(format_input_word(sentence_formatted))
  sentence_word_class <- suppressMessages(format_input_word_class(sentence_formatted))

  sentence_word <- pad_sequences(sentence_word, maxlen = pad_len, padding = "post")
  sentence_word_class <- pad_sequences(sentence_word_class, maxlen = pad_len, padding = "post")

  message("Loader model")
  model_sti <- paste0(getwd(), "/data/")
  model <- keras::load_model_hdf5(paste0(model_sti, "model.h5"))

  message("Scorer data")
  out <- predict(model, list(sentence_word, sentence_word_class))

  # output
  out[[1]] <- round(out[[1]], 3)
  out[[2]] <- round(out[[2]], 2)

  return(list(sentence = sentence, scores = out))

}



run_v2 <- function(sentence, pad_len) {

  #sentence <- c("først gik jeg en vej men så gik jeg en anden vej")

  message("Formatterer input")
  sentence_formatted <- suppressMessages(format_input(sentence))
  sentence_word <- suppressMessages(format_input_word(sentence_formatted))
  sentence_word_class <- suppressMessages(format_input_word_class(sentence_formatted))

  sentence_word <- pad_sequences(sentence_word, maxlen = pad_len, padding = "post")
  sentence_word_class <- pad_sequences(sentence_word_class, maxlen = pad_len, padding = "post")

  message("Loader model")
  model_sti <- paste0(getwd(), "/data/")
  model <- keras::load_model_hdf5(paste0(model_sti, "model.h5"))

  message("Scorer data")
  out <- predict(model, list(sentence_word, sentence_word_class)) %>% round(3)


  return(list(sentence = sentence, scores = out))

}



run_v1 <- function(sentence) {

  #sentence <- c("først gik jeg en vej men så gik jeg en anden vej")

  message("Formatterer input")
  sentence_formatted <- suppressMessages(format_input(sentence))
  data_word <- suppressMessages(format_input_word(sentence_formatted))
  data_word_class <- suppressMessages(format_input_word_class(sentence_formatted))

  message("Loader model")
  model_sti <- paste0(getwd(), "/data/")
  word_model <- keras::load_model_hdf5(paste0(model_sti, "word_model.h5"))
  word_class_model <- keras::load_model_hdf5(paste0(model_sti, "word_class_model.h5"))

  message("Scorer med word class")
  data_word <- keras::pad_sequences(data_word, maxlen = 60, padding = "post")
  word_model_scores <- word_model %>% predict(data_word) %>% round(4)

  message("Scorer med word class model")
  data_word_class_array <- fill_3d_array(data_word_class, 60, word_classes_vocabulary)
  word_class_scores <- word_class_model %>% predict(data_word_class_array) %>% round(4)

  return(list(sentence = sentence, word_model_scores = word_model_scores,
              word_class_scores = word_class_scores))

}

