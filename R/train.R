
train_model <- function(model_nyt = "") {

  system.time(input_data <- prepare_training_data())

  kontruer_ordklasse() # Skal ordklasser grupperes?

  #system.time(formatted_data <- format_input(input_data$sentences))
  #save(formatted_data, file = "~/Data Science/KommaModelv1/formatted_data.rds")
  load("~/Data Science/KommaModelv1/dev/formatted_data.rds")

  message("tjekker data")
  tjek_data(formatted_data$data_word, input_data$komma_ord_index)
  tjek_data(formatted_data$data_word_class, input_data$komma_ord_index)


  message("fitter model")
  tictoc::tic()
  fit_model(word_data = formatted_data$data_word[1:1000],
            word_class_data = formatted_data$data_word_class[1:1000],
            comma_list_index = input_data$komma_ord_index[1:1000],
            comma_list_number = input_data$antal_kommaer[1:1000],
            model_nyt = "")
  tictoc::toc()
}

train_model_in_cloud <- function(model_nyt = "") {

  input_data <- prepare_training_data()

  kontruer_ordklasse() # Skal ordklasser grupperes?

  # formatted_data <- format_input(input_data$sentences)
  # save(formatted_data, file = "~/Data Science/KommaModelv1/dev/formatted_data.rda")
  load("./dev/formatted_data.rds")

  message("tjekker data")
  tjek_data(formatted_data$data_word, input_data$komma_ord_index)
  tjek_data(formatted_data$data_word_class, input_data$komma_ord_index)

  message("klargør og splitter data")
  test_data <- prepare_model_data(word_data = formatted_data$data_word[1:1000],
                                  word_class_data = formatted_data$data_word_class[1:1000],
                                  comma_list_index = input_data$komma_ord_index[1:1000],
                                  comma_list_number = input_data$antal_kommaer[1:1000])

  message("Uploader data til google cloud")
  cloudml::gs_rsync("dev/cloud_data", "gs://komma_model")

  message("fitter model")
  tictoc::tic()
  cloudml::cloudml_train(file = "dev/train_model_script.R",
                         config = "inst/train_config.yml",
                         collect = FALSE,
                         master_type = "standard_gpu")

  cloudml::job_collect()
  tictoc::toc()

  message("evaluerer model")
  model_nyt <- "Der er tilføjet callback i modellen."

  rmarkdown::render(input = "dev/model evaluation/Model evaluation cloud.Rmd",
                    output_file = paste0("Model evaluering cloud ", Sys.Date(),".pdf"),
                    quiet = TRUE,
                    output_dir = "dev/model evaluation",
                    params = list(model_nyt = model_nyt,
                                  test_data = test_data,
                                  model_folder = get_model_path())
                    )
}

tune_model_in_cloud <- function(model_nyt = "") {

  input_data <- prepare_training_data()

  kontruer_ordklasse() # Skal ordklasser grupperes?

  #formatted_data <- format_input(input_data$sentences)
  #save(formatted_data, file = "~/Data Science/KommaModelv1/dev/formatted_data.rda")
  load("./dev/formatted_data.rds")

  message("tjekker data")
  tjek_data(formatted_data$data_word, input_data$komma_ord_index)
  tjek_data(formatted_data$data_word_class, input_data$komma_ord_index)

  message("klargør og splitter data")
  test_data <- prepare_model_data(word_data = formatted_data$data_word[1:1000],
                                  word_class_data = formatted_data$data_word_class[1:1000],
                                  comma_list_index = input_data$komma_ord_index[1:1000],
                                  comma_list_number = input_data$antal_kommaer[1:1000])

  message("Uploader data til google cloud")
  cloudml::gs_rsync("dev/cloud_data", "gs://komma_model")

  message("fitter model")
  cloudml::cloudml_train(file = "dev/tune_model_script.R",
                         config = "inst/train_config.yml",
                         collect = FALSE #,
                         #master_type = "standard_gpu"
                         )

  cloudml::job_collect()

}
