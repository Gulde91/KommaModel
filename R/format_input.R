
#' @title format_input
#'
#' @param input_str ...
#' @param numCores ...
#'
#'
#' @return
#'
format_input <- function(input_str, numCores = 4L) {

  stopifnot(is.character(input_str))
  dk_ord <- ord_og_klasse
  navne <- danske_navne$navn

  message("Splitting sentences to words")
  regex <- split_sentence_to_word_regex()
  splitted_data <- strsplit(input_str, regex, perl = TRUE)
  splitted_data <- lapply(splitted_data, function(x) x[x != " "])

  message("Handling uppercase words")
  up_impute <- "ukent_uppercase"
  dk_ord[nrow(dk_ord) + 1, ] <- c(up_impute, up_impute)
  splitted_data <- handle_uppercase(splitted_data, dk_ord$Ord, navne, up_impute)

  message("Identifying wordindex in vocabulary")
  system.time(
  sentence_match <- match_words_in_vocab(splitted_data, dk_ord$Ord, numCores = numCores)
  )

  message("Number of words not matched: ",
          sum(missing_data(splitted_data, sentence_match)$antal))

  message("Identifying person names")
  person_navn <- nrow(dk_ord) + 1
  dk_ord[person_navn, ] <- c("person_navn", "person_navn")

  tictoc::tic()
  sentence_match <- replace_na_with_vector(sentence_match, splitted_data,
                                           navne, # tolower(navne)
                                           replace_value = person_navn)
  # tolower skal fjernes?? hvad hvis navn indgår som første ord i sætning?
  # kan det håndteres inde i replace_na_with_vector funktionen?
  tictoc::toc()

  message("Identifying persons names with s (e.g. Bos or Bo's)")
  namess <- c(paste0(navne, "s"), paste0(navne, "'s")) # fjernet tolower runde om begge

  person_navn_med_s <- nrow(dk_ord) + 1
  dk_ord[person_navn_med_s, ] <- c("person_navn_med_s", "person_navn_med_s")

  tictoc::tic()
  sentence_match <- replace_na_with_vector(
    sentence_match, splitted_data, namess,
    replace_value = person_navn_med_s
  )
  tictoc::toc()

  # identificerer grammatiske tegn => skal de splittes så de ikke har samme betydning?
  message("Identifying grammatical characters")
  grammatiske_tegn <- nrow(dk_ord) + 1
  dk_ord[grammatiske_tegn, ] <- c("grammatiske_tegn", "grammatiske_tegn")

  sentence_match <- replace_na_with_vector(
    sentence_match, splitted_data,
    c("'", "-", "&", "%", "§", "(", ")", "[", "]", "/"), # ...
    replace_value = grammatiske_tegn
    ) # måske lidt for generelt, kan vel bare lade tegnene stå?


  message("Handling names in general") # Ord med stort begyndelsesbogstav markeres som navn
  navn_ <- nrow(dk_ord) + 1
  dk_ord[navn_, ] <- c("navn_", "navn_")

  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "^[[:upper:]]{1}", navn_)

  message("Handling numbers")
  # Håndterer "rene" tal
  tal_ <- nrow(dk_ord) + 1
  dk_ord[tal_, ] <- c("tal_", "tal_")

  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "^-*[0-9]*[\\.|,|:|\\d]*[0-9]+[\\.]*$", tal_
  ) # NB! tal som ikke er markeret med NA vil ikke blive markeret med tal_

  # Håndterer "-årig(s)"
  # mangler test på nedenstående: "84-årig" "2-årig" "121-årig" "-årig" "14-årige"
  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "^\\d{1,3}-årig$", which(dk_ord$Ord %in% "-årig"))

  # skal også kunne håndterer 13-14-årige
  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "\\d{1,3}-årige$", which(dk_ord$Ord %in% "-årige"))

  # Håndterer årtier "60'erne" "60erne" "1880'erne" "1880erne" "123erne" "212'erne" "1880'ernes"
  tal_årti <- nrow(dk_ord) + 1
  dk_ord[tal_årti, ] <- c("tal_årti", "sb.")
  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "^\\d{2,4}(['-]|)erne$", tal_årti)

  tal_årtis <- nrow(dk_ord) + 1
  dk_ord[tal_årtis, ] <- c("tal_årtis", "sb.")
  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "^\\d{2,4}(['-]|)ernes$", tal_årtis)

  tal_århundrede <- nrow(dk_ord) + 1
  dk_ord[tal_århundrede, ] <- c("tal_århundrede", "sb.")
  sentence_match <- replace_na_with_regex(
    sentence_match, splitted_data, "^\\d{4}-tallet$", tal_århundrede)

  # Antal og andel ord som ikke matches
  num_no_match <- sum(missing_data(splitted_data, sentence_match)$antal)
  message("Number of words not matched: ", num_no_match)
  message("Proportion of words not matches: ",
          round(num_no_match/sum(sapply(sentence_match, length)) * 100, 2), "%")
  no_match <- missing_data(splitted_data, sentence_match)
  save(no_match, file = "~/Data Science/KommaModelv1/dev/no_match.rda")

  message("Identifying word & word class in vocabulary")
  data_word <- lapply(sentence_match, function(x) dk_ord$Ord[x])
  data_word_class <- lapply(sentence_match, function(x) dk_ord$Klasse[x])

  message("Imputing for missing values")
  data_word <- lapply(data_word, function(x) {x[is.na(x)] <- "ukendt_ord"; return(x)})
  data_word_class <- lapply(data_word_class, function(x) {x[is.na(x)] <- "ukendt_klasse"; return(x)})

  # output
  return(list(sentence_match = sentence_match,
              full_sentence_splitted = splitted_data,
              data_word = data_word,
              data_word_class = data_word_class))
}

format_input_word <- function(input_data, create_word_vocab = FALSE) {

  if (create_word_vocab) {
    # opretter word vocabulary
    word_vocabulary <- unique(unlist(input_data)) %>% sort()
    usethis::use_data(word_vocabulary, overwrite = TRUE)
  }

  # mapper ord til word vocabulary index
  data_indices <- lapply(input_data, function(x) match(x, word_vocabulary))

  # output
  return(data_indices)
}

format_input_word_class <- function(input_data, create_word_class_vocab = FALSE) {

  # ordklasser
  if(create_word_class_vocab) {
    word_classes_vocabulary <- unique(unlist(input_data))
    usethis::use_data(word_classes_vocabulary, overwrite = TRUE)
  }

  # mapper ord til word vocabulary index
  match_data <- lapply(input_data, function(x) match(x, word_classes_vocabulary))

  # output
  return(match_data)
}
