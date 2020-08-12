
prepare_training_data <- function() {

  # henter data fra mappe
  data_raw <- get_data(path = "./data", file_pattern = "^raw_", gsub_pattern = ".rda")
  #data_raw <- get_data(path = "./data", file_pattern = "^raw_drengene_i_gr", gsub_pattern = ".rda")

  # sætter data sammen til en lang character
  data_raw <- paste0(data_raw, collapse = " ")

  # fjerner whitespaces hvis der er 2 eller flere i træk
  data_char <- remove_additional_whitespaces(data_raw)

  # erstatter lowercase med uppercase hvis der er lowercase efter den specificerede regex
  data_char <- swap_to_capital(data_char, c("[?!:]|[\\.\\!\\?:]{2,}"))

  # håndterer sætninger inde i parentes
  data_char <- sentence_in_parentheses(data_char)

  # substituerer udtryk
  data_char <- gsub_custom(data_char, c("#" = "", "´" = "", ";" = ","))

  # splitter sætninger til en række pr. sætning i en data.frame

  df <- split_str_to_dataframe(data_char,
                               abbreviation = c("hr.", "str.", "Nr.", "kl.",
                                                "nr.", "mr.", "Mr.", "jf.", "\\(jf.",
                                                "ca.", "s.", "\\(se bl.a.", "bl.a.",
                                                "et al.", "\\(f.eks.", "f.eks.",
                                                "\\(inkl.", "hhv.", "d.", "mrs.",
                                                "dr.", "U.S.", "H.C.")
                               )

  # fjerner sluttegn fra sætning og laver til egen feature
  df <- cbind.data.frame(df, split_end_punctuation(df$data))

  # trimmer whitespaces
  df$sentences <- trimws(df$sentences)

  # fjerner komma fra sætning og laver 2 features der angiver antallet
  # af kommaer samt positionen af ordet før kommaet
  df <- extract_comma(df, label = "sentences")

  # tager kun unique sætninger
  df <- df %>% dplyr::distinct(sentences, .keep_all = TRUE)


  ### tjekker data ----
  #tjek for stoptegn i sætning (det må godt være i forkortelser eller tal)
  #df$sentences[grepl("[.:?!,;]", df$sentences)]
  # tjek for ,' og .' og ?' og !'

  # tjek om det findes sætninger som ikke starter med stort begyndelsesbogstav,
  # et tal eller '
  # df$sentences[!substr(df$sentences, 1, 1) %in%
  #                    c(LETTERS, "Æ", "Ø", "Å", "É",
  #                      seq(0, 9, by = 1),
  #                      "'")]
  #
  # # tjek at alle strenge slutter på bogstav, tal, slutparentes eller anførselstegn
  # df$sentences[!substr(df$sentences,
  #                      nchar(df$sentences),
  #                      nchar(df$sentences)) %in%
  #                    c(letters, LETTERS,
  #                      "æ", "ø", "å", "é", "ß", "Æ", "Ø", "Å",
  #                      seq(0, 9, by = 1),
  #                      ")", "'")]
  #
  # # tjek sætninger der starter på tal og sætningen før og efter
  # for (i in 1:length(df$sentences)) {
  #   if(substr(df$sentences[i],
  #             nchar(df$sentences[i]), nchar(df$sentences[i])) %in%
  #      seq(0, 9, by = 1)) {
  #     cat(paste(df$sentences[i-1],
  #               df$sentences[i],
  #               df$sentences[i+1],
  #               sep = ". "
  #               ,"\n")
  #     )
  #   }
  # }
  #
  # # inspicerer tilfældige sætninger
  # sample(df$sentences, 10)

  ### transformerer features ----

  # laver feature der tæller antallet af ord i hver sætning
  antal_ord <- strsplit(df$sentences, split_sentence_to_word_regex(), perl = TRUE)
  antal_ord <- sapply(antal_ord, function(x) length(x[x != " "]))
  df$antal_ord <- antal_ord
  df$antal_ord_norm <- df$antal_ord / max(df$antal_ord)

  # tjekker at antal_ord er ens med længden af komma_ord_index
  stopifnot(all(df$antal_ord == sapply(df$komma_ord_index, length)))

  # fordeling af kommaer
  message("Fordeling af komma:")
  print(round(prop.table(table(df$antal_kommaer)), 3))

  # test <- df %>%
  #         dplyr::select(data, sentences, antal_ord, komma_ord_index) %>%
  #         dplyr::mutate(length_index = sapply(df$komma_ord_index, length)) %>%
  #         dplyr::filter(antal_ord != length_index)


  # tolower (bør nok ikke gøres? - eller hvad?? et problem med navne,
  # eller i og I, men hvad hvis hele ordet er skrevet i uppercase?)
  #df$sentences <- tolower(df$sentences)

  # output
  return(df)

}
