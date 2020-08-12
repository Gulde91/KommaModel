# konstruer_ordbog ----------------------------------------------------------------------
#' @title Konstruer ordbog
#'
#' @description
#' Funktionen returnerer end ordbog dannet efter brugeres valg. Her kan vælges
#' mellem 3 metoder:
#'
#'
#' @param ordbog_type Typen af ordbog. Se description
#' @param sentences Sætninger fra data
#' @param max_words num_words argument i \code{\link[keras]{text_tokenizer}}
#' @param antal_ord Antal ord der skal indgår i ordbog dannet fra data
#'
#' @return En ... .
#'
konstruer_ordbog <- function(ordbog_type, sentences = NULL, max_words = NULL,
                             antal_ord = NULL) {

  # keras metode
  if (ordbog_type == "keras" && !is.null(max_words) && !is.null(sentences)) {
    ordbog <- keras::text_tokenizer(num_words = max_words) %>%
              keras::fit_text_tokenizer(sentences)
  }
  else if (ordbog_type == "data" && !is.null(sentences)) {
    ord_vektor <- paste(sentences, collapse = " ")

    ordbog <- strsplit(ord_vektor, "(?=[ .,?!:;()\n])", perl = TRUE) %>%
      unlist() %>%
      as.data.frame()

    names(ordbog) <- "ord"

    ordbog <- dplyr::filter(ordbog, !(ord %in% c(" ", "\n", ",")))
    ordbog$ord <- trimws(ordbog$ord, which = "both")

    ordbog$ord <- tolower(ordbog$ord)
    ordbog <- ordbog %>%
      group_by(ord) %>%
      summarise(antal = n()) %>%
      arrange(desc(antal))

    # tager de n_ord mest almindelige ord
    if(!is.null(antal_ord)) n_ord <- antal_ord else n_ord <- nrow(ordbog)

    ordbog <- ordbog[1:n_ord, "ord"]
    ordbog <- arrange(ordbog, ord)
    ordbog <- ordbog$ord
  }
  else if (ordbog_type == "dansk_ordbog") {

    # ordbog <- c(dansk_ordbog$Ord, danske_navne$navn)
    # ordbog <- unique(tolower(ordbog))
    ordbog <- unique(tolower(dansk_ordbog$Ord))
  }
  else if (ordbog_type == "ordklasse") {

    dansk_ordbog$Klasse[dansk_ordbog$Klasse == ""] <- "ingen"

    # Hvis ord indgår flere gange, så vælges ordet med den klasse
    # som fremgår flest gange. Hvis der er 2 eller flere klasser som
    # indgår lige mange gange, så vælges de alle
    dansk_ordbog <- dansk_ordbog %>%
      dplyr::group_by(Ord, Klasse) %>%
      dplyr::mutate(count = n()) %>%
      dplyr::ungroup()

    dansk_ordbog <- dansk_ordbog %>%
      dplyr::group_by(Ord) %>%
      dplyr::filter(count == max(count)) %>%
      dplyr::ungroup()

    # Hvis ord går igen flere gange, så vælges den
    # klasse som er den mest hyppige i datasættet
    tabel_count <- as.data.frame(table(dansk_ordbog$Klasse), stringsAsFactors = FALSE)

    dansk_ordbog <- dplyr::left_join(dansk_ordbog, tabel_count,
                                     by = c("Klasse" = "Var1"))

    dansk_ordbog <- dansk_ordbog %>%
                    dplyr::group_by(Ord) %>%
                    dplyr::filter(Freq == max(Freq)) %>%
                    ungroup() %>%
                    data.frame()

    dansk_ordbog <- dansk_ordbog %>% dplyr::select(Ord, Klasse)

    ordbog <- dansk_ordbog %>%
              dplyr::mutate(Ord = tolower(Ord)) %>%
              dplyr::distinct(Ord, .keep_all = TRUE)

    # Grupperer klasser

    # ordbog$Klasse <- ifelse(stringr::str_detect(ordbog$Klasse, "konj"), "konj",
    #                  ifelse(stringr::str_detect(ordbog$Klasse, "udråbsord"), "udråbsord",
    #                  ifelse(stringr::str_detect(ordbog$Klasse, "lydord"), "",
    #                  ifelse(stringr::str_detect(ordbog$Klasse, "pron. sg. og pl."), "pron",
    #                         ordbog$Klasse))))
    # pron
  }

  return(ordbog)
}
