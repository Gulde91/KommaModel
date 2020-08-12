
# split_str_to_dataframe ------------------------------------------------------
#' @title split_str_to_dataframe
#'
#' @description
#' Funktionen opdeler i sætninger og tager delimiteren med. Her betinges på at
#' delimiteren skal være efterfulgt af mindst ét white space eller linjeskifte
#' \code{\n} samt at det er efterfulgt af et stort startbogstav eller tal.
#' Dette for at undgå, at en sætning ikke bliver splittet på en forkortelse.
#'
#' @param string Input streng
#' @param abbreviation Forkortelser som fx. "mr.", som efterfølges af et stort
#'                     start bogstav. Det parses ind i regex expression, så
#'                     sætningen ikke opdeles på disse forkortelser. For hver
#'                     input forkortelse indsættes et white space før
#'                     forkortelsen, således at et afsluttende ord i en sætning
#'                     som tilfældigvis ender på forkortelsen, ikke bliver
#'                     opdelt. Ligeledes indsættes et ^ (regex anchor), således
#'                     at sætningen ikke splittes, selvom forkortelsen er i
#'                     starten af sætningen.
#'
#' @return En data.frame med en varible, hvor hver række indeholder en sætning.
#'
split_str_to_dataframe <- function(string, abbreviation = NULL) {

  # tjek for om linjeskift indgår i strengen (\n)
  if(grepl("\n", string)) {
    message("Her findes markering for linjeskift i strengen, derfor benyttes remove_additional_whitespaces()")
    string <- remove_additional_whitespaces(string)
  }

  # Betinger på, at forkortelsen, skal have et whitespace før forkortelsen,
  # eller at forkortelsen skal være i starten af sætningen.
  parse_abbr <- paste0(paste0("\\s", abbreviation, collapse = "|"), "|",
                       paste0("^", abbreviation, collapse = "|"))

  # regex expression
  regex_expr <- paste0("(?<=[.?!:])+",
                       "(?<!",parse_abbr,")",
                       "[\\s]+(?=[A-ZÆØÅÉ0-9('-])")

  # splitte sætning
  data <- strsplit(string, regex_expr, perl = TRUE)
  data <- unlist(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  return(data)
}

# identify_abbreviation -------------------------------------------------------
#' @title Identify abbrevation
#'
#' @description Funktionen undersøger om der i tekst-stregnen er forkortelser,
#'              som eksempelvis "bl.a." eller "fx.". Det gøres ved at
#'              identificere steder i strengen som har et stoptegn (punktum,
#'              spørgsmålstegn, udråbstegn, kolon) og det efterfølgende tegn
#'              er et space efterfuldt af et lowercase bogstav.
#'
#' @param string En tekststreng.
#' @param output_length Længden på strengen der printes til konsolen omkring
#'                      den fundne forkortelse.
#'
#' @return Tekst der opfylder beskrivelsen i description.
#'
identify_abbreviation <- function(string, output_lenght = 10) {

  regex_expr <- "(?<=[.?!:;])[ |\n]*[a-zæøå]"

  index <- unlist(gregexpr(regex_expr, string, perl = TRUE))

  output <- list()

  for (i in 1:length(index)) {
    output[i] <- substr(string,
                        index[i] - output_lenght,
                        index[i] + output_lenght)
  }

  return(output)
}

# identify_number_before_punctuation ------------------------------------------
#' @title Identificer tekst hvor et tal efterfølges af et sluttegn.
#'
#' @description Identificer tekst hvor er tal efterfølges af et sluttegn.
#'
#' @param streng En tekststreng.
#' @param output_lenght Længden på strengen der printes til konsolen omkring
#'                      den fundne forkortelse.
#'
#' @return Tekst der opfylder beskrivelsen i description.
#'
identify_number_before_punctuation <- function(streng, output_lenght = 10) {

  regex_expr <- "[0-9][.?!:;]"

  index <- unlist(gregexpr(regex_expr, streng, perl = TRUE))

  output <- list()

  for (i in 1:length(index)) {
    output[i] <- substr(streng, index[i] - output_lenght, index[i] + output_lenght)
  }

  return(output)
}

# swap_to_capital -------------------------------------------------------------
#' @title Swap to capital
#'
#' @description Hvis der efter et "?", "!", ":" (eller andet, kommer an på
#'              \code{streng}) er et lille begyndelsesbogstav, så rettes det til
#'              det tilsvarende store begyndelsesbogstav. I søge-strengen
#'              betinges på, at der skal være et ikke whitespace symbol før,
#'              og at der skal være et, eller flere, mellemrum eller
#'              linjeskifte \code{(\n)} efter.
#'
#' @param streng En tekststreng.
#' @param match_symbol
#'
#' @return Inputstrengen med stort begyndelsesbogstav efter ? og !.
#'
swap_to_capital <- function(string, match_symbol = NULL) {

  regex_expr <- paste0("(\\S)(", # \\S ikke whitespace-type character
                        match_symbol, # "[?!:]|[\\!\\?\\.:]{2,}"
                        ")(\\s+)([a-zæøå])")

  string <- gsub(regex_expr, "\\1\\2\\3\\U\\4", string, perl = TRUE)

  return(string)
}

# gsub_custom -----------------------------------------------------------------
#' @title gsub custom
#'
#' @description Funktionen er en wrapper rundt om \code{base::gsub}, blot med
#'              en forlykke uden om. Input er en teststreng hvor en eller flere
#'              mønstre ønskes byttet ud. De mønstre der ønskes erstattet,
#'              skal speficeres i en vector som:
#'              vek <- c("nuværende mønster 1" = "nyt mønster 1",
#'              "nuværende mønster 2" = "nyt mønster 2").
#'
#'
#' @param streng En tekststreng
#' @param replace En named vektor med mønstre der skal erstattes
#'
#' @return Input streng hvor gamle mønstre er udbyttet med de nye specificerede
#'         mønstre
#'
gsub_custom <- function(string, replace) {

  if (!is.null(replace)) {
    for (i in 1:length(replace)) {
      string <- gsub(names(replace[i]), replace[[i]], string, perl = TRUE)
    }
  }

  return(string)
}

# remove_additional_whitespaces ------------------------------------------------
#' @title Remove additional whitespaces
#'
#' @description Hvis der i en streng fremgår mere end 1 whitespace i træk, så
#'              fjernes de. Denne funktion fjerner også linjeskift. Derudover
#'              fjernes whitespaces før og efter strengen.
#'
#' @param string En tekststreng
#'
#' @return Input strengen hvor overflødige mellemrum er fjernet.
#'
remove_additional_whitespaces <- function(string) {

  string <- gsub("\\s+"," ", string)
  string <- trimws(string)

  return(string)
}

# split_end_punctuation -------------------------------------------------------
#' @title split end punctuation
#'
#' @description Funktionen tager input strengen og opslitter i sætning og
#'              sluttegn.
#'
#' @param string En character vektor
#'
#' @return En matrix med kolonnerne sentence og end_punctuation
#'
split_end_punctuation <- function(char_vec) {

  tmp <- unlist(gregexpr("[.!?:]+$", char_vec))
  sentences <- gsub("[.!?:]+$","", char_vec)
  end_punctuation <- substr(char_vec, tmp, nchar(char_vec))

  return(cbind.data.frame(sentences, end_punctuation, stringsAsFactors = FALSE))
}

# extract_comma ---------------------------------------------------------------
#' @title extract comma
#'
#' @description Funktionen tæller antallet af kommaer i \code{label} i
#'              \code{input_data}, og danner en ny feature \code{antal_komma}.
#'              Derefter identificeres positionen af ordet før kommaet og
#'              returneres i featuren \code{komma_position}. Til sidst fjernes
#'              kommaer i \code{label}. \cr
#'              Hvis der i sætningen indgår semikolon, så opfattes det som
#'              et komma.
#'
#' @param input_data data.frame
#' @param label Relevant feature (character-class)
#'
#' @return input_data plus 2 nye features: antal komma og komma_position. I
#'         \code{label}-kolonnen er kommaer fjernet.
#'
extract_comma <- function(input_data, label) {

  stopifnot(exists(label, input_data), is.character(input_data[[label]]))

  # Hvis der i sætningen indgår ; (semikolon), så erstattes det med et komma
  input_data[[label]] <- gsub(pattern = ";", ",", input_data[[label]])

  # tæller antal kommaer i sætning og laver en target feature fra den. For at
  # undgå at decimaltal, som eksempelvis 23,42 tæller som et komma, betinges her
  # på, at der skal være et space efter komma.
  input_data$antal_kommaer <- stringr::str_count(input_data[[label]], ', ')

  # splitter sætning til vecktor med ord og tegn
  regex <- split_sentence_to_word_regex()
  sen_split <- strsplit(input_data[[label]], regex, perl = TRUE)
  sen_split <- lapply(sen_split, function(x) x[x != " "])

  # finder position for komma. trækker fra for at identificere
  # det korrekte ord før komma
  komma_pos <- lapply(sen_split, function(x) grep("^,$", x))
  komma_pos <- lapply(komma_pos, function(x) x - 1:length(x))

  # fjerner komma fra sen_split
  sen_split <- lapply(sen_split, function(x) x[x != ","])


  # laver binær vektor med indikator for komma
  komma_ord_index <- lapply(sen_split, function(x) vector("integer", length(x)))
  komma_ord_index <- mapply(x = komma_ord_index,
                            y = komma_pos,
                            FUN = function(x, y) {
                              x[y] <- 1
                              return(x)
                            })

  # sætter binær vektor på input_data
  input_data$komma_ord_index <- komma_ord_index

  # fjerner kommaer i strengen
  input_data[[label]] <- gsub(', ', ' ', input_data[[label]])

  # output
  return(input_data)
}


# vectorize sequences ---------------------------------------------------------
#' @title vectorize sequences
#'
#' @description Mapper til binær matrice
#'
#' @note Denne funktion er ikke testet!
#'
#' @param sequence ...
#' @param dimension ...
#'
#' @return ...
#'
vectorize_sequences <- function(sequence, dimension) {
  results <- matrix(0, nrow = length(sequence), ncol = dimension)
  for (i in 1:length(sequence))
    results[i, sequence[[i]]] <- 1
  return(results)
}

# to_one_hot ------------------------------------------------------------------
#' to_one_hot
#'
#' @param labels
#' @param dimension
#'
#' @return
#' @export
#'
#' @examples
to_one_hot <- function(labels, dimension) {
  result <- matrix(0, nrow = length(labels), ncol = dimension)
  for (i in 1:length(labels)) {
    result[i, labels[[i]] + 1] <- 1
  }
  result
}




# get_data --------------------------------------------------------------------
#' @title get_data
#'
#' @description Funktionen henter r-datasæt fra den specificerede \code{path}
#'              og laver en \code{\link[dplyr]{bind_rows}}.
#'
#' @param path En character vector med stien til det data der skal hentes
#' @param file_pattern Et valgfrit regex udtryk til at hente specifikke datasæt
#' @param gsub_pattern Endelsen på de hentede filer. Eksemelvis ".rda"
#'
#' @return Data fra den specifirede sti i en liste.
#'
get_data <- function(path, file_pattern = NULL, gsub_pattern = "") {

 file_list <- list.files(path = path, pattern = file_pattern)
 file_list <- gsub(gsub_pattern, "", file_list)
 file_list <- lapply(file_list, get)

 return(file_list)
}

# map_sentence_to_dictionary_index --------------------------------------------
#' @title map_sentence_to_dictionary_index
#'
#' @description Funktionen mapper hvert ord i \code{sentence} til det korrekte
#'              index i \code{dictionary}. Det gøres ved at opslitte hver
#'              sætning efter regular expression udtrykket i \code{split}.
#'              Hvis et ord ikke findes i \code{dictionary}, så indsættes
#'              værdien defineret i \code{missing}.
#'
#' @param sentence En character vector med sætninger
#' @param split En character vector med regular expression
#' @param dictionary En character vector med ord
#' @param missing Inputationsværdi for manglende værdier
#'
#' @return En liste hvor hvert element er en numerisk vector
#'
# map_sentence_to_dictionary_index <- function(sentence, split, dictionary,
#                                              missing) {
#   stopifnot(is.character(dictionary))
#
#   sentence <- strsplit(sentence, split, perl = TRUE)
#   sentence <- lapply(sentence, function(x) x[x != " "])
#   sentence <- lapply(sentence, function(x) match(x, dictionary))
#   sentence <- lapply(sentence, function(x) {x[is.na(x)] <- missing; return(x)})
#
#   return(sentence)
# } # bruges denne nogen steder??

# map_sentence_to_word_class --------------------------------------------------
#' @title map_sentence_to_word_class
#'
#' @description ... bruger \code{\link{split_sentence_to_word_regex}}
#'
#' @param sentence En character vector med sætninger
#' @param dictionary En dataframe med 2 kolonner; Ord og Klasse
#'
#' @return En liste hvor hvert element er en character vector
#'
map_sentence_to_word_class <- function(sentence, dictionary) {

  stopifnot(is.data.frame(dictionary),
            names(dictionary) == c("Ord", "Klasse"))

  # splitter data
  regex <- split_sentence_to_word_regex()
  splitted_sentence <- strsplit(sentence, regex, perl = TRUE)
  splitted_sentence <- lapply(splitted_sentence, function(x) x[x != " "])

  # matcher data med ordbog
  sentence_match <- lapply(splitted_sentence, function(x) match(x, dictionary$Ord))
  sentence_match <- lapply(sentence_match, function(x) dictionary$Klasse[x])

  # input for manglende værdier
  #sentence_match_out <- lapply(sentence_match, function(x) {x[is.na(x)] <- missing; return(x)})

  # identificerer manglende match
  data_missing <- missing_data(splitted_sentence, sentence_match)

  return(list(mapped_sentences = sentence_match, data_missing = data_missing))
}

# fill_3d_array ---------------------------------------------------------------
#' @title fill_3d_array
#'
#' @description Funktionen mapper inputlisten \code{sentence_list} til et 3d
#'              array hvor index [1,,] angiver
#'
#' @param sentence_list Liste med character vectore.
#' @param pad_len Padding længde
#' @param features character vector med navne på de featuers der indgår
#'
#' @return Et 3d array med dimensionen {length(sentence_list), pad_len,
#'         length(features)}
#'
fill_3d_array <- function(sentence_list, pad_len, features) {

  pad_vec <- rep("", pad_len)
  sentence_list <- lapply(sentence_list, function(x) c(x, pad_vec)[1:pad_len])

  data_array <- array(0, dim = c(length(sentence_list), pad_len, length(features)))

  for (i in 1:length(sentence_list)) {
    data_array[i,,] <- sapply(features, function(p) {
      as.integer(p == sentence_list[[i]])
    })
  }

  return(data_array)
}



# sentence_in_parentheses -----------------------------------------------------
#' @title sentence_in_parentheses
#'
#' @description Hvis der i \code{string} indgår en parentes, hvor der inde i
#'              parentesen indgår seperate sætninger (adskilt med ., ! eller ?)
#'              så vil der før parentesen starter, indsættes et punktum.
#'
#' @details Hvis der inde i sætningen indgår en forkortelse, så vil den også
#'          blive splittet.
#'
#' @param string En character vector
#'
#' @return En character-vector identisk med \code{string}, men med et indsat
#'         punktum før en parantes, hvis parantesen indeholder flere sætninger
#'         (defineret ved at punktum, udråbstegn eller spørgsmålstegn indgår).
#'
sentence_in_parentheses <- function(string) {

  stopifnot(is.character(string), length(string) == 1)

  # tjek for om linjeskift indgår i strengen (\n)
  if(grepl("\n", string)) {
    message("Her findes markering for linjeskift i strengen, derfor benyttes remove_additional_whitespaces()")
    string <- remove_additional_whitespaces(string)
  }

  # regex expression
  # blok 1: der må ikke være et sluttegn (punktum, spørgsmåls- eller udråbstegn)
  # blok 2: der skal være mindst et whitespace
  # blok 3: det skal være en start parentes,
  # blok 4: her må være nul eller flere karaktertegn (bogstaver, tal), mellemrum
  #         eller komma
  # blok 5: her må være et eller flere sluttegn (punktum, spørgsmåls-
  #         eller udråbstegn),
  # blok 6: her må være alle mulige kombinationer af karaktertegn, mellemrum,
  #         sluttegn og kommaer.
  # blok 7: her skal være en slutparentes
  # blok 8: og tilsidst et sluttegn.
  regex_expr <- paste0("([^\\.\\?\\!])",
                       "(\\s+)",
                       "(\\()",
                       "([\\w\\s,]*)",
                       "([\\.\\?\\!]+)",
                       "([\\w\\s\\.\\?\\!,]*)",
                       "(\\))",
                       "([\\.\\?\\!])")

  # printer de dele af strengen som matcher regex expression
  print_string <- regmatches(string, gregexpr(regex_expr, string, perl = TRUE))
  message(print_string)

  # Indsætter punktum
  string <- gsub(regex_expr, "\\1. \\3\\4\\5\\6\\7\\8", string, perl = TRUE)

  return(string)
}

# missing_data ----------------------------------------------------------------
#' @title missing_data
#'
#' @description Funktionens formål er at identificere ord i
#'              \code{data_splitted} som ikke kunne matches i
#'              \code{\link{konstruer_ordbog}}. Ord der ikke kunne matches er
#'              angivet med \code{NA} i \code{indices_data}.
#'
#' @param data_splitted En liste hvor hvert element er en character vektor
#'                      indeholdende en række ord
#' @param indices_data En liste hvor hvert element matcher det tilsvarende
#'                     element i \code{data_splitted} med samme længde.
#'
#' @return En data.frame med de ord, som ikke kunne matches og antallet af
#'         gange de optræder.
#'
missing_data <- function(data_splitted, indices_data) {

  out <- mapply(data_splitted, indices_data, FUN = function(x, y) x[is.na(y)])
  out <- unlist(out)
  out <- table(out)
  out <- as.data.frame(out)

  if(nrow(out) == 0) return()

  names(out) <- c("ord", "antal")

  return(out)
}


# replace_na_with_vector ------------------------------------------------------------------
#' @title replace na med vector
#'
#' @description ...
#'
#' @param indices_data
#' @param data_splitted
#' @param word_vector
#' @param replace_value
#'
#' @return En liste med samme værdier som \code{data_splitted}, hvor NA er
#'         erstattet med værdien \code{replace_value} ... .
#'
replace_na_with_vector <- function(indices_data, data_splitted,
                       word_vector, replace_value) {

  indices_names <- lapply(data_splitted, function(x) (x %in% word_vector))

  out <- mapply(x = indices_data,
                y = indices_names,
                FUN = function(x, y) {
                  x[base::intersect(which(is.na(x)), which(y))] <- replace_value
                  return(x)
                },
                SIMPLIFY = FALSE
  )

  return(out)
}


# add_word_to_dictionary ------------------------------------------------------
#' @title add_word_to_dictionary
#'
#' @description Funktionen tilføjer et nyt ord til ordbogen
#'
#' @param new_word Det nye ord
#' @param word_class Det nye ords ordklasse
#'
#' @return Intet. Men gemmer ordbogen som datasæt i /data
#'
add_word_to_dictionary <- function(new_word = NULL, word_class = NULL) {

  stopifnot(is.character(new_word), is.character(word_class))

  # loader ordbog
  data("dansk_ordbog")

  # tjekker om ordet allerede findes i ordbogen
  if (new_word %in% dansk_ordbog$Ord) stop("Ordet findes allerede i ordbogen!")
  if (!(word_class %in% unique(dansk_ordbog$Klasse))) stop("Ordklassen er ny i ordbogen!")

  # indsætter nyt ord og ordklasse i ordbogen
  dansk_ordbog[nrow(dansk_ordbog)+1, ] <- c(new_word, word_class)

  # sorterer data
  dansk_ordbog <- dplyr::arrange(dansk_ordbog, Ord)

  # gemmer ny ordbog
  usethis::use_data(dansk_ordbog, overwrite = TRUE)

  message(new_word, " er tilføjet ordbogen")
}

# replace_na_with_regex -------------------------------------------------------
#' @title replace_na_with_regex
#'
#' @param match_data
#' @param splitted_data
#' @param regex_expr
#' @param replace_value
#'
#' @return En liste med samme værdier som \code{splitted_data}, hvor NA er
#'         erstattet med værdien \code{replace_value} ... .
#'
replace_na_with_regex <- function(match_data, splitted_data,
                                  regex_expr, replace_value) {

  indices_names <- lapply(splitted_data, function(x) grepl(regex_expr, x, perl = TRUE))

  out <- mapply(x = match_data,
                y = indices_names,
                FUN = function(x, y) {
                  x[base::intersect(which(is.na(x)), which(y))] <- replace_value
                  return(x)
                },
                SIMPLIFY = FALSE
  )

  return(out)
}


# split_sentence_to_word_regex ------------------------------------------------
#' @title split_sentence_to_word_regex
#'
#' @description Funktionen bruges blot til at returnere et regex udtryk som
#'              bruges til at splitte en sætning til enkeltstående ord.
#'              Det gøres flere steder i koden, så for ikke at skulle rette
#'              det flere steder, så laves det som en funktion.
#'
#' @details Forklaring af de forskellige regex blokker:
#'     \itemize{
#'       \item blok 1: Splitter på space, komma, start- og slutparantes, /, ], [
#'       \item blok 2: Splitter på apostrof, betinget på at der enten er et
#'                     mellemrum før, eller et mellemrum efter (fordi nogle ord
#'                     eller navne naturligt slutter med apostrof og de skal
#'                     ikke splittes). Splitter også hvis apostrofen er i starten
#'                     eller i slutningen af sætningen, og i de tilfælde hvor der
#'                     er et komma eller en parentes efterfølgende.
#'       \item blok 3: splitter på komma, betinget på, at der efter et komma
#'                     skal være et space (så et decimaltal som 23,32 ikke bliver
#'                     splittet).
#'     }
#'
#' @return regex udtryk
#'
split_sentence_to_word_regex <- function() {

  regex <- paste0("(?=",
                  "([ )(/\\]\\[])", # blok 1
                  "|",
                  "( '|' |'\\)|',|^'|'$)", # blok 2
                  "|",
                  "(, )", # blok 3
                  ")"
                  )

  return(regex)
}


# kontruer_ordklasse ----------------------------------------------------------
#' @title kontruer ordklasse
#'
#' @description Her identificeres ordklassen til de ord der fremgår af
#'              ordbogen. Nogle ord har flere klasser, da det ikke er
#'              muligt fra gang til gang at tage stilling til den
#'              korrekte ordklasse, så vælges den ordklasse som fremgår flest
#'              gange først grupperet på ordet og derefter grupperet på
#'              ordklassen i hele datasættet. For dem hvor ingen ordklasse er
#'              tilknyttet gives værdien "ingen".
#'              Det konstruerede data gemmes i mappen /data under navnet
#'              "ord_og_klasse".
#'
kontruer_ordklasse <- function() {

  dansk_ordbog$Klasse[dansk_ordbog$Klasse == ""] <- "ingen"

  # Hvis ord indgår flere gange, så vælges ordet med den klasse
  # som fremgår flest gange. Hvis der er 2 eller flere klasser som
  # indgår lige mange gange, så vælges de alle
  dansk_ordbog <- dansk_ordbog %>%
    dplyr::group_by(Ord, Klasse) %>%
    dplyr::mutate(count = dplyr::n()) %>%
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
    dplyr::ungroup() %>%
    data.frame()

  dansk_ordbog <- dansk_ordbog %>% dplyr::select(Ord, Klasse)

  ordbog <- dansk_ordbog %>%
    #dplyr::mutate(Ord = tolower(Ord)) %>% # Vi vil gerne udenom denne del
    dplyr::distinct(Ord, .keep_all = TRUE)

  # Grupperer klasser

  # ordbog$Klasse <- ifelse(stringr::str_detect(ordbog$Klasse, "konj"), "konj",
  #                  ifelse(stringr::str_detect(ordbog$Klasse, "udråbsord"), "udråbsord",
  #                  ifelse(stringr::str_detect(ordbog$Klasse, "lydord"), "",
  #                  ifelse(stringr::str_detect(ordbog$Klasse, "pron. sg. og pl."), "pron",
  #                         ordbog$Klasse))))
  # pron

  # gemmer
  ord_og_klasse <- ordbog

  usethis::use_data(ord_og_klasse, overwrite = TRUE)
  # gøres for at sikre at den opdaterede ord_og_klasse
  # bliver loaded når den kaldes senere
  devtools::load_all()
}



# tjek_data -------------------------------------------------------------------
#' @title tjek_data
#'
#' @description Funktionen tjekker om længden på hvert element i sentence_list
#'              er den samme som det tilsvarende element i comma_index_list.
#'
#' @param sentence_list Liste med sætninger
#' @param comma_index_list Liste med komma index
#'
#' @return Funktionen returnerer intet
#'
tjek_data <- function(sentence_list, comma_index_list) {

  invisible(
    mapply(x = sentence_list, y = comma_index_list,
           FUN = function(x, y) {
             if (length(x) != length(y)) stop("Fejl i data-tjek: ", y, " ", x)
           })
  )
}





# handle_uppercase ------------------------------------------------------------
#' @title handle_uppercase
#'
#' @description
#' Hvis et ord er uppercase, med længde større en 1, ikke fremgår i ordbogen, men
#' fremgår i ordbogen som lowercase, så rettes ordet til lowercase. Hvis ordet
#' ikke fremgår i ordbogen, så tjekkes navneordbogen, hvis ordet heller ikke
#' findes her, så bliver ordet markeret som den indsatte værdi for
#' \code{impute_value}.
#'
#' @note Ideen med at give ukendte uppercase ord sin egen klasse er, at disse
#'       ord har en anden karakteristik end almindelige ukendte ord. Tanken er,
#'       er disse ukendte uppercase ord typisk vil være forkortelser som
#'       eksempelvis GDPR eller FIFA.
#'       Længde større end 1 gør, at "I" ikke forvekles med "i".
#'
#' @detail Problemet med at lave en lowercase på alle ord, er eksempelvis
#'         tilfældet med "i" og "I", som har 2 forskellige betydninger. En anden
#'         problemstilling er navne som også kan være ord. Eksempelvis "hans" og
#'         "Hans". Begge eksempler er dog stadig problemfyldte, hvis de fremgår
#'         som det første ord i en sætning, hvor begyndelsesbogstavet altid er
#'         stort.
#'
#' @param str_list Liste med sætninger hvor hver sætning er splitter op i ord
#' @param ord Ordbog som character vector
#' @param navneordbog Navne som character vektor
#' @param impute_value Værdier som skal imputes hvis uppercase ordet er ukendt
#'
#' @importFrom stringr str_to_title
#'
#' @return \code{str_list} hvor uppercase er håndteret som ovenstående
#'         beskrivelse
#'
handle_uppercase <- function(str_list, ord, navneordbog, impute_value){

  stopifnot(is.list(str_list), is.character(ord), is.character(navneordbog))

  navne <- c(navneordbog, paste0(navneordbog, "s"))

  out <- lapply(str_list,
                function(x) {

                  if(any(grepl("^[[:upper:]]+$", x) & nchar(x) > 1)) {

                    # identificerer position af uppercase ord
                    indi_up <- grepl("^[[:upper:]]+$", x) & nchar(x) > 1

                    # kun hvis lowercase af ordet findes i *ord* og uppercase
                    # versionen ikke findes i *ord* skal det erstattes
                    indi_ord <- !(x[indi_up] %in% ord) & tolower(x[indi_up]) %in% ord

                    indi_navn <- str_to_title(x[indi_up]) %in% navne

                    # tester om et uppercase ord både indgår i *ord* og *navne*
                    # if (any(x[indi_up][indi_ord] %in% x[indi_up][indi_navn])) {
                    #   warning("Ordet ",
                    #           paste(intersect(x[indi_up][indi_ord], x[indi_up][indi_navn]), collapse = ", "),
                    #           " går igen i både ordbog og navneordbog.",
                    #           " Ordet vil indgå som i ordbog")
                    # }

                    # hvis uppercase ord findes i *navne*, erstattes det med str_to_title versionen
                    if(any(indi_navn)) {
                      x[indi_up][indi_navn] <- str_to_title(x[indi_up][indi_navn])
                    }


                    # hvis uppercase ord findes i *ord*, erstattes det med lowercase versionen
                    # dette tjek er efter tjekket med *ord* for at opfylde warning-statement
                    if(any(indi_ord)) {
                      x[indi_up][indi_ord] <- tolower(x[indi_up][indi_ord])
                    }


                    # håndterer første ord i sætningen
                    if(indi_up[[1]] && indi_ord[[1]]) {
                      x[1] <- str_to_title(x[1])
                    }

                    # imputerer værdi for de uppercase tilfælde, som ikke(!) findes i *ord*
                    if(any(grepl("^[[:upper:]]+$", x) & nchar(x) > 1)) {
                      check_upper <- !(x[grepl("^[[:upper:]]+$", x) & nchar(x) > 1] %in% ord)
                      x[grepl("^[[:upper:]]+$", x) & nchar(x) > 1][check_upper] <- impute_value
                    }

                  }
                  return(x)
                }
                )

  return(out)
}

# match_words_in_vocab --------------------------------------------------------
#' @title match_words_in_vocab
#'
#' @description Her matches hvert ord i hvert element i \code{str_list} i
#' \code{ord} og får det tilsvarende index nummer i \code{ord}.
#'
#' @details
#' I både \code{str_list} og \code{ord} findes ord hvor der kan indgå både store
#' og små bogstaver. For at matche så korrekt som muligt, bruges
#' hverken tolower() eller toupper() funktionalitet. Det har dog den
#' ulempe, at det første ord i hver sætning, som altid har stort
#' begyndelsesbogstav, sjældent matches i ordbogen. Derfor bruges
#' tolower() på første ord i hver sætning, og det første ord matches
#' både på det originale ord, med stort begyndelsesbogstav og på
#' tolower udgaven. Efterfølgende vælges det ord, som kunne matches i
#' \code{ord}. Hvis ingen kunne matches returneres *NA* for første ord, hvis
#' de begge kunne matches, så returneres tolower indekset. Det vurderes at det
#' er mest korrekt, selvom der kan være tilfælde, hvor det vil være mest korrekt
#' at returnere indekset for det oprindelige ord (dvs. uden tolower). Årsagen
#' til, at det er nødvendigt at matche det første ord 2 gange er, at et ord som
#' eksempelvis *Hvem* kun findes i tolower-udgaven i \code{ord}, mens et ord som
#' *USA* ikke findes i tolower-udgaven.
#'
#' @param str_list Liste med sætninger hvor hver sætning er splitter op i ord
#' @param ord Ordbog som character vector
#' @param numCores Antal kerner
#'
#' @import parallel
#'
#' @return En liste som \code{str_list}, men i stedet for ord, så indeholdende
#'         index ift. hvor ordet er matchet i \code{ord}.
#'
match_words_in_vocab <- function(str_list, ord, numCores) {

  stopifnot(is.list(str_list), is.character(ord), is.integer(numCores))
  if(numCores == 1) stop("numCores skal være større end 1")

  first_word <- lapply(str_list, function(x) {x[1] <- tolower(x[1])})
  str_list_tmp <- mapply(x = first_word, y = str_list, function(x,y) c(x, y))
  tmp <- split(str_list_tmp, cut(seq_along(str_list_tmp), numCores, labels = FALSE))
  cl <- makePSOCKcluster(detectCores() / numCores)
  clusterExport(cl, varlist = c("ord"), envir = environment())
  tmp <- parLapply(cl, tmp, function(x) lapply(x, function(y) match(y, ord)))
  stopCluster(cl)
  sentence_match <- unlist(tmp, recursive = FALSE)
  names(sentence_match) <- NULL

  sentence_match <- lapply(sentence_match,
                           function(x) {
                             if(!is.na(x[1])) {
                               x <- x[-2]
                             } else if(!is.na(x[2])) {
                               x <- x[-1]
                             } else {
                               x <- x[-2]
                             }
                             return(x)
                           }
  )

  return(sentence_match)
}

#' @title prepare_model_data
#'
#' @description Funktionaliteten splitter data i train, val og test
#'
#' @param word_data ...
#' @param word_class_data ...
#' @param comma_list_index ...
#' @param comma_list_number ...
#'
#' @return En liste hvor data er splittet i train, val og test
#'
prepare_model_data <- function(word_data, word_class_data,
                               comma_list_index, comma_list_number
                               ) {

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
  #usethis::use_data(word_vocabulary, overwrite = TRUE)

  # word class vocabulary (created on training data)
  word_classes_vocabulary <- unique(unlist(word_class_data))
  #usethis::use_data(word_classes_vocabulary, overwrite = TRUE)

  # padding length
  pad_len <- do.call(max, lapply(word_data[idx == 1], function(x) length(x)))
  print(pad_len)

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

  ## Gemmer train data og val data som csv fil ----
  message("Gemmer train data og val data som csv-fil")
  # train
  write.csv(train_word, row.names = FALSE,
            file = "./dev/cloud_data/train_word.csv")
  write.csv(train_word_class, row.names = FALSE,
            file = "./dev/cloud_data/train_word_class.csv")

  # val
  write.csv(val_word, row.names = FALSE,
            file = "./dev/cloud_data/val_word.csv")
  write.csv(val_word_class, row.names = FALSE,
            file = "./dev/cloud_data/val_word_class.csv")

  # response
  write.csv(index_target_train, row.names = FALSE,
            file = "./dev/cloud_data/index_target_train.csv")
  write.csv(index_target_val, row.names = FALSE,
            file = "./dev/cloud_data/index_target_val.csv")


  write.csv(num_target_train, row.names = FALSE,
            file = "./dev/cloud_data/num_target_train.csv")
  write.csv(num_target_val, row.names = FALSE,
            file = "./dev/cloud_data/num_target_val.csv")

  ## Returnerer test data
  message("Returnerer test data")
  return(list(test_word = test_word, test_word_class = test_word_class,
              index_target_test = index_target_test, num_target_test = num_target_test))
}

# get_model_path --------------------------------------------------------------
#' @title get_model_path
#'
#' @description Funktionen identificerer den gældende sti med modelobjekt når
#'              der trænes en ny model via google cloud.
#'
#' @return Sti til nyeste model
#'
get_model_path <- function() {

  # identificerer folders i "runs"
  runs <- list.files("./runs")

  # identificerer folder der matcher dagsdato
  idx <- substr(runs, 9, 18) %>% gsub("_", "/", .) %>% as.Date() %in% Sys.Date()

  if(!any(idx)) stop("Ingen folders ser ud til at matche dagsdato")

  current_run_path <- runs[idx]

  # Hvis der er flere folders på samme dag, identificeres den nyeste
  if (length(current_run_path) > 1) {

    time <- lapply(current_run_path, function(x) file.info(paste0("./runs/", x))$ctime)
    current_run_path <- current_run_path[which.max(time)]

  }

  return(current_run_path)
}

