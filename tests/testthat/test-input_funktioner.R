context("Input funktioner")

test_that("Validerer identify_abbreviation()", {

  input_streng <-
  c("Denne streng skal teste bl.a. teste forkortelser i data.
  En fork. er kendetegnet ved, at der efter et punktum eller et andet
  stoptegn, efterfølgende er et lille begyndelsesbogstav.
  Hvis mr. # afsnit er lavet med vilje
  Graham indgår i data, så bliver han ikke identificeret!")

  output <- identify_abbreviation(input_streng)

  val_output <- list()
  val_output[[1]] <- c(" teste bl.a. teste fo")
  val_output[[2]] <- c("este bl.a. teste fork")
  val_output[[3]] <- c("  En fork. er kendete")

  expect_equal(output, val_output)

})

test_that("Validerer split_str_to_dataframe()", {

# input
string <- c(
"Her testes fx. forkortelser, men spm. er, om det f.eks. kan lade sig gøre.
Hvis mr. Graham indgår, har vi så et problem?
Når sætningen slutter med 1.
Eller indeholder en dato, som d. 12, så går det ikke galt.
Heller ikke hvis sætningen slutter med en f.ortelse?
Nr. skaber heller ikke problemer, mere.
Heller ikke, hvis den slutter medNr.
Så skal sætningen stadig splittes.
Hvis altså den rigtige forkortelse - her d. - er med.
Øvelsen er, at sætningen skal kunne starte med ÆØÅ.
1 sætning kan sagtens starte med et tal.
Kan andre stoptegn bruges?? Ja, det kan de..!!
Hvis 2 sætninger ikke er opdelt med et whitespace.Så bliver de ikke splittet.
Hvis en tusindtalsseparator bruges, fx. 500.000, så skal det ikke splittes!
For 1.000 skal ikke splittes i 2.
Hvis en sætning ender! (og der så kommer en parentes, så regnes parentes sætningen som
en selvstændig sætning, betinget på, at der efter parentesen er et sluttegn).
Denne sætning skal også splittes. 'Viborg'.
Denne sætning vil også blive splittet. 'viborg'.
Ligeledes vil denne blive splittet! -på grund af bindestreg.")

# TODO: hvis en forkortelse indgår i slutningen af en sætning, således at
# sætningen skal splittes. hvordan skal det så håndteres?

abbreviation <- c("mr.", "d.", "fx.", "f.ortelse", "Nr.")

# output
output <- split_str_to_dataframe(string, abbreviation)

# validering
val <- data.frame(data = c(
"Her testes fx. forkortelser, men spm. er, om det f.eks. kan lade sig gøre.",
"Hvis mr. Graham indgår, har vi så et problem?",
"Når sætningen slutter med 1.",
"Eller indeholder en dato, som d. 12, så går det ikke galt.",
"Heller ikke hvis sætningen slutter med en f.ortelse?",
"Nr. skaber heller ikke problemer, mere.",
"Heller ikke, hvis den slutter medNr.",
"Så skal sætningen stadig splittes.",
"Hvis altså den rigtige forkortelse - her d. - er med.",
"Øvelsen er, at sætningen skal kunne starte med ÆØÅ.",
"1 sætning kan sagtens starte med et tal.",
"Kan andre stoptegn bruges??",
"Ja, det kan de..!!",
"Hvis 2 sætninger ikke er opdelt med et whitespace.Så bliver de ikke splittet.",
"Hvis en tusindtalsseparator bruges, fx. 500.000, så skal det ikke splittes!",
"For 1.000 skal ikke splittes i 2.",
"Hvis en sætning ender!",
"(og der så kommer en parentes, så regnes parentes sætningen som en selvstændig sætning, betinget på, at der efter parentesen er et sluttegn).",
"Denne sætning skal også splittes.",
"'Viborg'.",
"Denne sætning vil også blive splittet.",
"'viborg'.",
"Ligeledes vil denne blive splittet!",
"-på grund af bindestreg."
), stringsAsFactors = FALSE)

expect_equal(output, val)
})

test_that("Validerer swap_to_capital()", {

  # input
  streng <- c(
  "Kan funktionen omkode en streng der ender på et stoptegn
   efterfulgt af et lille begyndelsesbogstav? hvis ikke, så
   virker funktionen ikke! hvis der bl.a. indgår en forkortelse,
   så skal der ikke gives stort startbogstav..
   måske kan den? i så fald virker funktionen! Hvilket er godt!!!
   åh nej: hvad hvis der startes med æ, ø eller å?eller hvis der
   intet mellemrum er mellem 2 sætninger? hvad hvis stoptegnet
   har mere end længden 1..!? eller hvis ? indgår i midten af
   sætningen.. hvad hvis ..?!? indgår i midten af en sætning ..!?
   men det vil være en mærkelig sætning!")

  # output
  output <- swap_to_capital(streng, match_symbol = c("[?!:]|[\\!\\?\\.:]{2,}"))
  # noter: Punktum indgår ikke som betingelse, medmindre at der er mindst
  #        2 punktummer efter hinanden

  # validering
  val <- c(
   "Kan funktionen omkode en streng der ender på et stoptegn
   efterfulgt af et lille begyndelsesbogstav? Hvis ikke, så
   virker funktionen ikke! Hvis der bl.a. indgår en forkortelse,
   så skal der ikke gives stort startbogstav..
   Måske kan den? I så fald virker funktionen! Hvilket er godt!!!
   Åh nej: Hvad hvis der startes med æ, ø eller å?eller hvis der
   intet mellemrum er mellem 2 sætninger? Hvad hvis stoptegnet
   har mere end længden 1..!? Eller hvis ? indgår i midten af
   sætningen.. Hvad hvis ..?!? Indgår i midten af en sætning ..!?
   Men det vil være en mærkelig sætning!")
  # NB! der skal være mindst et whitespace efter skilletegnet!

  expect_equal(output, val)

})

test_that("Validerer gsub_custom()", {

  # input
  streng <- c("Hvis der i strengen indgår mærkelige tegn som ... eller ...
               måske... eller..
               så skal det udskiftes!.? ikke !!!?
               Derfor; denne; funktion!!")

  replace <- c("\\s[\\.\\?\\!]{2,}(\\s|$)" = "\\. ", #\\s -> mellemrum, ny linje, tab
               "(\\S)([\\.\\?\\!]{2,}(\\s|$))" = "\\1\\. ",
               ";" = ",")

  # output
  output <- gsub_custom(streng, replace)
  output <- remove_additional_whitespaces(output)

  # val
  val <- c("Hvis der i strengen indgår mærkelige tegn som. eller.
            måske. eller.
            så skal det udskiftes. ikke.
            Derfor, denne, funktion. ")
  val <- remove_additional_whitespaces(val)

  expect_equal(output, val)

})

test_that("Validerer remove_additional_whitespaces()", {

  # input
  streng <- c("  Her  testes    om funktionen kan  fjerne
              overflødige white-spaces
              og linjeskift.  ")

  # output
  output <- remove_additional_whitespaces(streng)

  # val
  val <- c("Her testes om funktionen kan fjerne overflødige white-spaces og linjeskift.")

  expect_equal(output, val)

})

test_that("Validerer split_end_punctuation()", {

  # input
  streng <- c("Her skal det testes...!",
              "Om sætningerne kan opsplittes?",
              "Mellem tekst sluttegn ?!?!",
              "Det skulle gerne virke.")
  # output
  output <- split_end_punctuation(streng)

  # val
  val <- data.frame(sentences = c("Her skal det testes",
                                  "Om sætningerne kan opsplittes",
                                  "Mellem tekst sluttegn ",
                                  "Det skulle gerne virke"),
                    end_punctuation = c("...!", "?", "?!?!", "."),
                    stringsAsFactors = FALSE)

  expect_equal(output, val)

})

test_that("Validerer extract_comma()", {

  # input
  test <- data.frame(sentences = c("her testes om kommaer, bliver fjernet.",
                                   "og her tælles,,, antallet.",
                                   "for at se, om funktionen, virker.",
                                   "det gør den forhåbentlig",
                                   "også, hvis, der, er, flere, kommaer.",
                                   "selv; med; semikolen, bør, det, virke.",
                                   "hvis,der,ikke,er,space,virker,det,ikke",
                                   "decimal, som 23,42, er den primære årsag.",
                                   "et komma (efter en parentes), skal kunne håndteres"),
                     stringsAsFactors = FALSE)

  # output
  output <- extract_comma(test, "sentences")

  # val
  val <- data.frame(sentences = c("her testes om kommaer bliver fjernet.",
                                  "og her tælles,, antallet.",
                                  "for at se om funktionen virker.",
                                  "det gør den forhåbentlig",
                                  "også hvis der er flere kommaer.",
                                  "selv med semikolen bør det virke.",
                                  "hvis,der,ikke,er,space,virker,det,ikke",
                                  "decimal som 23,42 er den primære årsag.",
                                  "et komma (efter en parentes) skal kunne håndteres"),
                    antal_kommaer = c(1, 1, 2, 0, 5, 5, 0, 2, 1),
                    stringsAsFactors = FALSE)
  val$komma_ord_index <- list(c(0, 0, 0, 1, 0, 0), c(0, 0, 1, 0),
                              c(0, 0, 1, 0, 1, 0), c(0, 0, 0, 0),
                              c(1, 1, 1, 1, 1, 0), c(1, 1, 1, 1, 1, 0),
                              c(0), c(1, 0, 1, 0, 0, 0, 0),
                              c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
                              )

  expect_equal(output, val)

})

# test_that("Validerer map_sentence_to_dictionary_index()", {
#
#   # input
#   sentence <- c("jeg ved en lærkerede",
#                 "jeg siger ikke mere",
#                 "den findes på en (hede)",
#                 "et sted som ingen ser")
#
#   ordbog <- c("jeg", "ved", "en", "ikke", "mere",
#               "den", "findes", "på", "hede", "(")
#   len <- length(ordbog) + 1
#
#   # output
#   output <- map_sentence_to_dictionary_index(sentence, "(?=[ )(])", ordbog, len)
#
#   # val
#   val <- list(c(1, 2, 3, len),
#               c(1, len, 4, 5),
#               c(6, 7, 8, 3, 10, 9, 11),
#               c(len, len, len, len, len))
#
#   expect_equal(output, val)
# })

test_that("Validerer map_sentence_to_word_class()", {

  # input
  sentence <- c("jeg ved en lærkerede",
                "jeg siger ikke mere",
                "den findes på en (hede)",
                "et sted som ingen ser")

  ordbog <- data.frame(Ord = c("jeg", "ved", "en", "ikke", "mere",
                               "den", "findes", "på", "hede", "("),
                       Klasse = c("sb.", "sb.", "pron.", "adv.", "ubøj. adj.",
                                  "pron.", "vb.", "præp.,adv.", "adj.", ""),
                       stringsAsFactors = FALSE
                       )
  mis <- "ukendt"

  # output
  output <- map_sentence_to_word_class(sentence, ordbog, mis)

  # val
  val <- list(c("sb.", "sb.", "pron.", "ukendt"),
              c("sb.", "ukendt", "adv.", "ubøj. adj."),
              c("pron.", "vb.", "præp.,adv.", "pron.", "", "adj.", "ukendt"),
              c("ukendt", "ukendt", "ukendt", "ukendt", "ukendt")
              )

  val_missing <- data.frame(ord = c(")", "et", "ingen", "lærkerede",
                                    "ser", "siger", "som", "sted"),
                            antal = 1)

  expect_equal(output$mapped_sentences, val)
  expect_equal(output$data_missing, val_missing)
})

test_that("Validerer fill_3d_array()", {

  # input
  sentence_list <- list(c("sb.", "pron.", "vb.", "sb."),
                        c("sb.", "ukendt", "konj.", "sb.", "sb.", "sb."))

  max_len <- do.call(max, lapply(sentence_list, length)) + 2

  ord_klasser <- c("sb.", "pron.", "vb.", "konj.", "ukendt", "test", " ")

  # output
  output <- fill_3d_array(sentence_list, max_len, ord_klasser)


  # val
  output_1 <- matrix(data = c(1, 0, 0, 0, 0, 0, 0,
                              0, 1, 0, 0, 0, 0, 0,
                              0, 0, 1, 0, 0, 0, 0,
                              1, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0),
                     ncol = length(ord_klasser), nrow = max_len, byrow = TRUE)

  output_2 <- matrix(data = c(1, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 1, 0, 0,
                              0, 0, 0, 1, 0, 0, 0,
                              1, 0, 0, 0, 0, 0, 0,
                              1, 0, 0, 0, 0, 0, 0,
                              1, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0),
                     ncol = length(ord_klasser), nrow = max_len, byrow = TRUE)


  expect_equal(output_1, output[1,,])
  expect_equal(output_2, output[2,,])

})

test_that("Validerer sentence_in_parentheses()", {

  # input
  input <- c("Testsætning_start. Sætning før parentes (flere sætninger.
               I, denne, parentes. Hvordan går det?).
               Testsætning slut",
              "blaaa (bla bla bla? Bla bla bla).",
              "blaaa (bla bla).",
              "blaaa (bla bla) og funktionen fortsætter i det
               uendeligeeeeeeeeeeeeeeeee. (ny parentes).",
              "Testsætning_start. Sætning før parentes (kun 1 sætning. I denne). Testsætning slut.
               Ny sætning (denne. gang. med. flere. sætninger.)."
              )
  # output
  output <- input %>% lapply(sentence_in_parentheses) %>% unlist()

  # val
  val <- c("Testsætning_start. Sætning før parentes. (flere sætninger.
               I, denne, parentes. Hvordan går det?).
               Testsætning slut",
           "blaaa. (bla bla bla? Bla bla bla).",
           "blaaa (bla bla).",
           "blaaa (bla bla) og funktionen fortsætter i det
               uendeligeeeeeeeeeeeeeeeee. (ny parentes).",
           "Testsætning_start. Sætning før parentes. (kun 1 sætning. I denne). Testsætning slut.
               Ny sætning. (denne. gang. med. flere. sætninger.).")

  val <- lapply(val, remove_additional_whitespaces) %>% unlist()

  testthat::expect_equal(output, val)
})

test_that("Validerer missing_data", {

  # input
  splitted_data <- list(c("A", "B", "C"),
                        c("A", "DC", "D"),
                        c("AA", "AA", "AB"))

  data_indices <- list(c(1, 2, 3),
                       c(1, NA, 4),
                       c(NA, NA, NA))

  # output
  output <- missing_data(splitted_data, data_indices)

  # val
  val <- data.frame(ord = c("AB", "DC", "AA"),
                    antal = c(1, 1, 2))

  testthat::expect_equal(output, val)
})

test_that("Validerer replace_na_with_vector()", {

  # input
  splitted_data <- list(c("a", "c"),
                        c("a", "bo", "b"),
                        c("anita", "alex", "sted"))

  data_indices <- list(c(1, 2, 3),
                       c(1, NA, 4),
                       c(NA, NA, NA))

  word_vector <- c("anita", "alex", "bo")

  # output
  output <- replace_na_with_vector(data_indices, splitted_data, word_vector, -1)

  # val
  val <- list(c(1, 2, 3),
              c(1, -1, 4),
              c(-1, -1, NA))

  testthat::expect_equal(output, val)
})

test_that("Validerer replace_na_with_regex()", {

  # input
  splitted_data <- list(c("et", "tal", "3", "01", "12.32", "7.000.000", "8.", ".8", "-10.00"),
                        c("her", "er", "ingen", "tal"),
                        c("432", "321,32", "32.23", "34:12", "12", "elleve"))

  match_data <- list(c("et", "tal", NA, NA, NA, NA, NA, NA, NA),
                     c("her", "er", "ingen", "tal"),
                     c(NA, NA, NA, NA, "12", "elleve"))

  # output
  output <- replace_na_with_regex(match_data, splitted_data,
                                  "^-*[0-9]*[\\.|:|,|\\d]*[0-9]+[\\.]*$",
                                  "tal_")

  # val
  val <- list(c("et", "tal", "tal_", "tal_", "tal_", "tal_", "tal_", "tal_", "tal_"),
              c("her", "er", "ingen", "tal"),
              c("tal_", "tal_", "tal_", "tal_", "12", "elleve"))

  testthat::expect_equal(output, val)
})

test_that("Validerer split_sentence_to_word_regex()", {

  # input
   input <- c("Denne sætning er helt simpel.",
             "Hvis komma indgår, skal det splittes.",
             "Her indgår (parentes)",
             "her testes med (komma), efter parentes",
             "hvis,der,ikke,er,space,virker,det,ikke",
             "her testes med decimaltal 32,23",
             "her testes 'med' plinger",
             "'her testes med plinger'",
             "her indgår pling'en inde i ordet",
             "flere kommaer i træk,,, vil kun splitte på den sidste",
             "her ('testes'), med 'plinger', og parentes.",
             "Han/din/den skal splittes"
             )

  # output
  regex <- split_sentence_to_word_regex()
  output <- strsplit(input, regex, perl = TRUE)
  output <- lapply(output, function(x) x[x != " "])

  # val
  val <- list(c("Denne", "sætning", "er", "helt", "simpel."),
              c("Hvis", "komma", "indgår", ",", "skal", "det", "splittes."),
              c("Her", "indgår", "(", "parentes", ")"),
              c("her", "testes", "med", "(", "komma", ")", ",", "efter", "parentes"),
              c("hvis,der,ikke,er,space,virker,det,ikke"),
              c("her", "testes", "med", "decimaltal", "32,23"),
              c("her", "testes", "'", "med", "'", "plinger"),
              c("'", "her", "testes", "med", "plinger", "'"),
              c("her", "indgår", "pling'en", "inde", "i", "ordet"),
              c("flere", "kommaer", "i", "træk,,", ",", "vil", "kun", "splitte", "på", "den", "sidste"),
              c("her", "(", "'", "testes", "'", ")", ",", "med", "'", "plinger", "'", ",", "og", "parentes."),
              c("Han", "/", "din", "/", "den", "skal", "splittes")
              )

  testthat::expect_equal(output, val)
})

test_that("Validerer handle_uppercase()", {

  # input
  test_data <- list(c("Her", "I", "DENNE", "sætning", "er", "både",
                      "UPPERCASE", "og", "lowercase"),
                    c("navne", "som", "ANDERS", "Andreas", "og", "sigurd",
                      "skal", "håndteres"),
                    c("Navne", "med", "s", "som", "ALEXANDERS", "eller", "ALEXANDER'S"),
                    c("UPPERCASE", "I", "ALLE", "ORD", "MED", "TINE"),
                    c("USA", "findes", "i", "ordbogen", "og", "USA"),
                    c("Her", "R", "ingen", "uppercase"),
                    c("Her", "findes", "TO", "UPPERCASE", "ord"))
  # NB: ALEXANDER'S hånderes ikke at regex udtrykker ^[[:upper:]]+$ fordi der indgår apostrof
  # alternativet er at tjekke om første og sidste bogstav i et ord er uppercase

  tmp_ordbog <- c("denne", "skal", "i", "I", "alle", "ord", "med", "USA", "bo")
  tmp_navneordbog <- c("Anders", "Alexander", "Tine", "Bo")

  out <- handle_uppercase(test_data, tmp_ordbog, tmp_navneordbog, "ukendt_uppercase")

  # output
  val <- list(c("Her", "I", "denne", "sætning", "er", "både",
                "ukendt_uppercase", "og", "lowercase"),
              c("navne", "som", "Anders", "Andreas", "og", "sigurd", "skal", "håndteres"),
              c("Navne", "med", "s", "som", "Alexanders", "eller", "ALEXANDER'S"),
              c("ukendt_uppercase", "I", "alle", "ord", "med", "Tine"),
              c("USA", "findes", "i", "ordbogen", "og", "USA"),
              c("Her", "R", "ingen", "uppercase"),
              c("Her", "findes", "ukendt_uppercase", "ukendt_uppercase", "ord")
             )

  testthat::expect_equal(out, val)

  # test 2
  test_data2 <- list(c("Her", "med", "ALLE", "BO")) # BO indgår både i ord og navne
  expect_warning(handle_uppercase(test_data2, tmp_ordbog, tmp_navneordbog, "ukendt_uppercase"))
  expect_equal(
    suppressWarnings(
      handle_uppercase(test_data2, tmp_ordbog, tmp_navneordbog, "ukendt_uppercase")
      ),
    list(c("Her", "med", "alle", "Bo"))
    )

})

test_that("Validerer match_words_in_vocab()", {

  # input
  test_data <- list(c("Her", "tester", "vi"),
                    c("USA", "findes"),
                    c("Det", "gør", "¤#!", "ikke"),
                    c("987", "findes", "ikke"))

  ord <- c("her", "tester", "vi",
           "USA", "findes",
           "det", "gør", "ikke")

  out <- match_words_in_vocab(test_data, ord, 2L)

  # output
  val <- list(c(1, 2, 3),
              c(4, 5),
              c(6, 7, NA, 8),
              c(NA, 5, 8))

  testthat::expect_equal(out, val)
  testthat::expect_error(match_words_in_vocab(test_data, ord, 1L))

})
