### renser deadline ###

### DATABEHANDLING ###

### Henter data ----
library(dplyr)

data("maridt")

data <- maridt

### Renser data ----
output <- remove_additional_whitespaces(data)
output <- swap_to_capital(output, c("[?!:]|[\\.\\!\\?:]{2,}"))
output <- sentence_in_parentheses(output)
#output <- gsub_custom(output, NULL)
output <- split_str_to_dataframe(output, abbreviation = NULL) # TODO: hvad med ' ???
output <- cbind.data.frame(output, split_end_punctuation(output$data))
output$sentences <- trimws(output$sentences)
output <- extract_comma(output, label = "sentences")

### tjek data ----
# tjek for stoptegn i sætning (det må godt være i forkortelser)
output$sentences[grepl("[.:?!,;]", output$sentences)]

# tjek om det findes sætninger som ikke starter med stort begyndelsesbogstav
output$sentences[!substr(output$sentences, 1, 1) %in%
                   c(LETTERS, "Æ", "Ø", "Å", seq(0, 9, by = 1))]

# tjek om der findes sætninger med start parentes ( eller [ og ingen slutparentes

# tjek at alle strenge slutter på bogstav eller tal
output$sentences[!substr(output$sentences,
                         nchar(output$sentences),
                         nchar(output$sentences)) %in%
                   c(letters, "æ", "ø", "å", seq(0, 9, by = 1))]

# tjek sætninger der starter på tal og sætningen efter
for (i in 1:length(output$sentences)) {
  if(substr(output$sentences[i],
            nchar(output$sentences[i]), nchar(output$sentences[i])) %in%
     seq(0, 9, by = 1)) {
    cat(paste(output$sentences[i-1],
              output$sentences[i],
              output$sentences[i+1],
              sep = ". "
              ,"\n")
    )
  }
}

# inspicerer tilfældige sætninger
sample(output$sentences, 10)


### transformerer features ----

# laver feature der tæller antallet af ord i hver sætning
output$antal_ord <- stringr::str_count(output$sentences, "\\S+")
output$antal_ord_norm <- output$antal_ord / max(output$antal_ord)

# tolower (bør nok ikke gøres? - eller hvad??)
output$sentences <- tolower(output$sentences)

# gemmer data
maridt_clean <- output
usethis::use_data(maridt_clean, overwrite = TRUE)

