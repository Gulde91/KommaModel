### renser bunker 137 ###

### DATABEHANDLING ###

### Henter data ----
library(dplyr)

data("bunker_137")

data <- bunker_137

### Renser data ----
output <- remove_additional_whitespaces(data)
output <- swap_to_capital(output, c("[?!:]|[\\.\\!\\?:]{2,}"))
output <- gsub_custom(output, c("#" = "", "´" = "", ";" = ","))
output <- split_str_to_dataframe(output,
                                 abbreviation = c("mr.", "d.", "stk.",
                                                  "9."))
output <- cbind.data.frame(output, split_end_punctuation(output$data))
output$sentences <- trimws(output$sentences)
output <- extract_comma(output, label = "sentences")

### tjek data ----
# tjek for stoptegn i sætning (det må godt være i forkortelser)
output$sentences[grepl("[.:?!,;]", output$sentences)]

# tjek om det findes sætninger som ikke starter med stort begyndelsesbogstav
output$sentences[!substr(output$sentences, 1, 1) %in%
                  c(LETTERS, "Æ", "Ø", "Å", seq(0, 9, by = 1))]

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
bunker_137_clean <- output
usethis::use_data(bunker_137_clean, overwrite = TRUE)

