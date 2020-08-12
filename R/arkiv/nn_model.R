### Her fittes model ###
library(dplyr)

tmp <- function() {
### data hentes ----
data(list = c("skyggeforbandelsen_clean",
              "bunker_137_clean",
              "naomi_og_anders_clean",
              "krystalnat_clean",
              "anitas_millioner_clean",
              "zetland_1_clean"))

data <- rbind(bunker_137_clean, skyggeforbandelsen_clean,
              naomi_og_anders_clean, krystalnat_clean,
              anitas_millioner_clean, zetland_1_clean)
rm(list=setdiff(ls(), "data"))

### konstruerer ordbog ----
data(list = c("skyggeforbandelsen", "bunker_137", "naomi_og_anders",
              "krystalnat", "anitas_millioner", "zetland_1"))
ordbog <- paste(skyggeforbandelsen, bunker_137, naomi_og_anders,
                krystalnat, anitas_millioner, zetland_1)
rm(skyggeforbandelsen, bunker_137, naomi_og_anders,
   krystalnat, anitas_millioner, zetland_1)

ordbog <- strsplit(ordbog, "(?=[ .,?!:;\n])", perl = TRUE) %>%
  unlist() %>%
  as.data.frame()

names(ordbog) <- "ord"

ordbog <- filter(ordbog, !(ord %in% c(" ", "\n", ",")))
ordbog$ord <- trimws(ordbog$ord, which = "both")

ordbog$ord <- tolower(ordbog$ord)
ordbog <- ordbog %>%
  group_by(ord) %>%
  summarise(antal = n()) %>%
  arrange(desc(antal))

# tager de n med almindelige ord
ordbog <- ordbog[1:500, "ord"]
ordbog <- arrange(ordbog, ord)

### Mapper data til ordbog ----
# Mapper ord til indices i ordbog
data_indices <- strsplit(data$sentences, " ")
data_indices <- lapply(data_indices, function(x) match(x, ordbog$ord))
head(data_indices)

# replacer NA's
data_indices <- lapply(data_indices,
                       function(x) {x[is.na(x)] <- nrow(ordbog)+1; return(x)})
head(data_indices)

# Mapper til binær matrice
data_indices <- vectorize_sequences(sequence = data_indices,
                                    dimension = nrow(ordbog) + 1)
#data_indices <- cbind(data_indices, data$antal_ord_norm)

# bør måske gøres på træningsdata
#data_indices <- cbind(data_indices, data$antal_ord / max(data$antal_ord))

### opdeler i train, test og val sæt ----
set.seed(679)
idx <- sample(1:3, size = nrow(data_indices), replace = TRUE, prob = c(0.6, 0.2, 0.2))

train_data <- data_indices[idx == 1,]
test_data <- data_indices[idx == 2,]
val_data <- data_indices[idx == 3,]

train_label <- data$antal_komma[idx == 1]
test_label <- data$antal_komma[idx == 2]
val_label <- data$antal_komma[idx == 3]

# one hot encoding
#dimension <- max(data$antal_komma)
#train_label_one_hot <- to_one_hot(train_label, dimension = dimension + 1)
#test_label_one_hot <- to_one_hot(test_label, dimension = dimension + 1)
#val_label_one_hot <- to_one_hot(val_label, dimension = dimension + 1)

### fitter model ----
library(keras)

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = 501) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 8, activation = "relu") %>%
  layer_dense(units = 1, activation = "relu") # activation = "softmax"

model %>% compile(
  optimizer = "adam", # rmsprop
  loss = "mse", # categorical_crossentropy
  metric = c("mae") # accuracy
)

model %>% fit(
  train_data,
  train_label, # train_label_one_hot
  epochs = 10,
  batch_size = 128,
  validation_data = list(val_data, val_label) # val_label_one_hot
)

results <- model %>% evaluate(test_data, test_label) # test_label_one_hot
results

# baseline
mean(rep(length(which(test_label == sample(test_label))) / length(test_label), 100))
prop.table(table(test_label))

pred <- model %>% predict(test_data) %>% round(digits = 4)
pred <- cbind.data.frame(pred, pred_round = round(pred), test_label)

length(which(pred$pred_round == pred$test_label)) / nrow(pred)

# tilfældig sætning
test <- c("jeg er hjemme i det hus jeg bor i")
#nword <- length(strsplit(test, " ")[[1]]) / max(data$antal_ord)
test <- strsplit(test, " ")
test <- lapply(test, function(x) match(x, ordbog$ord))
test <- lapply(test, function(x) x[!is.na(x)])
test <- vectorize_sequences(test, 501)
#test <- cbind(test, nword)
model %>% predict(test)

}
