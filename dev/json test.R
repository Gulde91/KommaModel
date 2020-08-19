library(rjson)

data <- fromJSON(file = "~/model_log.json")

data_list <- lapply(data, `[`, "jsonPayload")
data_list <- lapply(data, `[`, "message")

df <- dplyr::bind_rows(data_list)


library(jsonlite)

data <- jsonlite::fromJSON("~/model_log.json")


