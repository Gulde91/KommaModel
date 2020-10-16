library(rjson)
library(dplyr)

# data <- fromJSON(file = "~/model_log.json")
data_ny <- fromJSON(file = "~/ml_job_job_id_cloudml_2020_08_12_164316872__logs__2020-08-12T07-02.json")

# data_list <- lapply(data, `[`, "jsonPayload")
# data_list <- lapply(data, `[`, "message")

# df <- lapply(data, function(x) data.frame(x, stringsAsFactors = FALSE)) %>%
#       dplyr::bind_rows()

df_ny <- lapply(data_ny, function(x) data.frame(x, stringsAsFactors = FALSE)) %>%
  dplyr::bind_rows()


library(jsonlite)

data <- jsonlite::fromJSON("~/model_log.json")


