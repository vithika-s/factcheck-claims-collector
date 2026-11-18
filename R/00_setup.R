#### Setup (run once) ####

install.packages(c(
  "httr", "jsonlite", "rvest", "xml2", "dplyr", "stringr",
  "purrr", "lubridate", "readr", "tibble"
))

# Google Fact Check Tools API key (needed for 01_google_factcheck_api.R only).
# Create a key in the Google Cloud Console with the Fact Check Tools API enabled,
# then store it outside the scripts, for example in ~/.Renviron:
#   GOOGLE_FACTCHECK_API_KEY=your-key
# and restart R. Check that it is available:

nchar(Sys.getenv("GOOGLE_FACTCHECK_API_KEY")) > 0
