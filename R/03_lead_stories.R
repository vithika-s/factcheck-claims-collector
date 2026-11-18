#### Extracting fact-check articles from Lead Stories ####

# sitemap.txt -> article list -> one row per article
# (NewsArticle and ClaimReview JSON-LD + article text) in resumable batches
# -> rerun of failed articles -> topic filter -> date filter.

####Settings (change here only)####

OUTPUT_DIR    <- "LeadStories_Output"
SITEMAP_URL   <- "https://leadstories.com/sitemap.txt"
BATCH_SIZE    <- 500
DELAY_SECONDS <- 1                      # pause between articles

# Publication-date windows to keep (any number of start/end pairs)
date_windows <- list(
  c("2020-01-01", "2021-12-31"),
  c("2024-01-01", "2025-12-31")
)

# Topic filter applied to title + excerpt + claim + article text (demo topic: vaccines)
topic_pattern <- "vaccin|booster|mrna|immuni|myocarditis|pfizer|moderna"

####Packages####

# install.packages(c("rvest","xml2","dplyr","stringr","purrr",
#                    "lubridate","readr","tibble","jsonlite"))

library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(readr)
library(tibble)
library(jsonlite)

dir.create(OUTPUT_DIR, showWarnings = FALSE)

####Step 1: Read the sitemap and build the archive table####

leadstories_urls <-
  read_lines(
    SITEMAP_URL
  )

length(leadstories_urls)
sum(duplicated(leadstories_urls))

leadstories_archive <-
  tibble(
    url = leadstories_urls
  ) |>

  mutate(

    # month of publication from the URL (/YYYY/MM/)
    date =
      str_extract(
        url,
        "20\\d{2}/\\d{2}"
      ) |>
      str_replace("/", "-") |>
      paste0("-01") |>
      ymd(),

    title =
      url |>
      basename() |>
      str_remove("\\.html$") |>
      str_replace_all("-", " ") |>
      str_squish()

  )

#QC
summary(leadstories_archive)
colSums(is.na(leadstories_archive))
range(leadstories_archive$date, na.rm = TRUE)

# Keep only articles whose URL month falls inside the date windows,
# so pages outside the study period are never requested
url_month_in_windows <- function(d){

  keep <- rep(FALSE, length(d))

  for(w in date_windows){

    keep <- keep | (d >= floor_date(as.Date(w[1]), "month") & d <= as.Date(w[2]))

  }

  keep & !is.na(d)

}

leadstories_archive <-
  leadstories_archive |>
  filter(url_month_in_windows(date))

nrow(leadstories_archive)

#===========================================================
# Helper Functions
#===========================================================

read_page <- function(url){

  read_html(url)

}

safe_text <- function(node){

  if(length(node) == 0)
    return(NA_character_)

  txt <- html_text2(node)

  if(txt == "")
    return(NA_character_)

  txt

}

safe_json <- function(x){

  if(is.null(x))
    return(NA_character_)

  if(length(x) == 0)
    return(NA_character_)

  as.character(x)

}

safe_list <- function(x){

  if(is.null(x))
    return(NA_character_)

  if(length(x) == 0)
    return(NA_character_)

  paste(
    unlist(x),
    collapse = "; "
  )

}

count_words <- function(text){

  if(is.na(text))
    return(NA_integer_)

  stringr::str_count(
    stringr::str_squish(text),
    "\\S+"
  )

}

#===========================================================
# Function to scrape one article
#===========================================================

scrape_leadstories <- function(article){

  page <- read_page(article$url)

  # ---------- JSON-LD (1 = NewsArticle, 2 = ClaimReview) ----------

  scripts <- html_elements(
    page,
    "script[type='application/ld+json']"
  )

  news_json <-
    jsonlite::fromJSON(
      html_text(scripts[1]),
      simplifyVector = FALSE
    )

  claim_json <-
    jsonlite::fromJSON(
      html_text(scripts[2]),
      simplifyVector = FALSE
    )

  # ---------- Article ----------

  article_node <-
    html_element(
      page,
      "article"
    )

  article_text <-
    safe_text(article_node)

  tibble(

    source = "Lead Stories",

    url = article$url,

    title = safe_json(news_json$headline),

    subtitle = NA_character_,

    excerpt = safe_json(news_json$description),

    date_published =
      lubridate::ymd_hms(
        news_json$datePublished,
        quiet = TRUE
      ),

    date_modified =
      lubridate::ymd_hms(
        news_json$dateModified,
        quiet = TRUE
      ),

    claim =
      safe_json(
        claim_json$claimReviewed
      ) |>
      stringr::str_squish(),

    verdict =
      safe_json(
        claim_json$reviewRating$alternateName
      ),

    article_section = safe_list(news_json$articleSection),

    article_text = article_text,

    author = safe_json(news_json$author$name),

    image_url = safe_json(news_json$image$url),

    word_count = count_words(article_text),

    language = safe_json(news_json$inLanguage),

    scrape_time = Sys.time(),

    scrape_status = "Success",

    error_message = NA_character_

  )

}

#===========================================================
# Safe scraper (returns a Failed row instead of stopping)
#===========================================================

safe_scrape_leadstories <- function(article){

  tryCatch(

    scrape_leadstories(article),

    error = function(e){

      tibble(
        source = "Lead Stories",
        url = article$url,
        title = NA_character_,
        subtitle = NA_character_,
        excerpt = NA_character_,
        date_published = as.POSIXct(NA),
        date_modified = as.POSIXct(NA),
        claim = NA_character_,
        verdict = NA_character_,
        article_section = NA_character_,
        article_text = NA_character_,
        author = NA_character_,
        image_url = NA_character_,
        word_count = NA_integer_,
        language = NA_character_,
        scrape_time = Sys.time(),
        scrape_status = "Failed",
        error_message = conditionMessage(e)
      )

    }

  )

}

#testing on 5 articles

leadstories_test_batch <-
  purrr::map_dfr(
    1:5,
    function(i){
      cat("Scraping article", i, "of 5\n")
      Sys.sleep(DELAY_SECONDS)
      safe_scrape_leadstories(leadstories_archive[i, ])
    }
  )

table(leadstories_test_batch$scrape_status)
colSums(is.na(leadstories_test_batch))

#===========================================================
# Step 2: Scrape all articles in batches (resumable)
#===========================================================

batches <-
  split(
    leadstories_archive,
    ceiling(
      seq_len(nrow(leadstories_archive)) /
        BATCH_SIZE
    )
  )

length(batches)

process_batch <- function(batch_number){

  batch <- batches[[batch_number]]

  results <-
    purrr::map_dfr(
      seq_len(nrow(batch)),
      function(i){

        cat(
          "Batch", batch_number,
          "- article", i,
          "of", nrow(batch), "\n"
        )

        Sys.sleep(DELAY_SECONDS)                     #polite delay between articles

        safe_scrape_leadstories(
          batch[i, ]
        )

      }
    )

  readr::write_csv(
    results,
    file.path(
      OUTPUT_DIR,
      paste0("leadstories_batch_", batch_number, ".csv")
    )
  )

  invisible(results)

}

print(Sys.time())

for(i in seq_along(batches)){

  outfile <-
    file.path(
      OUTPUT_DIR,
      paste0("leadstories_batch_", i, ".csv")
    )

  # Skip if already completed
  if(file.exists(outfile)){
    cat("\nBatch", i, "already exists. Skipping.\n")
    next
  }

  cat("\n=====================================\n")
  cat("Starting Batch", i, "of", length(batches), "\n")
  cat("=====================================\n\n")

  process_batch(i)

  gc()

  Sys.sleep(2)

}

print(Sys.time())

cat("\n\nAll batches completed!\n")

#combine batches

files <-
  list.files(
    OUTPUT_DIR,
    pattern = "leadstories_batch_.*\\.csv$",
    full.names = TRUE
  )

leadstories <-
  files |>
  purrr::map_dfr(
    read_csv,
    show_col_types = FALSE
  )

nrow(leadstories)
table(leadstories$scrape_status)
sum(duplicated(leadstories$url))

leadstories |>
  filter(scrape_status == "Failed") |>
  count(error_message)

write_csv(
  leadstories,
  file.path(OUTPUT_DIR, "leadstories_raw.csv")
)

#===========================================================
# Step 3: Rerun failed articles once and rebuild the dataset
#===========================================================

leadstories_failed <-
  leadstories |>
  filter(scrape_status == "Failed")

nrow(leadstories_failed)

leadstories_rerun <-
  purrr::map_dfr(
    seq_len(nrow(leadstories_failed)),
    function(i){
      cat("Recovering article", i, "of", nrow(leadstories_failed), "\n")
      Sys.sleep(DELAY_SECONDS)
      safe_scrape_leadstories(leadstories_failed[i, ])
    }
  )

table(leadstories_rerun$scrape_status)

leadstories_final <-
  dplyr::bind_rows(
    leadstories |> filter(scrape_status == "Success"),
    leadstories_rerun
  )

#QC
nrow(leadstories_final)
table(leadstories_final$scrape_status)
sum(duplicated(leadstories_final$url))

write_csv(
  leadstories_final,
  file.path(OUTPUT_DIR, "leadstories_final.csv")
)

#===========================================================
# Step 4: Topic filter and date filter
#===========================================================

leadstories_topic <-

  leadstories_final |>

  filter(scrape_status == "Success") |>

  mutate(
    search_text =
      paste(
        title,
        excerpt,
        claim,
        article_text,
        sep = " "
      )
  ) |>

  filter(
    str_detect(
      str_to_lower(search_text),
      topic_pattern
    )
  )

in_windows <- function(d){

  keep <- rep(FALSE, length(d))

  for(w in date_windows){

    keep <- keep | (d >= as.Date(w[1]) & d <= as.Date(w[2]))

  }

  keep

}

leadstories_topic <-
  leadstories_topic |>
  filter(
    in_windows(as.Date(date_published))
  )

#QC
nrow(leadstories_topic)

leadstories_topic |>
  count(format(date_published, "%Y"))

colSums(is.na(leadstories_topic))

write_csv(
  leadstories_topic,
  file.path(OUTPUT_DIR, "leadstories_topic.csv")
)

#END
