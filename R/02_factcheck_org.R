#### Extracting fact-check articles from FactCheck.org ####

# Collects the "fake news" (debunked viral claims) archive:
# archive pages -> article list -> one row per article (JSON-LD + article text)
# -> date filter -> topic filter.
# FactCheck.org's copyright policy allows its original articles to be
# reprinted and distributed with credit and notification; see README.

####Settings (change here only)####

OUTPUT_DIR    <- "FactCheck_Output"
ARCHIVE_URL   <- "https://www.factcheck.org/fake-news/"
N_PAGES       <- 118                    # number of archive pages (check the last page number on the site)
BATCH_SIZE    <- 500
DELAY_SECONDS <- 0.5                    # pause between requests

# Publication-date windows to keep (any number of start/end pairs)
date_windows <- list(
  c("2020-01-01", "2021-12-31"),
  c("2024-01-01", "2025-12-31")
)

# Topic filter applied to title + excerpt + article text (demo topic: vaccines)
topic_pattern <- "vaccin|booster|mrna|immuni|myocarditis|pfizer|moderna"

####Packages####

# install.packages(c("rvest","xml2","httr2","dplyr","stringr","purrr",
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

####Step 1: Scrape the archive pages####

scrape_archive_page <- function(page_number){

  tryCatch({

    message("Reading archive page ", page_number)

    url <- if(page_number == 1){

      ARCHIVE_URL

    } else {

      paste0(
        ARCHIVE_URL,
        "page/",
        page_number,
        "/"
      )

    }

    page <- read_html(url)

    Sys.sleep(DELAY_SECONDS)

    articles <- html_elements(page, "article")

    tibble(

      archive_page = page_number,

      url = articles |>
        html_element("h3 a") |>
        html_attr("href"),

      title = articles |>
        html_element("h3 a") |>
        html_text2(),

      date = articles |>
        html_element(".entry-meta") |>
        html_text2() |>
        mdy(),

      excerpt = articles |>
        html_element(".entry-content") |>
        html_text2()

    )

  }, error = function(e){

    message("ERROR on page ", page_number, ": ", e$message)

    tibble()

  })

}

print(Sys.time())

archive <- purrr::map_dfr(
  1:N_PAGES,
  scrape_archive_page
)

print(Sys.time())

#Quality control
nrow(archive)
sum(duplicated(archive$url))
range(archive$date, na.rm = TRUE)
colSums(is.na(archive))

write_csv(
  archive,
  file.path(OUTPUT_DIR, "factcheck_archive.csv")
)

####Step 2: Scraper for one article####

#helper functions

read_page <- function(url){

  read_html(url)

}

safe_text <- function(x){

  if(length(x) == 0)
    return(NA_character_)

  html_text2(x)

}

safe_list <- function(x){

  if(is.null(x))
    return(NA_character_)

  if(length(x) == 0)
    return(NA_character_)

  paste(unlist(x), collapse = "; ")

}

#Function to scrape one article

scrape_factcheck <- function(article){

  tryCatch({

    page <- read_page(article$url)

    # ---------- JSON-LD ----------

    json_node <- html_element(
      page,
      "script[type='application/ld+json']"
    )

    json_text <- html_text(json_node)

    fc_json <- jsonlite::fromJSON(
      json_text,
      simplifyVector = FALSE
    )

    article_json <- fc_json$`@graph`[
      sapply(
        fc_json$`@graph`,
        function(x) identical(x$`@type`, "Article")
      )
    ][[1]]

    # ---------- Article ----------

    article_node <- html_element(
      page,
      ".entry-content"
    )

    # ---------- Author names ----------

    author_ids <- sapply(
      article_json$author,
      function(x) x$`@id`
    )

    author_names <- sapply(author_ids, function(id){

      person <- fc_json$`@graph`[
        sapply(
          fc_json$`@graph`,
          function(x)
            identical(x$`@id`, id)
        )
      ][[1]]

      if(is.null(person$name))
        return(NA_character_)

      person$name

    })

    author <- paste(
      author_names,
      collapse = "; "
    )

    tibble(

      source = "FactCheck.org",

      url = article$url,

      title =
        if (is.null(article_json$headline)) {
          article$title
        } else {
          article_json$headline
        },

      subtitle = NA_character_,
      excerpt = article$excerpt,

      date_published =
        lubridate::ymd_hms(
          article_json$datePublished,
          quiet = TRUE
        ),

      date_modified =
        if (is.null(article_json$dateModified)) {
          as.POSIXct(NA)
        } else {
          lubridate::ymd_hms(
            article_json$dateModified,
            quiet = TRUE
          )
        },

      claim = NA_character_,

      verdict = NA_character_,

      article_section =
        safe_list(
          article_json$articleSection
        ),

      article_text =
        safe_text(article_node),

      author = author,

      image_url =
        if (is.null(article_json$thumbnailUrl))
          NA_character_
      else
        article_json$thumbnailUrl,

      word_count =
        if (is.null(article_json$wordCount))
          NA_integer_
      else
        article_json$wordCount,

      language =
        if (is.null(article_json$inLanguage))
          NA_character_
      else
        article_json$inLanguage,

      scrape_time = Sys.time(),

      scrape_status = "Success",

      error_message = NA_character_

    )

  }, error = function(e){

    tibble(

      source = "FactCheck.org",
      url = article$url,
      title = article$title,
      subtitle = NA_character_,
      excerpt = article$excerpt,
      date_published = as.POSIXct(article$date),
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
      error_message = e$message

    )

  })

}

#Retry function

scrape_factcheck_retry <- function(article,
                                   max_attempts = 3){

  for(i in 1:max_attempts){

    result <- scrape_factcheck(article)

    if(result$scrape_status == "Success"){

      return(result)

    }

    message(
      "Retry ",
      i,
      " failed for: ",
      article$url
    )

    Sys.sleep(2)

  }

  result

}

####Step 3: Scrape all articles in batches (resumable)####

batches <- split(
  archive,
  ceiling(seq_len(nrow(archive)) / BATCH_SIZE)
)

length(batches)
sapply(batches, nrow)

print(Sys.time())

for(batch_number in seq_along(batches)){

  outfile <- file.path(
    OUTPUT_DIR,
    paste0("factcheck_batch_", batch_number, ".csv")
  )

  # Skip if already completed
  if(file.exists(outfile)){
    message("Batch ", batch_number, " already exists. Skipping.")
    next
  }

  message("\n==============================")
  message("Starting Batch ", batch_number)
  message("==============================\n")

  factcheck_batch <- purrr::map_dfr(

    seq_len(nrow(batches[[batch_number]])),

    function(i){

      message(
        "Batch ",
        batch_number,
        " | Article ",
        i,
        "/",
        nrow(batches[[batch_number]])
      )

      Sys.sleep(DELAY_SECONDS)

      scrape_factcheck_retry(
        batches[[batch_number]][i, ]
      )

    }

  )

  write_csv(factcheck_batch, outfile)

  message("\nBatch ", batch_number, " complete!\n")

}

print(Sys.time())

#Combining the batches

files <- list.files(
  OUTPUT_DIR,
  pattern = "^factcheck_batch_.*\\.csv$",
  full.names = TRUE
)

factcheck <- files |>
  purrr::map_dfr(
    read_csv,
    show_col_types = FALSE
  )

table(factcheck$scrape_status)

write_csv(
  factcheck,
  file.path(OUTPUT_DIR, "factcheck_raw.csv")
)

####Step 4: Date filter####

in_windows <- function(d){

  keep <- rep(FALSE, length(d))

  for(w in date_windows){

    keep <- keep | (d >= as.Date(w[1]) & d <= as.Date(w[2]))

  }

  keep

}

factcheck <- factcheck |>
  filter(
    in_windows(as.Date(date_published))
  )

#QC
nrow(factcheck)
range(as.Date(factcheck$date_published))

factcheck |>
  count(format(date_published, "%Y"))

write_csv(
  factcheck,
  file.path(OUTPUT_DIR, "factcheck_date_filtered.csv")
)

####Step 5: Topic filter####

factcheck_topic <- factcheck |>

  mutate(
    search_text = str_to_lower(
      paste(
        title,
        excerpt,
        article_text,
        sep = " "
      )
    )
  ) |>

  filter(
    str_detect(
      search_text,
      regex(
        topic_pattern,
        ignore_case = TRUE
      )
    )
  )

nrow(factcheck_topic)

factcheck_topic |>
  count(format(date_published, "%Y"))

colSums(is.na(factcheck_topic))

write_csv(
  factcheck_topic,
  file.path(OUTPUT_DIR, "factcheck_topic.csv")
)

#END
