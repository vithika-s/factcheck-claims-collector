############################################################
## Google Fact Check Tools API - claim collection
## Collects fact-checked claims (ClaimReview) for a bank of search queries,
## flattens them, removes duplicates, filters by date and topic,
## classifies ratings, and recovers missing review dates from article metadata.
############################################################

####Settings (change here only)####

# API key is read from an environment variable, never written in the script.
# In R: Sys.setenv(GOOGLE_FACTCHECK_API_KEY = "your-key")  or add it to ~/.Renviron
api_key <- Sys.getenv("GOOGLE_FACTCHECK_API_KEY")
if(api_key == "") stop("Set GOOGLE_FACTCHECK_API_KEY before running.")

OUTPUT_DIR <- "GoogleFactCheck_Output"

# Query bank: named groups of search terms (demo topic: vaccines)
query_groups <- list(

  Vaccines = c(
    "vaccine","vaccines","vaccination","vaccinated",
    "booster","mrna","immunization"
  ),

  Safety_Claims = c(
    "vaccine side effects","vaccine injury","vaccine deaths",
    "myocarditis","autism vaccine"
  ),

  Institutions = c(
    "cdc","fda","who vaccine","pfizer","moderna"
  )

)

# Review-date windows to keep (any number of start/end pairs)
date_windows <- list(
  c("2020-01-01", "2021-12-31"),
  c("2024-01-01", "2025-12-31")
)

# Topic filter applied to claim + review title (regular expression)
topic_pattern <- "vaccin|booster|mrna|immuni|myocarditis|pfizer|moderna"

# Publishers whose missing review dates are recovered from the article page
# (check each site's terms of use before adding a publisher)
date_recovery_publishers <- c("FactCheck.org")

####Packages####

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(lubridate)
library(rvest)

#helper function to handle missing values
`%||%` <- function(x, y){

  if(is.null(x))
    y
  else
    x

}

dir.create(
  OUTPUT_DIR,
  showWarnings = FALSE
)

#Build collector function (follows nextPageToken until the last page)
get_google_claims <- function(query, api_key){

  message("Searching: ", query)

  base_url <-
    "https://factchecktools.googleapis.com/v1alpha1/claims:search"

  page_token <- NULL

  results <- list()

  repeat{

    req <- GET(
      url = base_url,
      query = list(
        query = query,
        languageCode = "en",
        pageSize = 100,
        pageToken = page_token,
        key = api_key
      )
    )

    data <-
      fromJSON(
        content(req, "text", encoding = "UTF-8"),
        simplifyVector = FALSE
      )

    if(is.null(data$claims))
      break

    results <-
      append(
        results,
        data$claims
      )

    if(is.null(data$nextPageToken))
      break

    page_token <-
      data$nextPageToken

    Sys.sleep(0.25)

  }

  return(results)

}

#function to flatten the claims (one row per claim, first review)

flatten_google_claims <- function(claims,
                                  search_query,
                                  category_name){

  purrr::map_dfr(

    claims,

    function(x){

      #-----------------------------
      # Handle missing claimReview
      #-----------------------------

      if(is.null(x$claimReview) || length(x$claimReview) == 0){

        return(

          tibble(
            claim = x$text %||% NA_character_,
            category = category_name,
            publisher = NA_character_,
            publisher_site = NA_character_,
            review_title = NA_character_,
            review_url = NA_character_,
            review_date = as.POSIXct(NA),
            textual_rating = NA_character_,
            language = NA_character_,
            search_query = search_query,
            scrape_time = Sys.time()
          )

        )

      }

      #-----------------------------
      # Use first review
      #-----------------------------

      review <- x$claimReview[[1]]

      publisher <-
        if(!is.null(review$publisher$name))
          review$publisher$name
      else
        NA_character_

      # Standardize publisher names
      publisher <-
        publisher |>
        str_remove("\\.com$") |>
        str_trim()

      tibble(

        claim = x$text %||% NA_character_,

        category = category_name,

        publisher = publisher,

        publisher_site =
          if(!is.null(review$publisher$site))
            review$publisher$site
        else
          NA_character_,

        review_title =
          review$title %||% NA_character_,

        review_url =
          review$url %||% NA_character_,

        review_date =
          suppressWarnings(
            ymd_hms(
              review$reviewDate %||% NA_character_,
              quiet = TRUE
            )
          ),

        textual_rating =
          review$textualRating %||% NA_character_,

        language =
          review$languageCode %||% NA_character_,

        search_query = search_query,

        scrape_time = Sys.time()

      )

    }

  )

}

#Run all queries in one group, with a QC summary and one CSV per group

run_google_category <- function(category_name,
                                queries,
                                api_key){

  category_results <- list()

  for(i in seq_along(queries)){

    message(
      "Category: ",
      category_name,
      " | Query ",
      i,
      "/",
      length(queries),
      ": ",
      queries[i]
    )

    try({

      claims <-
        get_google_claims(
          query = queries[i],
          api_key = api_key
        )

      category_results[[i]] <-
        flatten_google_claims(
          claims,
          search_query = queries[i],
          category_name = category_name
        )

    }, silent = TRUE)

    Sys.sleep(0.5)

  }

  category_df <-
    bind_rows(category_results)

  #----------------------------------------
  # QC Summary
  #----------------------------------------

  message("Rows: ", nrow(category_df))
  message("Unique claims: ", n_distinct(category_df$claim))
  message("Duplicate claims: ", sum(duplicated(category_df$claim)))
  message("Publishers: ", n_distinct(category_df$publisher))

  write_csv(
    category_df,
    file.path(
      OUTPUT_DIR,
      paste0(category_name, ".csv")
    )
  )

  message(
    category_name,
    " complete. ",
    nrow(category_df),
    " rows saved."
  )

  return(category_df)

}

####Step 1: Collect all query groups####

print(Sys.time())

google_results <- list()

for(i in seq_along(query_groups)){

  category_name <- names(query_groups)[i]

  google_results[[category_name]] <-

    run_google_category(

      category_name = category_name,

      queries = query_groups[[i]],

      api_key = api_key

    )

}

print(Sys.time())

google_raw <-
  bind_rows(google_results)

write_csv(
  google_raw,
  file.path(OUTPUT_DIR, "google_factcheck_raw.csv")
)

####Step 2: Assign claim_id and remove duplicates####

google_raw <-

  google_raw |>

  mutate(

    claim_id = sprintf(
      "GFC_%06d",
      row_number()
    ),

    .before = 1

  )

# duplicates = same claim from the same publisher (found by more than one query)
google_with_publisher <-

  google_raw |>

  filter(!is.na(publisher)) |>

  distinct(
    claim,
    publisher,
    .keep_all = TRUE
  )

google_missing_publisher <-

  google_raw |>

  filter(is.na(publisher))

google_dedup <-

  bind_rows(

    google_with_publisher,

    google_missing_publisher

  )

nrow(google_raw)
nrow(google_dedup)
nrow(google_raw) - nrow(google_dedup)

####Step 3: Date filter (missing dates kept for recovery)####

in_windows <- function(d){

  keep <- rep(FALSE, length(d))

  for(w in date_windows){

    keep <- keep | (d >= as.POSIXct(w[1]) &
                      d <= as.POSIXct(paste(w[2], "23:59:59")))

  }

  keep

}

google_date <-

  google_dedup |>

  filter(
    is.na(review_date) | in_windows(review_date)
  )

nrow(google_dedup)
nrow(google_date)
table(format(google_date$review_date, "%Y"), useNA = "ifany")

####Step 4: Topic filter####

google_topic <-

  google_date |>

  mutate(
    search_text = paste(
      claim,
      review_title,
      sep = " "
    )
  ) |>

  filter(
    str_detect(
      str_to_lower(search_text),
      topic_pattern
    )
  )

nrow(google_topic)

google_topic |>
  count(publisher, sort = TRUE)

####Step 5: Classify ratings####

#------------------------------------------------------------
# Ratings to REMOVE (accurate claims)
#------------------------------------------------------------

exclude_ratings <- c(
  "True",
  "Mostly True",
  "Half True",
  "Half true",
  "Accurate",
  "Correct Attribution",
  "Largely accurate",
  "correct"
)

#------------------------------------------------------------
# Ratings that are CLEARLY misinformation
#------------------------------------------------------------

misinformation_ratings <- c(

  "False", "FALSE", "False.",
  "Mostly False", "Mostly false",

  "Pants on Fire",
  "Four Pinocchios", "Three Pinocchios", "Two Pinocchios",

  "Fake", "fake",

  "Misleading", "misleading", "MISLEADING",

  "Missing Context", "Missing context",
  "Needs Context", "Needs context",
  "Lacks context",
  "Out of Context", "Out Of Context",

  "Distorts the Facts",
  "Spins the Facts",

  "Unsupported",
  "Unproven",
  "Unproven (retired)",

  "Partly False", "Partly false",

  "Wrong",
  "Wrong Numbers",

  "Altered", "Altered Image", "Altered Video", "Altered video",
  "Altered photo", "Altered picture",

  "Miscaptioned",

  "Not the Whole Story",
  "Not full story"

)

#------------------------------------------------------------
# Filter accurate ratings and classify the rest
#------------------------------------------------------------

google_final <-

  google_topic |>

  filter(
    !textual_rating %in% exclude_ratings
  ) |>

  mutate(

    review_status = case_when(

      # Already-classified misinformation ratings
      textual_rating %in% misinformation_ratings ~
        "Misinformation",

      # Anything explicitly saying FALSE
      str_detect(str_to_lower(textual_rating), "false") ~
        "Misinformation",

      # Fake / forged / edited / altered
      str_detect(str_to_lower(textual_rating),
                 "fake|forger|edited|altered") ~
        "Misinformation",

      # Wrong / incorrect
      str_detect(str_to_lower(textual_rating),
                 "wrong|incorrect") ~
        "Misinformation",

      # No evidence / unsupported
      str_detect(str_to_lower(textual_rating),
                 "no evidence|unsupported|unproven|unfounded|baseless|not proven|lacks evidence") ~
        "Misinformation",

      # Misleading
      str_detect(str_to_lower(textual_rating),
                 "misleading|distort|miscaption") ~
        "Misinformation",

      # Everything else is left for manual review
      TRUE ~
        "Review"

    )

  )

#QC
table(google_final$review_status)

google_final |>
  filter(review_status == "Review") |>
  count(textual_rating, sort = TRUE) |>
  print(n = 200)

write_csv(
  google_final,
  file.path(OUTPUT_DIR, "google_final_before_date_recovery.csv")
)

####Step 6: Recover missing review dates from the article page####
# Reads the article:published_time meta tag of the fact-check page.

recover_date <- function(url){

  tryCatch(

    {

      page <- read_html(url)

      Sys.sleep(1)                                    #polite delay between pages

      date <-
        html_element(
          page,
          "meta[property='article:published_time']"
        ) |>
        html_attr("content")

      ymd_hms(date, quiet = TRUE)

    },

    error = function(e) NA

  )

}

date_missing <-

  google_final |>

  filter(
    publisher %in% date_recovery_publishers,
    is.na(review_date)
  )

nrow(date_missing)

print(Sys.time())

date_missing <-

  date_missing |>

  mutate(

    recovered_review_date = purrr::map(
      review_url,
      recover_date
    )

  ) |>

  mutate(

    recovered_review_date =
      as.POSIXct(
        unlist(recovered_review_date),
        origin = "1970-01-01",
        tz = "UTC"
      )

  )

print(Sys.time())

#QC
sum(!is.na(date_missing$recovered_review_date))
sum(is.na(date_missing$recovered_review_date))

#Merge into master
google_final <-

  google_final |>

  left_join(

    date_missing |>
      select(
        claim_id,
        recovered_review_date
      ),

    by = "claim_id"

  ) |>

  mutate(

    date_source = if_else(
      is.na(review_date) & !is.na(recovered_review_date),
      "Recovered from article page",
      "Google Fact Check API"
    ),

    review_date =
      coalesce(
        review_date,
        recovered_review_date
      )

  ) |>

  select(
    -recovered_review_date
  )

table(google_final$date_source)

# What is still missing (left for manual review)
google_final |>
  filter(is.na(review_date)) |>
  count(publisher, sort = TRUE)

####Step 7: Final files####

# Master file (all classified claims)
write_csv(
  google_final,
  file.path(OUTPUT_DIR, "google_factcheck_master.csv")
)

# Analysis file (dated claims inside the windows)
google_final_date <-

  google_final |>

  filter(
    !is.na(review_date) & in_windows(review_date)
  )

nrow(google_final_date)
table(format(google_final_date$review_date, "%Y"))

write_csv(
  google_final_date,
  file.path(OUTPUT_DIR, "google_factcheck_analysis.csv")
)

#END
