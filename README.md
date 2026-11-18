# Fact-check claims collector (R)

R scripts that collect fact-checked claims and fact-check articles from three sources and prepare them for analysis: the **Google Fact Check Tools API**, **FactCheck.org** and **Lead Stories**. Each script collects, cleans, removes duplicates, filters by date and topic, and logs failures so they can be retried.

I built this collection pipeline for my dissertation research on misinformation, which is in progress. This repository shares the collection code only. The search terms, topic filter and date windows here are a demo (vaccines, 2020–2021 and 2024–2025); no data or results from the dissertation are included.

## What each script does

| Script | Source | Steps |
|---|---|---|
| `R/00_setup.R` | — | Installs packages; explains how to store the API key outside the code |
| `R/01_google_factcheck_api.R` | Google Fact Check Tools API (ClaimReview data from many fact-checkers) | Query bank by theme → paginated API collection → flatten to one row per claim → remove duplicate claim–publisher pairs → date and topic filters → classify ratings → recover missing review dates from the fact-check page |
| `R/02_factcheck_org.R` | FactCheck.org "fake news" archive | Archive pages → article list → JSON-LD and article text per article, with retries → resumable batches → date and topic filters |
| `R/03_lead_stories.R` | Lead Stories | `sitemap.txt` → keep URLs inside the date windows → NewsArticle and ClaimReview JSON-LD plus article text → resumable batches → rerun of failed articles → topic and date filters |

### Rating classification (Google API script)

Fact-checkers use different rating labels ("Pants on Fire", "Four Pinocchios", "Miscaptioned", "Missing Context" and so on). The script drops accurate ratings, maps a list of known misinformation labels plus keyword patterns (false, fake, altered, wrong, unsupported, misleading) to `Misinformation`, and leaves everything else as `Review` for manual checking.

## How to run

1. Run `R/00_setup.R` once and store your Google API key in `~/.Renviron` as `GOOGLE_FACTCHECK_API_KEY`.
2. Open a script and edit the **Settings** block at the top: queries or topic pattern, date windows, output folder and delay.
3. Run the script. Each one writes its CSV files to its own output folder, which `.gitignore` keeps out of the repository.

## Responsible use

- The Google script uses the official API and reads the key from an environment variable; no key is stored in the code.
- The website scripts request public pages only, pause between requests, and support resuming so that pages are not requested twice.
- FactCheck.org's copyright policy allows its original articles to be reprinted or distributed with credit and notification. Lead Stories' robots.txt allows crawling. Terms change, so check each site's current terms of use before collecting.
- Not every fact-checker allows automated collection, so the scripts cover only sources whose terms permit it.

## Limitations

- Website layouts change, so selectors may need updating.
- The Google API returns the claims its index holds for each query, not a complete archive of any fact-checker.
- Rating classification is rule-based; the `Review` category needs a manual check.

## Data availability

The research these scripts were built for is still in progress. Data are not shared here. Please reach out to me to discuss data availability.

## Author

Vithika Salomi, Ph.D. candidate, Department of Communication, Georgia State University. [LinkedIn](https://www.linkedin.com/in/vithika-salomi) · [ORCID](https://orcid.org/0000-0002-6164-0437)
