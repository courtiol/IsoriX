## List (and check) all the links of the bookdown and of the documentation of
## the package, so that they can be reviewed before a release (see issue #188).
##
## Usage, from the root of the repository:
##   Rscript release_checks/list_links.R             ## list and check all links
##   Rscript release_checks/list_links.R --no-check  ## only list the links
##
## Automatic checks (such as those performed by CRAN) only detect broken links,
## but not links that are outdated while still working. Such links usually work
## through a redirection (e.g. a renamed GitHub repository, a website that moved),
## so this script reports, for each unique link, where it is used, whether it
## works and whether it is redirected (and to where), for a quick manual review.
##
## The output is written to release_checks/output/links-<date-time>/ (ignored by
## git): open report.html in a browser (links.csv contains the same information).
##
## Requires the package curl (for checking the links).

args <- commandArgs(trailingOnly = TRUE)
check <- !"--no-check" %in% args

if (!dir.exists("bookdown") || !dir.exists("IsoriX")) {
  stop("Please run this script from the root of the repository.")
}
if (check && !requireNamespace("curl", quietly = TRUE)) {
  stop("The package curl is required to check the links: install.packages('curl') (or use --no-check)")
}

## Files to scan --------------------------------------------------------------

files <- c(
  bookdown = list.files("bookdown", pattern = "\\.(Rmd|yml)$", full.names = TRUE), ## (not old_material/)
  package = c(
    list.files("IsoriX/R", pattern = "\\.R$", full.names = TRUE),
    "IsoriX/DESCRIPTION", "IsoriX/inst/CITATION", "README.md"
  )
)
files <- files[file.exists(files)]
source_of_file <- sub("[0-9]+$", "", names(files))

## Extraction of the links -----------------------------------------------------

url_pattern <- "https?://[^][[:space:]()<>\"'`{}|\\\\^]+"

extract_links <- function(file, source) {
  lines <- readLines(file, warn = FALSE)
  hits <- regmatches(lines, gregexpr(url_pattern, lines, perl = TRUE))
  n <- lengths(hits)
  if (sum(n) == 0) {
    return(NULL)
  }
  urls <- unlist(hits)
  urls <- sub("[.,;:!?*]+$", "", urls) ## trailing punctuation is not part of the link
  data.frame(url = urls, source = source, location = paste0(file, ":", rep(seq_along(lines), n)))
}

occurrences <- do.call(rbind, Map(extract_links, files, source_of_file))
links <- do.call(rbind, lapply(split(occurrences, occurrences$url), function(d) {
  data.frame(
    url = d$url[1],
    source = paste(sort(unique(d$source)), collapse = " & "),
    n_uses = nrow(d),
    locations = paste(d$location, collapse = "; ")
  )
}))
rownames(links) <- NULL
message(nrow(links), " unique links found in ", length(files), " files.")

## Checking the links ----------------------------------------------------------

check_link <- function(url) {
  fetch <- function(head) {
    handle <- curl::new_handle(nobody = head, followlocation = TRUE, timeout = 30, useragent = "R link checker")
    curl::curl_fetch_memory(url, handle = handle)
  }
  res <- tryCatch(fetch(head = TRUE), error = function(e) e)
  ## some servers do not answer HEAD requests properly: try again with GET
  if (inherits(res, "error") || res$status_code >= 400) {
    res_get <- tryCatch(fetch(head = FALSE), error = function(e) e)
    if (!inherits(res_get, "error")) res <- res_get
  }
  if (inherits(res, "error")) {
    return(data.frame(http_status = NA, final_url = NA, error = conditionMessage(res)))
  }
  data.frame(http_status = res$status_code, final_url = res$url, error = "")
}

## minor redirections: only the scheme (http -> https), "www.", a default port
## (:80 or :443) or a trailing slash differ
is_minor_redirection <- function(url, final_url) {
  simplify <- function(x) {
    x <- sub("^https?://(www\\.)?", "", x)
    x <- sub("^([^/]+):(80|443)(/|$)", "\\1\\3", x)
    sub("/$", "", x)
  }
  simplify(url) == simplify(final_url)
}

if (check) {
  message("Checking the links (this may take a minute)...")
  checks <- do.call(rbind, lapply(links$url, check_link))
  links <- cbind(links, checks)
  ## many websites (e.g. publishers, Stack Overflow) refuse automated requests
  ## while working fine in a browser: these links must be checked manually
  denied <- !is.na(links$http_status) & links$http_status %in% c(401, 403, 429)
  links$status <- ifelse(denied, "access denied",
    ifelse(is.na(links$http_status) | links$http_status >= 400, "broken",
      ifelse(links$final_url == links$url, "ok",
        ifelse(is_minor_redirection(links$url, links$final_url), "redirected (minor)", "redirected")
      )
    )
  )
  order_status <- c("broken", "access denied", "redirected", "redirected (minor)", "ok")
  links <- links[order(match(links$status, order_status), links$url), ]
} else {
  links$status <- "not checked"
}

## Outputs ---------------------------------------------------------------------

out_dir <- file.path("release_checks", "output", paste0("links-", format(Sys.time(), "%Y%m%d-%H%M%S")))
dir.create(out_dir, recursive = TRUE)
utils::write.csv(links, file.path(out_dir, "links.csv"), row.names = FALSE)

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
a <- function(url) sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>', esc(url), esc(url))

counts <- table(factor(links$status, levels = unique(c("broken", "access denied", "redirected", "redirected (minor)", "ok", links$status))))
rows <- vapply(seq_len(nrow(links)), function(i) {
  l <- links[i, ]
  detail <- if (!check) {
    ""
  } else {
    problem <- if (l$status %in% c("broken", "access denied")) {
      esc(if (is.na(l$http_status)) l$error else paste("HTTP status", l$http_status))
    } else {
      ""
    }
    ## show where the link leads whenever this differs from the link itself
    moved <- if (!is.na(l$final_url) && l$final_url != l$url) paste("&rarr;", a(l$final_url)) else ""
    paste(c(problem, moved)[c(problem, moved) != ""], collapse = "<br>")
  }
  sprintf(
    "<tr class='%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class='loc'>%s</td></tr>",
    gsub("[^a-z]", "", l$status), a(l$url), l$status, detail, l$source, esc(gsub("; ", "\n", l$locations))
  )
}, character(1))

html <- c(
  "<!doctype html><html><head><meta charset='utf-8'><title>IsoriX links</title><style>",
  "body{font-family:sans-serif;margin:24px} table{border-collapse:collapse}",
  "td,th{border:1px solid #ccc;padding:4px 8px;vertical-align:top;text-align:left}",
  ".broken{background:#fde2e2} .accessdenied{background:#e8e8f8} .redirected{background:#fff3cd} .redirectedminor{background:#fdf8e6} .ok{background:#e6f4e6}",
  ".loc{white-space:pre;font-family:monospace;font-size:12px}",
  "</style></head><body>",
  "<h1>Links in the bookdown and in the documentation of IsoriX</h1>",
  sprintf("<p>Generated on %s. %d unique links: %s.</p>", format(Sys.time()), nrow(links), paste(names(counts), counts, sep = ": ", collapse = ", ")),
  "<p>Review in particular the links that are broken or redirected: a redirected link still works, but may be outdated (e.g. a renamed repository or a website that moved). Links with access denied are refused to scripts by their website (common for publishers and some large websites) and must be opened in a browser to be checked. Also check that the remaining links still point to the most relevant pages.</p>",
  "<table><tr><th>link</th><th>status</th><th>details</th><th>source</th><th>used in (file:line)</th></tr>",
  rows,
  "</table></body></html>"
)
writeLines(html, file.path(out_dir, "report.html"))

message(
  if (check) paste0(paste(names(counts), counts, sep = ": ", collapse = ", "), "\n") else "",
  "Report: ", normalizePath(file.path(out_dir, "report.html"))
)
