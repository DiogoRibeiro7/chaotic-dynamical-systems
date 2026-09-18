#!/usr/bin/env Rscript

# Fail CI only for lint findings introduced on lines added by the current
# change. This keeps new code clean without requiring unrelated historical
# lint debt to be fixed in every pull request.

parse_added_lines <- function(diff_lines) {
  added <- list()
  current_path <- NULL

  for (line in diff_lines) {
    if (startsWith(line, "+++ b/")) {
      current_path <- sub("^\\+\\+\\+ b/", "", line)
      if (is.null(added[[current_path]])) {
        added[[current_path]] <- integer(0)
      }
      next
    }

    if (!startsWith(line, "@@") || is.null(current_path)) {
      next
    }

    match <- regexec(
      "^@@ -[0-9]+(?:,[0-9]+)? \\+([0-9]+)(?:,([0-9]+))? @@",
      line
    )
    groups <- regmatches(line, match)[[1L]]

    if (length(groups) == 0L) {
      next
    }

    start <- as.integer(groups[[2L]])
    count <- if (length(groups) >= 3L && nzchar(groups[[3L]])) {
      as.integer(groups[[3L]])
    } else {
      1L
    }

    if (count > 0L) {
      added[[current_path]] <- c(
        added[[current_path]],
        seq.int(start, start + count - 1L)
      )
    }
  }

  added
}

base_sha <- Sys.getenv("LINT_BASE_SHA")
head_sha <- Sys.getenv("LINT_HEAD_SHA")

if (!nzchar(head_sha)) {
  head_sha <- "HEAD"
}

if (!nzchar(base_sha) || grepl("^0+$", base_sha)) {
  base_sha <- paste0(head_sha, "^")
}

diff_lines <- system2(
  "git",
  c(
    "diff",
    "--unified=0",
    "--no-color",
    base_sha,
    head_sha,
    "--",
    "*.R",
    "*.Rmd"
  ),
  stdout = TRUE,
  stderr = TRUE
)

added_lines <- parse_added_lines(diff_lines)
candidate_files <- names(added_lines)
candidate_files <- candidate_files[
  file.exists(candidate_files) &
    grepl("\\.(R|Rmd)$", candidate_files, ignore.case = TRUE)
]

if (length(candidate_files) == 0L) {
  cat("No added R/Rmd lines to lint.\n")
  quit(status = 0L)
}

failure_count <- 0L

for (path in candidate_files) {
  file_lints <- lintr::lint(path)
  new_lints <- Filter(
    function(lint) lint$line_number %in% added_lines[[path]],
    file_lints
  )

  if (length(new_lints) > 0L) {
    cat("\nNew lint findings in ", path, ":\n", sep = "")
    print(new_lints)
    failure_count <- failure_count + length(new_lints)
  }
}

if (failure_count > 0L) {
  cat("\n", failure_count, " new lint finding(s) introduced by this change.\n", sep = "")
  quit(status = 1L)
}

cat("No new lint findings introduced by this change.\n")
