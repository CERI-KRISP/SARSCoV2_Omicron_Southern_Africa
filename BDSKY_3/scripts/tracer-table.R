# -*- mode:ess-mode; -*-
#'
#' tracer-table
#' ============
#'
#' Tool to convert tracer summary to an org-mode/markdown style table.
#'
#' Usage
#' -----
#'
#' ./tracer-table --input <demo.txt> --output demo.org
#'

suppressPackageStartupMessages(library(argparse))

parser <- ArgumentParser()

parser$add_argument(
         "-v",
         "--verbose",
         action = "store_true",
         default = FALSE,
         help = "Verbose output"
       )
parser$add_argument(
         "-i",
         "--input",
         type = "character",
         help = "Input file"
       )
parser$add_argument(
         "-o",
         "--output",
         type = "character",
         help = "Output file"
       )

args <- parser$parse_args()


main <- function(args) {
  foo <- readLines(args$input) |>
    gsub(pattern = "\t", replacement = " | ") |>
    gsub(pattern = "^", replacement = "| ") |>
    gsub(pattern = "$", replacement = " |")
  tbl_header <- head(foo, 1)
  tbl_hline <- tbl_header |>
    gsub(pattern = "[_\ a-zA-Z]", replacement = "-") |>
    gsub(pattern = "-\\|-", replacement = "-+-")
  tbl_body <- tail(foo, -1)
  bar <- c(tbl_header, tbl_hline, tbl_body)
  writeLines(text = bar, con = args$output)
}

if (!interactive()) {
  args <- parser$parse_args()
  main(args)
}
