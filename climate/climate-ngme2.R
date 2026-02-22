source("climate/climate-fit-gauss.R")
source("climate/climate-fit-gauss_ns.R")
source("climate/climate-fit-nig.R")
source("climate/climate-fit-nig_ns.R")
source("climate/climate-cv.R")

# Save the final result to md
file_addr <- "climate/climate_result.md"

md_content <- c(
  "# Climate Model Results",
  "",
  paste("**Date:**", Sys.Date()),
  "",
  "**Package Versions:**",
  paste("- **ngme2:**", as.character(packageVersion("ngme2"))),
  "",
  "## Gaussian Model",
  "```",
  paste(capture.output(summary(fit_gauss)), collapse = "\n"),
  "```",
  "",
  "## NIG Model",
  "```",
  paste(capture.output(summary(fit_nig)), collapse = "\n"),
  "```",
  "",
  "## Non-Stationary Gaussian Model",
  "```",
  paste(capture.output(summary(fit_gauss_ns)), collapse = "\n"),
  "```",
  "",
  "## Non-Stationary NIG Model",
  "```",
  paste(capture.output(summary(fit_nig_ns)), collapse = "\n"),
  "```",
  "",
  "## Cross Validation Results",
  "```",
  paste(capture.output(print(cv_result)), collapse = "\n"),
  "```"
)

writeLines(md_content, file_addr)
