release_questions <- function() {
  c(
    "Have you updated the DESCRIPTION file? Make sure the version number is right.",
    "Have you checked for reverse dependencies?",
    "Have you updated README.md?",
    "Have you updated NEWS.md?",
    "Have you updated cran-comments.md?",
    "Have you updated all the documentation using devtools::check_man()?",
    "Have you gotten approval from all authors to push to CRAN?",
    "Did you make sure the DESCRIPTION matches what's in arsenal.R?"
  )
}
