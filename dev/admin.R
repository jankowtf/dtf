
# Dev dependencies --------------------------------------------------------

renv::install("devtools")
renv::install("testthat")
renv::install("roxygen2")
renv::install("roxygen2md")
renv::install("rmarkdown")
renv::install("here")

renv::install("dplyr")
renv::install("webshot")
renv::install("shiny")
webshot::install_phantomjs()

usethis::use_package("shiny", type = "Suggests")

# Dev preps ---------------------------------------------------------------

# Git
# usethis::use_git()

# Use {renv}
# renv::activate()

# "Add the pipe"
usethis::use_pipe()

# Add package description
usethis::use_package_doc(open = FALSE)

# Use {testthat}
usethis::use_testthat()
usethis::use_package("testthat", type = "Suggests")

# Use markdown in roxygen syntax
usethis::use_roxygen_md()
roxygen2md::roxygen2md()

# Misc
usethis::use_mit_license()
usethis::use_readme_rmd(open = FALSE)
usethis::use_lifecycle()
usethis::use_lifecycle_badge("experimental")
usethis::use_news_md(open = FALSE)

usethis::use_build_ignore(
    c(
        "devops",
        "inst/examples",
        "tests",
        "renv"
    )
)

# Prod dependencies -------------------------------------------------------

# --- Install
renv::install("DT")
renv::install("rappster/valid", rebuild = TRUE)
renv::install("rappster/confx")
renv::install("rappster/drop", rebuild = TRUE)
renv::install("assertthat")
renv::install("assertr")
renv::install("logger")
renv::install("snakecase")

# --- Declare
usethis::use_package("DT")
usethis::use_dev_package("valid", type = "Imports", remote = "rappster/valid")
usethis::use_dev_package("confx", type = "Imports", remote = "rappster/confx")
usethis::use_package("assertthat")
usethis::use_package("assertr")
usethis::use_package("logger")
usethis::use_package("snakecase")
usethis::use_dev_package("drop", type = "Imports", remote = "rappster/drop")

# Tests -------------------------------------------------------------------

usethis::use_test("dt")
usethis::use_test("bundles")
usethis::use_test("helpers")

# Vignette ----------------------------------------------------------------

usethis::use_vignette("overview")
