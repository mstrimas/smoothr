# clean
unlink(list.files("man", full.names = TRUE))
devtools::clean_vignettes()
pkgdown::clean_site()

# rebuild docs and install
devtools::document()
devtools::build()

# local tests and checks
devtools::test()
devtools::check()

# vignettes, readme, site
devtools::build_vignettes()
rmarkdown::render("README.Rmd")
pkgdown::build_site()

# checks
devtools::check_win_devel()
devtools::check_win_release()
rhub::check_for_cran(platforms = "solaris-x86-patched")
