# clean up
unlink(list.files("man", full.names = TRUE))

# rebuild docs and install
devtools::document()
pak::pak()

# local tests
devtools::test()

# vignettes, readme, site
devtools::clean_vignettes()
pkgdown::clean_site()
rmarkdown::render("README.Rmd")
unlink("README.html")
pkgdown::build_site()

# checks
# local checks
devtools::check()

# remote checks
devtools::check_win_devel()
devtools::check_win_release()
rhub::check_for_cran(platforms = c("solaris-x86-patched", "debian-gcc-release"),
                     show_status = FALSE)
