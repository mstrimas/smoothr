unlink(list.files("man", full.names = TRUE))
devtools::clean_vignettes()
pkgdown::clean_site()

devtools::document()

devtools::build()
f <- list.files("..", "smoothr.*gz$", full.names = TRUE)
devtools::install_local("../smoothr", force = TRUE)
unlink(f)

devtools::test()
devtools::check()

rmarkdown::render("README.Rmd")
devtools::build_vignettes()
pkgdown::build_site()
file.copy(list.files(".", "README.*png", full.names = TRUE), "docs/")
dir.create("docs/hex-logo/")
file.copy("hex-logo/smoothr.png", "docs/hex-logo/")

# checks
devtools::check_win_devel()
devtools::check_win_release()
rhub::check_for_cran()
