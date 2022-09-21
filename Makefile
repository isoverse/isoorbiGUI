# tools for active package development

R_MACOS='R'
R_WIN='C:\Program Files\R\R-4.2.0\bin\R.exe'
R=$(R_MACOS)

check:
	Rscript -e "devtools::check()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
