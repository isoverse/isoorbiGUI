# tools for active package development

check:
	Rscript -e "devtools::check()"

gui_dev:
	bundle exec guard

# windows specific
gui_test:
	@echo "Starting GUI..."
	@sleep 1
	@until 'C:\Program Files\R\R-4.2.0\bin\R.exe' -e 'library(devtools);load_all();turn_debug_on();iso_start_isoxl_app(log=TRUE,reset=FALSE)'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
