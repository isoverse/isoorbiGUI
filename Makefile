# tools for active package development

R_MACOS='R'
R_WIN='C:\Program Files\R\R-4.2.0\bin\R.exe'
R=$(R_MACOS)

check:
	Rscript -e "devtools::check()"

gui_dev:
	bundle exec guard

module_shot_noise_test:
	@echo "Starting module..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();test_module_isoxl_shot_noise()'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

module_dual_inlet_test:
	@echo "Starting module..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();test_module_isoxl_dual_inlet()'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

module_single_test:
	@echo "Starting module..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();test_module_isoxl_single()'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done


module_flow_test:
	@echo "Starting module..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();test_module_isoxl_flow()'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

module_make_inputFile_test:
	@echo "Starting module..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();test_module_isoxl_make_inputFile()'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

module_info_test:
	@echo "Starting module..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();test_module_isoxl_info()'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

# windows specific
gui_test:
	@echo "Starting GUI..."
	@sleep 1
	@until $(R) -e 'library(devtools);load_all();turn_debug_on();iso_start_isoxl_app(log=TRUE,reset=FALSE)'; do \
		echo "Restarting GUI..."; \
		sleep 1; \
	done

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
