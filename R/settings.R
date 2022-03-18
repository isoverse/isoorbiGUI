# retrieve package settings, internal function, not exported
setting <- function(name) {
  value <- getOption(paste0("isoorbi.", name))
  if (is.null(value)) stop("isoorbi setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_setting <- function(name, value, overwrite = TRUE) {
  if (overwrite || !paste0("isoorbi.", name) %in% names(options()))
    options(list(value) %>% rlang::set_names(paste0("isoorbi.", name)))
  return(invisible(value))
}

# turn debug mode on/off
# not exported, used for internal debugging
turn_debug_on <- function() {
  set_setting("debug", TRUE)
}

turn_debug_off <- function(data) {
  set_setting("debug", FALSE)
}

# turn logs on/off
turn_log_on <- function() {
  set_setting("log", TRUE)
}

turn_log_off <- function() {
  set_setting("log", FALSE)
}
