#' stores the settings
#' @param save_settings whether to save settings to the settings file
#' @param settings_file_path reactive function returning the settings file path
module_settings_server <- function(input, output, session, save_settings = FALSE, settings_file_path = reactive({})) {

  # reactive values ====
  values <- reactiveValues(
    settings = list()
  )

  # reset gui settings
  reset <- function() {
    module_message(session$ns, "info", "RESETTING gui settings")
    isolate({ values$settings <- list() })
    save()
  }

  # generate gui setting name
  make_name <- function(name, ns = NULL, variable = NULL) {
    if (!is.null(variable)){
      name <- paste0(name, "-", variable)
    }
    if (!is.null(ns)) {
      name <- ns(name)
    }
    return(name)
  }

  # generate message save char
  make_msg_value <- function(value) {
    if (is.list(value)) "<list>"
    else if (is.data.frame(value)) "<tibble>"
    else paste(as.character(value), collapse = ", ")
  }

  # set gui setting
  set <- function(name, value, ns = NULL, variable = NULL, quiet = FALSE) {
    # make name
    name <- make_name(name, ns = ns, variable = variable)

    # info message if namespace is provided
    if (!is.null(ns) && !quiet) {
      module_message(
        ns, "info",
        sprintf("SETTINGS set '%s' to '%s'", name, make_msg_value(value))
      )
    }
    isolate({ values$settings[[name]] <- value })
    save()
  }

  # returns gui setting if available, otherwise returns the default
  get <- function(name, ns = NULL, variable = NULL, default = NULL) {
    # make name
    name <- make_name(name, ns = ns, variable = variable)

    isolate({
      if (is.null(values$settings[[name]])) return(default)
      else return(values$settings[[name]])
    })
  }

  # get all gui settings table
  get_all_as_table <- function() {
    isolate({
      if (length(values$settings) == 0) return(tibble::tibble())
      tibble::tibble(
        setting = names(values$settings),
        type = purrr::map_chr(values$settings, ~class(.x)[1]),
        length = purrr::map_int(values$settings, length),
        value = purrr::map_chr(values$settings, make_msg_value)
      ) %>% dplyr::arrange(setting)
    })
  }

  # save gui settings
  save <- function() {
    if (save_settings) {
      isolate({
        file_path <- settings_file_path()
        module_message(
          session$ns, "info",
          sprintf("SETTINGS saving to '%s'", file_path)
        )
        readr::write_rds(values$settings, file_path)
      })
    }
  }

  # load gui settings
  load <- function() {
    isolate({
      file_path <- settings_file_path()
      if (file.exists(file_path)) {
        module_message(
          session$ns, "info",
          sprintf("SETTINGS loading from '%s'", file_path)
        )
        values$settings <- readr::read_rds(file_path)
      } else {
        module_message(
          session$ns, "info",
          sprintf("SETTINGS cannot load from '%s', file does not exist, resetting settings", file_path)
        )
        values$settings <- list()
      }
    })
  }

  # module functions
  list(
    load = load,
    reset = reset,
    set = set,
    get = get,
    get_all_as_table = get_all_as_table
  )
}
