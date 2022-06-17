# Shortcut files to start local GUI of IsoOrbi ShinyApp

## IsoOrbi_shiny.bat - for Windows

open in a text editor and modify your path to R and the app.R file on line 1

```
start "" "C:/Program Files/R/R-4.2.0/bin/R.exe" C:/Users/kantn/Dropbox/Dokumenty/US_Boulder/GitHub/isoorbi/app/app.R /k
start "IsoOrbi ShinyApp" "http://127.0.0.1:3838/"
```

in the app.R file, the `launch` option has to be set to `TRUE`:

```
isoorbi::iso_start_orbitrap_gui_server(
    data_folder = "data",
    launch = TRUE,
    log = TRUE
)
```
