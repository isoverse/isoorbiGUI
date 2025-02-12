# Shortcut files to start local GUI of IsoOrbi ShinyApp

## IsoOrbi_shiny_v2.bat - for Windows

open in a text editor and modify:

1. your browser on line 1 ("chrome", "firefox", ...)
2. your path to R and the app.R file on line 2

!!! the file path to R has backward slashes (Windows standard) but the file path to the app.R file has to have forward slashes (as that is read by the R terminal)

```
start chrome http://127.0.0.1:3838/
"C:\Program Files\R\R-4.2.0\bin\Rscript.exe" -e "shiny::runApp('/Users/kantn/Dropbox/Dokumenty/US_Boulder/GitHub/isoorbi/app/app.R')"
```

in the app.R file, the `launch` option has to be set to `TRUE`:

```
isoorbi::orbi_start_orbitrap_gui_server(
    data_folder = "data",
    launch = TRUE,
    log = TRUE
)
```

note: IsoOrbi_shiny.bat does not work, even though the syntax is oficially reported to be correct by the developers. `launch = TRUE` at the `shiny::RunApp` command does not work so the browser has to be initiated before running line #2