# note: this should be run via the run_app.bat or app.vbs to make sure it is executed with the portable R installation instead of the system R installation

# set lib path to only have local lib path
.libPaths(.Library)

# parameters
app_dir <- "app"
portable_browser_path <- "GoogleChromePortable/GoogleChromePortable"

# info
R_lib_paths <- paste(.libPaths(), collapse = ", ")
abs_browser_path <- file.path(getwd(), portable_browser_path)
sprintf("
\n==================================================
\nStarting app '%s'...
\nBrowser: %s
\nR libs: %s
\nStarting app...
", app_dir, abs_browser_path, R_lib_paths) |> message()

# launch
shiny::runApp(
 appDir = app_dir,
 launch.browser = function(shiny_url) {
   cmd <- sprintf("\"%s\" -incognito --new-window --app=%s", abs_browser_path, shiny_url)
   #cmd <- sprintf("\"%s\"", abs_browser_path)
   sprintf("\nLaunch brower '%s' ...", cmd) |> message()
   system(cmd, wait = F)
})
