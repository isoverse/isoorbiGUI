# note: this should be run via the install_app.bat file to make sure it is executed with the portable R installation instead of the system R installation

# set lib path to install into local lib path
.libPaths(.Library)

# info
R_lib_paths <- paste(.libPaths(), collapse = ", ")
sprintf("
\n==================================================
\nInstalling dependencies into R library: \n %s\n\n", R_lib_paths) |>
  message()

# install the required packages (and their dependencies)
install.packages("devtools")
devtools::install_github("isoverse/isoorbi")
devtools::install_github("isoverse/isoorbiGUI", ref = "win-installer")
