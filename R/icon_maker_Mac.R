# DISABLE for deploying app to shinyapps.io! Caj: 13-July-2022

# This script helps with the creation of a desktop icon to start a local instance of the isoorbi website with a simple double click of a desktop icon
# # Works on Mac OS
#
#
# #run script in the local folder that contains app.R
#
# #isoorbi icon
#
# fileConn<-file("~/Desktop/isoorbiGUI.command")
# writeLines(paste0("Rscript ", "'", getwd(),"/app.r'", "& open http://localhost:3838/;"), fileConn)
# close(fileConn)
#
# Sys.chmod("~/Desktop/isoorbiGUI.command", mode = "0777") #change file permissions
# file.rename("~/Desktop/isoorbiGUI.command", "~/Desktop/isoorbi") #remove file extension
#
#
# #isoorbi Online icon
# fileConn<-file("~/Desktop/isoorbiGUI Online.command")
# writeLines(paste0("open https://isobar.shinyapps.io/temp_isoorbiGUI/;"), fileConn)
# close(fileConn)
#
# Sys.chmod("~/Desktop/isoorbiGUI Online.command", mode = "0777") #change file permissions
# file.rename("~/Desktop/isoorbiGUI Online.command", "~/Desktop/isoorbi Online") #remove file extension
#
#
#
# #copy two icon files to the desktop
# #isoorbi
# file.copy(from = "icons/isoorbiGUI.ico", to = "~/Desktop/isoorbiGUI.ico")
# #isoorbi Online
# file.copy(from = "icons/isoorbiGUI Online.ico", to = "~/Desktop/isoorbiGUI Online.ico")
#
# #then manually right click 'isoorbiGUI' file -> more info; then drag and drop icon file onto icon in the left top corner
# #finally manually delete icon file from desktop
#
#
