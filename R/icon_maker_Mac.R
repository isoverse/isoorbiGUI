# This script helps with the creation of a desktop icon to start a local instance of the isoorbi website with a simple double click of a desktop icon
# Works on Mac OS


#run script in the local folder that contains app.R

#isoorbi icon

fileConn<-file("~/Desktop/isoorbi.command")
writeLines(paste0("Rscript ", "'", getwd(),"/app.r'", "& open http://localhost:3838/;"), fileConn)
close(fileConn)

Sys.chmod("~/Desktop/isoorbi.command", mode = "0777") #change file permissions
file.rename("~/Desktop/isoorbi.command", "~/Desktop/isoorbi") #remove file extension


#isoorbi Online icon
fileConn<-file("~/Desktop/isoorbi Online.command")
writeLines(paste0("open https://isobar.shinyapps.io/temp_isoorbi/;"), fileConn)
close(fileConn)

Sys.chmod("~/Desktop/isoorbi Online.command", mode = "0777") #change file permissions
file.rename("~/Desktop/isoorbi Online.command", "~/Desktop/isoorbi Online") #remove file extension



#copy two icon files to the desktop
#isoorbi
file.copy(from = "icons/isoorbi.ico", to = "~/Desktop/isoorbi.ico")
#isoorbi Online
file.copy(from = "icons/isoorbi Online.ico", to = "~/Desktop/isoorbi Online.ico")

#then manually right click 'isoorbi' file -> more info; then drag and drop icon file onto icon in the left top corner
#finally manually delete icon file from desktop


