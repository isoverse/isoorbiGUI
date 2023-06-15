@echo off
REM This script can be used to start the app by command line
REM However app.vbs is usually the start file for standard use
SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
R-Portable\App\R-Portable\bin\Rscript.exe %ROPTS% run_app.R
