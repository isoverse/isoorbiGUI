# standalone windows installer

To generate a stand-alone Windows installer, follow these steps (all on a Windows computer):

1. Clone this repo to the Windows computer.
1. Download <https://portableapps.com/apps/internet/google_chrome_portable> and install it into the `dist` folder. This will create the `dist/GoogleChromePortable` folder containing a stand-alone installation of the chrome brower.
1. Download the latest `R-portable_VERSION.paf.exe` from <https://sourceforge.net/projects/rportable/> and install it also in the `dist` folder. This will create the `dist/R-Portable` folder containing a stand-alone installation of R.
1. Open a Windows command prompt (`cmd`) and navigate to the `dist` folder (`cd .../dist`).
1. Execute `install_app.bat` in the command prompt to get the dependencies for the app installed as defined in the `install_app.R` script.
1. Execute `run_app.bat` to see if the app runs properly. If so, also test this by double-clicking on `app.vbs` in the `dist` directory.


