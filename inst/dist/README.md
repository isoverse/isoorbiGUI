# standalone windows installer

To generate a stand-alone Windows installer, follow these steps (all on a Windows computer):

1. Clone this repo to the Windows computer.
1. Download <https://portableapps.com/apps/internet/google_chrome_portable> and install it into the `dist` folder. This will create the `dist/GoogleChromePortable` folder containing a stand-alone installation of the chrome brower.
1. Download the latest `R-portable_VERSION.paf.exe` from <https://sourceforge.net/projects/rportable/> and install it also in the `dist` folder. This will create the `dist/R-Portable` folder containing a stand-alone installation of R.
1. Open a Windows command prompt (`cmd`) and navigate to the `dist` folder (`cd .../dist`).
1. Execute `install_app.bat` in the command prompt to get the dependencies for the app installed as defined in the `install_app.R` script.
1. Execute `run_app.bat` to see if the app runs properly. If so, also test this by double-clicking on `app.vbs` in the `dist` directory.
1. Install Inno Setup from <http://www.jrsoftware.org/isdl.php> (regular install location in Programs).
1. Start the Inno Setup program and load the `setup.iss` installer configuration file.
1. If this is a completely new app, run `Tools | Generate GUID inside the IDE` to make sure the app gets a unique ID
1. Start the setup creation process (will take a few minutes).
1. Once complete, test the installer (it will automatically start, otherwise just run the `setup.exe` that's now in the `dist` folder).
1. Once the app is installed, confirm that it opens and runs as expected from the desktop link and start menu item.
1. You can uninstall the app again in the regular Windows uninstall (find it by name).
1. Distribute the `setup.exe` for standalone installation. Keep in mind that the source code is not encrypted and can be freely accessed from the installed app.

## todo:

Consider including explicit license and readme files into the inno setup compiler, example: http://blog.analytixware.com/2014/03/packaging-your-shiny-app-as-windows.html

