Rexe           = "R-Portable\App\R-Portable\bin\Rscript.exe"
Ropts          = "--no-save --no-environ --no-init-file --no-restore --no-Rconsole"
RScriptFile    = "run_app.R"
Logfile        = "app.log"
strCommand     = Rexe & " " & Ropts & " " & RScriptFile & " 2> " & Logfile

intWindowStyle = 0     ' Hide the window and activate another window.'
bWaitOnReturn  = False ' continue running script after launching R   '

'consider some kind of information message, ideally nonblocking?'
'MsgBox "Hello, World!", vbSystemModal'

CreateObject("Wscript.Shell").Run strCommand, intWindowStyle, bWaitOnReturn
