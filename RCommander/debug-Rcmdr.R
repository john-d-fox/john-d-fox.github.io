
# 1. load the Rcmdr package via library(Rcmdr)
# 2. close the Commander window (but don't exit from R)
# 3. source this file
# 4. re-open the Commander window via Commander()
# 5. source the file for the dialog you're working on
# 6. open the dialog by entering your_dialog_function() in the R Console

path <-  "<location of the Rcmdr sources on your computer>/R"  # adjust for your system
files <- list.files(path, pattern=".*\\.R")
files <- paste(path, files, sep="/")
for (file in files) source(file)
library(tcltk)
library(tcltk2)
options(Rcmdr=list(RcmdrEnv.on.path=TRUE, suppress.X11.warnings=TRUE, use.markdown=TRUE))
