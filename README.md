# Get It Somewhere The F*** Online 
Turn your RStudio untitled tabs into gists. You monsters.

![wat](https://raw.githubusercontent.com/MilesMcBain/gistfo/master/inst/media/gistfo.gif)

### Carbon Edition

Carbon mode opens the code on https://carbon.now.sh and copies gist url to your clipboard so you can easily paste into a carbon tweet. 

![very niiice](https://raw.githubusercontent.com/MilesMcBain/gistfo/master/inst/media/carbon.png)

### Gistfo The App

Also includes an RStudio addin for opening and editing gists in RStudio.

![gistfo_app() preview image](https://raw.githubusercontent.com/MilesMcBain/gistfo/master/inst/media/gistfo-app.png)

## Installation

```r
# install.packages("remotes")
remotes::install_github("MilesMcBain/gistfo")

# To install app dependencies as well
remotes::install_github("MilesMcBain/gistfo", dependencies = TRUE)
```

## Usage

### As an addin

Select the text or source file tab you would like to turn into a Gist.
Then, from the RStudio Addins menu select either _Make Private Gist from Text or Tab_ or _Make Public Gist and Send to Carbon_.

### From the console

```r
library(gistfo)
```

Make sure an RStudio tab is active or select text in a source file, then run `gistfo()`. It will open a browser window asking you to authenticate a third-party OAuth application. Should be all good from there. 

Carbon mode: `gistfoc()`
