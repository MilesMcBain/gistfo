# Get It Somewhere The F*** Online 
Turn your RStudio untitled tabs into gists. You monsters.

![wat](https://raw.githubusercontent.com/MilesMcBain/gistfo/master/inst/media/gistfo.gif)

*Carbon Edition* Now Available:

![very niiice](https://raw.githubusercontent.com/MilesMcBain/gistfo/master/inst/media/carbon.png)

Carbon mode opens the code on https://carbon.now.sh and copies gist url to your clipboard so you can easily paste into a carbon tweet. 

## Installation

```
library(devtools)
install_github("MilesMcBain/gistfo")
```

## Usage

### As an addin

Select the text or source file tab you would like to turn into a Gist.
Then, from the RStudio Addins menu select either `Make Private Gist from Text or Tab` or `Make Public Gist and Send to Carbon`.

### From the console

```
library(gistfo)
```

Make sure an RStudio tab is active or select text in a source file, then run `gistfo()`. It will open a browser window asking you to authenticate a third-party OAuth application. Should be all good from there. 

Carbon mode: `gistfoc()`
