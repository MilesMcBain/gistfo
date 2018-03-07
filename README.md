# Get It Somewhere The F*** Online 
Turn your RStudio untitled tabs into gists. You monsters.

![wat](https://github.com/MilesMcBain/gistfo/raw/master/inst/media/gistfo.gif)

*Carbon Edition* Now Available:

![very niiice](https://cdn.rawgit.com/MilesMcBain/gistfo/64f35c97/inst/media/carbon.png)

Carbon mode opens the code on https://carbon.now.sh and copies gist url to your clipboard so you can easily paste into a carbon tweet. 

## To Install
```
library(devtools)
install_github("MilesMcBain/gistfo")
library(gistfo)
```

Make sure an RStudio tab is active, then run `gistfo()`. It will open a browser window asking you to authenticate a third-party OAuth application. Should be all good from there. 

Carbon mode: `gistfoc()`
