# eodplotter

Plot EOD


## Install

    install.packages('devtools')
    devtools::install_github('msuefishlab/eodplotter')

## Notes

Builds off the msuefishlab/tdmsreader. Also see msuefishlab/tdmsviewer


## Recommended setup

I recommend setting up the script so that it is in your $PATH variable

```
export PATH=$PATH:`Rscript -e 'cat(.libPaths())'`/eodplotter/scripts
function ploteod() {
   peak_finder -f "$1" -v
   mypath=`basename "$1"`.peaks.csv
   eodplot -f "$1" -p "$mypath" -v
}
```

This way, you can simply open up your terminal and type

    ploteod <filename>


