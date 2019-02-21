# HistoryOfEarth
Code for producing the History of Earth phylotastic educational resources website

To change default taxa, modify GetTaxa in R/historyofearth.R

To update maps or tree, `CacheMaps()` or `CacheTree()`, then commit and push the changes (which are stored in the data directory)

To run the site, `HistoryOfEarth::runSite()`

The other approach is to go to docs/index.Rmd, do `rmarkdown::render('index.Rmd')`, commit index.html, and push, and deploy on github pages: index.html will work there.
