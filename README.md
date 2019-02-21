# HistoryOfEarth
Code for producing the History of Earth phylotastic educational resources website

To change default taxa, modify taxa_links.csv, then store this as data (usethis::use_data())

To update maps or tree, look at the Cache functions, then commit and push the changes (which are stored in the data directory)

To run the site, `HistoryOfEarth::runSite()`

The other approach is to go to docs/index.Rmd, do `Rscript -e "rmarkdown::render('index.Rmd')"`, commit index.html, and push, and deploy on github pages: index.html will work there.

To see the website in action, go to https://jwiggi18.github.io/HistoryOfEarth/
