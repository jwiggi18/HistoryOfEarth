# HistoryOfEarth
Code for producing the History of Earth phylotastic educational resources website

To change default taxa, modify taxa_links.csv, then store this as data (usethis::use_data())

To update maps or tree, look at the Cache functions, then commit and push the changes (which are stored in the data directory)

To run the site, `HistoryOfEarth::runSite()`

The other approach is to go to docs, do `Rscript -e "rmarkdown::render('index.Rmd')"; sed -i.bak 's/index.utf8/History Of Earth/g' index.html; rm index.bak; git commit -m"updating site" -a; git push`. The sed command is to fix an issue with the page title not rendering properly.

To see the website in action, go to https://jwiggi18.github.io/HistoryOfEarth/
