# HistoryOfEarth
Code for producing the History of Earth phylotastic educational resources website

To change default taxa, modify taxa_links.csv, then store this as data (usethis::use_data())

To update maps or tree, look at the Cache functions, then commit and push the changes (which are stored in the data directory)

The other approach is to go to docs, do
`Rscript -e "rmarkdown::render('index.Rmd')"; sed -i.bak 's/historyofearth.utf8/History Of Earth/g' index.html; rm index.html.bak; git commit -m"updating site" -a; git push`

. The sed command is to fix an issue with the page title not rendering properly.

Brian O'Meara (bomeara) is the primary author of the map visualizations and code for the trees comes from Dave Bapst's (dwbapst) paleotree (https://github.com/dwbapst/paleotree).

The site is currently undergoing an overhaul to make it more useful to our partner teachers. Among other improvements each period will have its own page and videos will be embedded.

To see the website in action, go to historyofearth.net
