echo Starting Shiny
cd program
git pull
Rscript -e "library(renv)"
# Rscript -e "renv::init(bioconductor = TRUE)"
Rscript -e "print(getwd())"
Rscript -e "renv::restore(lockfile='renv.lock')"
RScript -e "renv::init(bioconductor = TRUE)"
Rscript -e "library(shiny)"
screen -S OmicShiny -d -m Rscript -e "shiny::runApp('shinyApp',port=3939)"
echo running...
