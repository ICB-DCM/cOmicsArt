echo Starting Shiny
cd program
git pull
Rscript -e "library(renv)"
# Rscript -e "renv::init(bioconductor = TRUE)"
pwd
RScript -e "renv::init(bioconductor = TRUE)"
echo ReInit
Rscript -e "renv::restore(lockfile='renv.lock')"
echo Restored!
Rscript -e "library(shiny)"
screen -S OmicShiny -d -m Rscript -e "shiny::runApp('shinyApp',port=3939)"
echo running...
