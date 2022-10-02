# biologicShinyPlot

R Version to use

```module purge;source /camp/stp/babs/working/software/modulepath_new_software_tree_2018-08-13;module load pandoc/2.2.3.2-foss-2016b;ml R/4.0.3-foss-2020a;R```

Initiate renv session:

```
if ( !require( "remotes" )){
   install.packages( "remotes" )
}

remotes::install_github( "rstudio/renv" )

if ( !file.exists( "renv.lock" ) ){
    renv::activate()
} else {
    renv::restore(prompt = FALSE )
}
```

Create Heatmap app
```
######################################## 
### Install required packages        ###
########################################

if (!require("golem")){
  renv::install("golem")
}

if (!require("rmarkdown")){
  renv::install("rmarkdown")
}

if (!require("thinkr")){
  renv::install("thinkr")
}

if (!require("colourpicker")){
  renv::install("colourpicker")
}

if (!require("ggplot2")){
  renv::install("ggplot2")
}

if (!require("DBI")){
  renv::install("DBI")
}

if (!require("RMySQL")){
  renv::install("RMySQL")
}

if (!require("spelling")){
  renv::install("spelling")
}

if (!require("attachment")){
  renv::install("attachment")
}

if (!require("covrpage")){
  renv::install("covrpage")
}

```

## Initiate new golem 
golem::create_golem("biologicHeatmap")
