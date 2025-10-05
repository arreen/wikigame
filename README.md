To run the shinyapp you need the following packages, shiny, strinr and Wiki.
```{r}
install.packages("stringr")
install.packages("shiny")
devtools::install_github("arreen/wiki", build_vignettes = TRUE)
```

After all the packages are installed, simply run the following command to play the wikigame:
```{r}
shiny::runGitHub("wikigame", "arreen")
```
