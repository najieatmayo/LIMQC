## About `LIMQC` Shiny App


LIMQC provides interactive graphic user interface for selecting and examining quality data element of interests, with exportable table and figure results.

## Run method 1

To run this application localy, simple paste the following code on `R` console: 

```{r} 
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("LIMQC", "najieatmayo")
```

## Run method 2

Or you can clone or download this repository, set working directory to LIMQC and use following code on `R` console:

```{r} 
shiny::runApp()
```
## Want to help?

Please, report bugs to na.jie@mayo.edu


## License

This software is Open Source and is under the public license [GPL-3.0](http://www.gnu.org/licenses/gpl-3.0.en.html)

_The OSI logo trademark is the trademark of [Open Source Initiative](http://opensource.org/)_

## Author

Jie Na (na.jie@mayo.edu)

## Credit

This application was developed with [shiny](http://shiny.rstudio.com/) in 
[R studio](https://www.rstudio.com/).
