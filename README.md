<p align="center"><a href="http://193.146.75.235/sample-apps/final_apps/layout/"><img src="https://github.com/ordanovich/images/blob/master/logoColorcentered_medium.png?raw=true"></a></p>

## Interactive application for programmatic data retrieval from [WHO](https://www.who.int/nutgrowthdb/database/en/)/[GHO](https://www.who.int/gho/en/)

This appication is based on the [**WHO** package](https://github.com/expersso/WHO). To learn more about the functionality of the package please refer to the [tutorial](https://cran.r-project.org/web/packages/WHO/vignettes/who_vignette.html).

### Nomenclature

- :rocket: [app.R](https://github.com/ordanovich/downloadWHO/blob/master/app.R): Shiny app combining **UI** and **server** parts.
- :globe_with_meridians: [global.R](https://github.com/ordanovich/downloadWHO/blob/master/global.R): helper functions to be used by the app.
- :bar_chart: [report.Rmd](https://github.com/ordanovich/downloadWHO/blob/master/report.Rmd): template markdown file to generate and download an HTML report through the application interface.

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/who_download/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to consult the contents of the data base, retrieve and visualize the desired datasets in a quick and easy-to-manipulate manner. 

