# Getting Started

This guide will briefly explain how to update the HWMO webapp.

## Installing R

The HWMO webapp requires the programming
language [R](https://www.r-project.org). The app was built with version 3.4.4. Download and install the current release after selecting your nearest location on the R [mirror page](https://cran.r-project.org/mirrors.html) or use the [Berkeley mirror](https://cran.cnr.berkeley.edu/).

### RStudio Editor Support

We recommend you use RStudio (the free, desktop version) to run R:

- [Rstudio](https://www.rstudio.com/products/rstudio/download/)

## Installing packages

You will need to install a fair amount of packages to run the application. These packages can be found at the top of `ui.R` and `server.R` code, e.g. `library(shinydashboard)` where `shinydashboard` is the package. You can install these packages by running the following code in your RStudio console panel:

```
install.packages("package1", "package2", "package3")
```

## Installing HWMO webapp

Clone or download the HWMO webapp repository from the HWMO webapp [Github website](https://github.com/niklaslollo/HWMO_webapp).

## Using the HWMO webapp

To see the webapp, open either `server.R` or `ui.R` and click on the `Run App` button in the top right of the RStudio dashboard. This opens the shiny app locally so you can visualize your changes. This local version will not auto-update; if you want to make and view any changes, you need to exit the shiny window and click `Run App` once again.  

When you want to publish to your server (i.e., put the app online), click on the icon to the right of `Run App`. This will guide you.

For more information, see the [Technical Guide](technicaluserguide.md).

## Troubleshooting

We recommend [StackOverflow](stackoverflow.com) and the [RStudio Community](https://community.rstudio.com/) for help with coding new features or troubleshooting problems.

For specific questions, you can send an email to [Niklas Lollo](https://www.niklaslollo.github.io/) (<nicklollo2@gmail.com>).

