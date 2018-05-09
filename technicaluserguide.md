# Technical User Guide

## Folder Structure

The folders are organized as follows.  

**app**  

Here you will find the app's code. This includes the three key files: `server.R`, `ui.R`, and `global.R`. It has three subfolders:  

*data*  

Here you will find data that are utilized by the app.   

*docs*  

Here you will find markdown docs that are read into the Take Action, About, and Help files.   

*www*  

Here you will find the css and various images used in the app.  

**data_processing**

Here you will find some raw data, as well as a file that documents our data processing.   

## App Structure

**server.R**  

This file contains the backend information for the app. It reads in the various data files, creates the map, watches (`observe`) for user clicks, and any other data manipulation. The code is fully documented inline.  

**ui.R**  

This file describes the user interface using the `shinyDashboard` package on top of `shiny`. It is comprised of a header, sidebar, and body. It communicates with the server file. The code is fully documented inline.  

Note: `global.R` is a small file that shares certain information between the server and ui.  

