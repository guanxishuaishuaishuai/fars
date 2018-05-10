---
title: "Vegnette for fars package for Coursera"
output: pdf_document
---

This vegnette is used for the \texttt{fars} package for the assignment of the course "r packages" on Coursera. The main R file is given. 

The main usage of this package is to plot the accident rate on the map for each State in United States.

The sturcture of data is like this

```{r,message=FALSE}
library(dplyr)
library(readr)
dt <- read_csv("accident_2013.csv.bz2")
dt
```



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##fars_read function

This function is used to load in the data.

```{r,echo=FALSE,message=FALSE}

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
```

```{r}
fars_read("accident_2013.csv.bz2")
```

##make_filename

The function is used to create a string as the name of the file. It uses the year as suffix.
```{r,echo=FALSE}
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
```

```{r}
make_filename(2012)
```

##fars_read_years

This function is used to grab the month and the year from data.
```{r,echo=FALSE}
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
```


```{r}
fars_read_years(2013:2014)
```


##fars_summarize_years

The function is used to get a data frame containing the total number of observations in each month for each year. It counted all observations in the data.

```{r,echo=FALSE}
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

```


```{r}
fars_summarize_years(2013:2014)
```

##fars_map_state

The function is used to draw a map showing the accident rate for each state.
The state number is from 1 to 51.

```{r}
unique(dt$STATE)
```


```{r,echo=FALSE}
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
      xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

```


```{r}
fars_map_state(1, 2014)
```

