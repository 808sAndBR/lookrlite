# LookR Lite
LookR Lite is a package for accessing the Looker API through HTTP requests. It's meant to be light-weight and extensible, providing just the barebone functions for putting together R functions that send HTTP requests to Looker API endpoints and receive data back. It is based on Scott Hoover's [LookR package](https://github.com/looker/lookr).

This repository uses [Google's style guide for R](https://google.github.io/styleguide/Rguide.xml) for clarity in all R code!e

# Basic Usage

First login to create a token that you can authenticate with.

```r
looker_token <- ApiLogin('yourcompany.looker.com', 'your_id', 'your_secret')
```

Then you will be able to call the two functions currently implemented.

run_look will retrieve the data from a look that already exists.

```r
look_data <- run_look(looker_token, look_id)
```

get_look will give you metadata about a look.

```r
look_metadata <- get_look(looker_token, look_id)
```

# More to come!