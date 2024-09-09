---
layout: default
title: "Local Installation"
nav_order: 5
editor_options: 
  markdown: 
    wrap: 72
---

# Introduction

![A comic about a cat finding
cOmicsART](/cOmicsArt/assets/images/cOmicsTurtle.png) *Image generated
using DALL-E by OpenAI. Adjusted by Lea Seep*

Why do you want to install cOmicsART locally? If you just want to use it
make sure to checkout the website:
[cOmicsART](https://shiny.iaas.uni-bonn.de/cOmicsArt/). For this there
is no installation effort required. If you know you are right here,
let's get started.

# Running a cOmicsART locally within RStudio

# Installing and Running the Shiny App Locally

This guide provides detailed instructions on how to install and run the
Shiny app from the provided GitHub repository.

## Prerequisites

Ensure you have the following software installed on your system: -
[Git](https://git-scm.com/) - [R](https://www.r-project.org/) -
[RStudio](https://rstudio.com/products/rstudio/download/) -
[renv](https://rstudio.github.io/renv/articles/renv.html) package in R

## Steps to Install and Run the Shiny App

### 1. Clone the GitHub Repository

Open a terminal or command prompt and use the following command to clone
the repository:

``` bash
git clone https://github.com/icb-dcm/cOmicsArt.git
```

### 2. Navigate to the Project Directory

Change the directory to the cloned repository:

``` bash
cd cOmicsArt
```

## 3. Restore the R Environment

The project uses renv to manage dependencies. Restore the required R
packages using the renv.lock file. Open R or RStudio. Ensure the package
`renv` is installed in your R environment.Test with

``` r
library(renv)
```

If you get an error, install it using the following command:

``` r
install.packages("renv")
```

Then set the working directory to the root directory to install the
environment from the lock file:

``` r
renv::restore(lockfile="renv.lock")
```

This will install all the necessary packages as specified in the
renv.lock file. **Note:** This takes quite some time as there are a lot of packages to retrieve. 
Some of those need specific system dependencies. 

## 4. Start the Shiny App

From the R console, start the Shiny app using the following command.
Note that you will need to be in the `program` directory.

``` r
shiny::runApp('shinyApp',port=3939)
```

After starting the Shiny app, you will see an IP address printed in the
R console:

``` r
Listening on http://127.0.0.1:3939
```

Open your web browser and go to the provided IP address to access the
Shiny app.

# Running a cOmicsART Using Docker

This guide will help you use the provided Docker image to start your
Shiny app.

## Prerequisites

### 1.  **Install Docker**

Ensure Docker is installed on your system. You can download and install Docker from 
[Docker's official website](https://www.docker.com/get-started).

## Steps to Install and Run the Shiny App

### 2. Pull the Docker Image

Open a terminal or command prompt and use the following command to pull the Docker image from Docker Hub:

```bash
docker pull pauljonasjost/comicsart:latest
```

### 3. Run the Docker Container

After pulling the image, you can run the Docker container with the following command:

```bash
docker run -p 3838:3838 pauljonasjost/comicsart:latest
```

This command does the following:
- `-p 3838:3838` maps port 3838 in the Docker container to port 3838 on your local machine.
- `pauljonasjost/comicsart:latest` specifies the Docker image to run.

### 4. Access the Shiny App

Once the container is running, open your web browser and navigate to:

```bash
http://localhost:3838
```

This will open the Shiny app in your browser.
Note, that this intitially may take some time due to initializing.


### 5. Update the Docker Image

To update the Docker image with the latest version, pull the image again:

```bash
docker pull pauljonasjost/comicsart:latest
```

Then follow the steps to run the updated image.

### Troubleshooting

If you encounter issues, consider the following tips:

- **Port Conflicts**: If port 3838 is already in use, map the container's port to a different local port, e.g., 8888:

  ```bash
  docker run -p 8888:3838 username/shinyapp:latest
  ```

  Then access the app at `http://localhost:8888`.

- **Permissions Issues**: On Linux, you may need to use `sudo` for Docker commands.

.....
