---
layout: default
title: "Local Installation"
nav_order: 5
editor_options: 
  markdown: 
    wrap: 72
---

# Local Installation

![A comic about a cat finding
cOmicsART](/cOmicsArt/assets/images/cOmicsTurtle.png) *Image generated
using DALL-E by OpenAI. Adjusted by Lea Seep*

Why do you want to install cOmicsART locally? If you just want to use it make sure to check out the website:
[cOmicsART](https://shiny.iaas.uni-bonn.de/cOmicsArt/). Here
is no installation effort required. If you know you are right here,
let's get started. You can find here instructions to run cOmicsART locally within RStudio or using Docker.

# Running a cOmicsART locally within RStudio

This guide provides detailed instructions on how to install and run the
Shiny app from the provided GitHub repository.

## Prerequisites

Ensure you have the following software installed on your system: -
[Git](https://git-scm.com/) - [R](https://www.r-project.org/) -
[RStudio](https://rstudio.com/products/rstudio/download/) -
[renv](https://rstudio.github.io/renv/articles/renv.html) package in R.

<div class="disclaimer" style="background-color:#fff0bf; color: black; border: 2px solid #ffcf30; border-radius: 8px; padding:0.2em;">
<span>
<p style='margin-top:1em; text-align:left ;margin-left:1em;'>
<b>Note:</b> cOmicsArt is built on version 4.2.0 of R. To run cOmicsArt locally, please make sure you have version 4.2.0 installed.
<br/>
<b>For Windows users:</b> In addition to the above mentioned software, [Rtools42](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html) is required to build pacakges from source.
</p></span>
</div> 

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
Some of those need specific system dependencies. Also note that you can use R within the provided Docker image, which comes with a fully preloaded and ready-to-use environment. See #Running a cOmicsART Using Docker.

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

<div class="disclaimer" style="background-color:#fff0bf; color: black; border: 2px solid #ffcf30; border-radius: 8px; padding:0.2em;">
<span>
<p style='margin-top:1em; text-align:left ;margin-left:1em;'>
<b>Apple Silicon (M1/M2/M3) users:</b> The image is built for the
<code>linux/amd64</code> architecture and runs under emulation on Apple
Silicon. <b>App mode works out of the box.</b> For the <b>development
(RStudio) mode</b> you must enable Apple's Virtualization framework and
Rosetta in Docker Desktop, otherwise RStudio will show
<i>"Unable to connect to service"</i>:
<br/><br/>
Docker Desktop &rarr; <b>Settings</b> &rarr; <b>General</b> &rarr; set
<b>Virtual Machine Manager (VMM)</b> to <b>Apple Virtualization
framework</b> &rarr; tick <b>"Use Rosetta for x86_64/amd64 emulation on
Apple Silicon"</b> &rarr; <b>Apply &amp; Restart</b>. Requires macOS 13
(Ventura) or newer.
</p></span>
</div>

## Steps to Install and Run the Shiny App

### 2. Pull the Docker Image

Open a terminal or command prompt and use the following command to pull the Docker image from Docker Hub:

```bash
docker pull pauljonasjost/comicsart:latest
```

### 3. Run the Docker Container (App mode)

After pulling the image, you can run the Docker container with the following command:

```bash
docker run --rm -p 3838:3838 pauljonasjost/comicsart:latest
```

This command does the following:
- `--rm` removes the container automatically when you stop it.
- `-p 3838:3838` maps port 3838 in the Docker container to port 3838 on your local machine.
- `pauljonasjost/comicsart:latest` specifies the Docker image to run.

The image supports two modes, selected with the `MODE` environment
variable: `MODE=app` (the default, shown above) runs the Shiny app, and
`MODE=rstudio` starts an RStudio Server for development (see
[Development mode (RStudio)](#development-mode-rstudio) below). Because
`app` is the default, no `-e MODE=...` flag is needed to run the app.

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

## Development mode (RStudio)

The same image can start an **RStudio Server**, giving you the app's
fully preloaded R environment inside a browser-based IDE. This is the
recommended way to develop or debug the app without setting up renv
locally.

> **Apple Silicon:** development mode requires the Apple Virtualization
> framework + Rosetta to be enabled first — see the note under
> [Install Docker](#1--install-docker).

Clone the repository (so your edits are saved to your machine), then
start the container in `rstudio` mode with your local `program/` folder
mounted into it:

```bash
git clone https://github.com/icb-dcm/cOmicsArt.git
cd cOmicsArt

docker run --rm -p 8787:8787 \
  -e MODE=rstudio \
  -e PASSWORD=yourpassword \
  -v "$PWD/program":/home/rstudio/project \
  pauljonasjost/comicsart:latest
```

This does the following:
- `-p 8787:8787` maps RStudio Server's port to your machine.
- `-e MODE=rstudio` starts RStudio Server instead of the app.
- `-e PASSWORD=yourpassword` sets the login password (choose your own).
- `-v "$PWD/program":/home/rstudio/project` mounts your local `program/`
  folder into the container at `~/project`, so any changes you make are
  written back to your machine.

Then open [http://localhost:8787](http://localhost:8787) and log in with
username `rstudio` and the password you set. Your mounted code is in the
`project/` folder. To launch the app from within RStudio:

```r
shiny::runApp("project/shinyApp")
```

Because the environment is baked into the image, packages such as
`DESeq2` and `ggtree` load immediately — no `renv::restore()` needed.

<details>
<summary>Alternative: plain shell / VS Code Dev Containers</summary>

If you prefer a terminal or VS Code instead of RStudio, you can open a
shell in the same environment:

```bash
docker run -it --rm \
  -v "$PWD":/workspace \
  -w /workspace \
  --name comicsart_dev \
  pauljonasjost/comicsart:latest bash
```

For an IDE, use VS Code + the **Dev Containers** extension: start the
container, then in VS Code open the command palette and select
*Dev Containers: Attach to Running Container...*. Open your mounted local
folder so saved changes persist on your machine.

</details>


### Troubleshooting

If you encounter issues, consider the following tips:

- **Port Conflicts**: If port 3838 is already in use, map the container's port to a different local port, e.g., 8888:

  ```bash
  docker run -p 8888:3838 username/shinyapp:latest
  ```

  Then access the app at `http://localhost:8888`.

- **Permissions Issues**: On Linux, you may need to use `sudo` for Docker commands.

- **RStudio "Unable to connect to service" (Apple Silicon)**: Development
  mode needs Docker Desktop's Apple Virtualization framework + Rosetta
  enabled (Settings → General → VMM: *Apple Virtualization framework* →
  *Use Rosetta for x86_64/amd64 emulation*). App mode is unaffected.

.....
