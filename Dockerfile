FROM rocker/tidyverse:4.2.1

# setup workdir
ENV HOME /home/rstudio
WORKDIR ${HOME}

## create directories
RUN mkdir -p /Figures

# disable authentication
ENV DISABLE_AUTH=true

# set the MRAN date to retrieve packages from this daily snapshot
ENV MRAN_BUILD_DATE=2022-11-01

## copy files
COPY . ./

# Install R package dependencies
RUN install2.r -r https://cran.microsoft.com/snapshot/${MRAN_BUILD_DATE} \
--error \
readr \
lubridate \
readxl \
nbpMatching \
furrr \
estimatr \
kableExtra \
patchwork \
ggtext

