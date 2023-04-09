FROM ubuntu:latest 

ARG FPM_BIN="https://github.com/fortran-lang/fpm/releases/download/v0.7.0/fpm-0.7.0-linux-x86_64"
ENV CC=gcc
ENV FC=gfortran
ENV COMP_SYS=gnu_linux
ENV LD_LIBRARY_PATH=/usr/local/lib
ENV DEBIAN_FRONTEND noninteractive

WORKDIR /mnt

# Get tools needed to build code
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      wget \ 
      build-essential \
      libssl-dev \
      bzip2 \
      tar \
      git \
      amqp-tools \
      openssh-client \
      gfortran \
      software-properties-common \
      gpg-agent \
      m4 \
      libpng-dev \
      mpich \
      python3-pip \
      graphviz \ 
      lsb-release \
      && rm -rf /var/lib/apt/lists/*

# Add gfortran-11 and configure it as default fortran compiler 
RUN add-apt-repository ppa:ubuntu-toolchain-r/test && \
      apt-get update && apt-get install -y --no-install-recommends \
      gfortran-11

RUN update-alternatives --install /usr/bin/gfortran gfortran \
      /usr/bin/gfortran-11 11 

# Install fpm
RUN cd /tmp && \
  wget --no-check-certificate "${FPM_BIN}" && \ 
  chmod +x $(basename "${FPM_BIN}") && \
  cp $(basename ${FPM_BIN}) /usr/bin && \
  cd /usr/bin && \
  ln -sf /usr/bin/$(basename "${FPM_BIN}") fpm

# Intall packages for fortran dev environment
RUN pip3 install fortls \
  fprettify \
  graphviz \
  markdown \ 
  ford

# Install arrow and parquet
RUN wget "https://apache.jfrog.io/artifactory/arrow/$(lsb_release --id --short | \
  tr 'A-Z' 'a-z')/apache-arrow-apt-source-latest-$(lsb_release --codename --short).deb" && \
  apt install -y -V ./apache-arrow-apt-source-latest-$(lsb_release --codename --short).deb && \
  apt update && \
  apt install -y -V libarrow-dev \ 
  libarrow-glib-dev \
  libarrow-dataset-dev \
  libarrow-dataset-glib-dev \
  libparquet-dev \ 
  libparquet-glib-dev


