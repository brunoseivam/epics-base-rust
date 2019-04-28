Original README: README.EPICS

# EPICS Base with modifications in Rust

#### Final project for COMS W6156 - Topics in Software Engineering
#### Spring 2019 - Bruno Martins (bm2787)

## Overview

The EPICS base original code can be found [here](https://github.com/epics-base/epics-base).
This version of the code is based off of EPICS Base R7.0.2.1.

## Code changed/introduced

The following files were modified/introduced for this project:

    modules/libcom/src/Makefile          # To include compilation of the Rust module
    modules/libcom/src/iocsh/Makefile    # To prevent compilation of the original iocsh module
    modules/libcom/src/iocsh-rust/*      # Rust implementaion of iocsh

## How to compile

Instructions on how to compile this project on a Debian/Ubuntu system.

    sudo apt install git build-essential libreadline-dev
    curl https://sh.rustup.rs -sSf | sh   # Press 1 at the prompt
    source $HOME/.cargo/env   # Needed for having cargo/rustc in the path
    
    git clone https://github.com/brunoseivam/epics-base-rust  # This repository
    cd epics-base-rust
    make  # This takes a while. To recompile, issue 'make distclean all'


## How to test

This project replaces the iocsh module, the EPICS IOC shell. This shell is used in EPICS IOCs. 
There is one pre-compiled IOC that can be used. To run it:

    ./bin/linux-x86_64/softIoc
    
Once `softIoc` is running, there are several commands that are available. Type `help` to see a list of commands:

    epics> help
    <... list of commands ...>

Help on a particular command can be obtained:

    epics> help dbl
    dbl 'record type' fields

A few commands are familiar:

    epics> pwd
    /home/bmartins/workspace/epics-base-rust
    epics> echo "hello!"
    hello!

EPICS IOCs are genearlly used to keep a "distributed database" of live data. There's a test database that can be loaded:

    epics> dbLoadRecords("db/iocsh_test.db")
    epics> # dbl means DataBase List
    epics> dbl
    SUM
    A
    B

This database exposes three Process Variables (PVs): A, B and SUM. SUM is the result of A + B, and is refreshed every second. 
To see it in action, we need to first run the IOC initialization routine:

    epics> iocInit
    Starting iocInit
    ############################################################################
    ## EPICS R7.0.2.1
    ## EPICS Base built Apr 27 2019
    ############################################################################
    iocRun: All initialization complete

Once it is up, we can start manipulating the PVs:

    epics> # dbpf stands for DataBase Put Field
    epics> dbpf A 1
    DBF_DOUBLE:         1
    epics> dbpf B 2
    DBF_DOUBLE:         2
    epics> # dbgf stands for DataBase Get Field
    epics> dbgf SUM
    DBF_DOUBLE:         3

