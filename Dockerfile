FROM amd64/golang:1.20 AS baseline

ENV DAFNY="/usr/app/ginger/dafny/dafny"

WORKDIR /usr/app

# Initial setup
RUN apt update
RUN apt install unzip
RUN (echo "Y" && cat) | apt install vim
RUN (echo "Y" && cat) | apt install python3-pip
RUN (echo "Y" && cat) | apt install python3-setuptools
RUN (echo "Y" && cat) | apt install python3-tqdm
RUN (echo "Y" && cat) | apt install sudo
# Install Dafny dependencies
RUN (echo "Y" && cat) | apt-get install libicu-dev

FROM baseline AS setup-baseline

FROM setup-baseline AS experimental-data

WORKDIR /usr/app
COPY ./fragments-examples /usr/app/fragments-examples
COPY ./motivating /usr/app/examples/src/motivating

# # # # # # # # # # # # # # # # # # # # #
#    ____  _                            #
#  / ___ ||_|                           #
# | |  |_| _  _ __    __ _    _    _ _  #
# | |     | || '_  \ / _' | / _ \ | '_| #
# | |  __ | || | | || | | || |_| || |   #
# | | |  || || | | || | | ||  ___|| |   #
# | |__| || || | | || |_| || |___ | |   #
#  \_____||_||_| |_| \__, | \____||_|   #
#                    /|_| |             #
#                    \____|             #
# # # # # # # # # # # # # # # # # # # # #
FROM experimental-data AS ginger-quick

# Install Stack & Ginger parsing dependencies
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN (echo "Y" && cat) | apt-get install bnfc
RUN apt install alex
RUN apt install happy

# Install Ginger
COPY ./ginger /usr/app/ginger
WORKDIR /usr/app/ginger
## Initialize Ginger grammars
RUN ./init.sh
## Install and expose Dafny
RUN ./get-dafny.sh
ENV PATH="${PATH}:/usr/app/ginger/dafny"
## Build Ginger and expose executable to PATH
RUN stack build --local-bin-path "/usr/bin" --copy-bins

# Install the result parser
# # Install a newer version of Go for the result parser.
RUN wget https://go.dev/dl/go1.21.1.linux-amd64.tar.gz
RUN rm -rf /usr/local/go && tar -C /usr/local -xzf go1.21.1.linux-amd64.tar.gz
RUN apt update

COPY ./result-parser /usr/app/result-parser
WORKDIR /usr/app/result-parser
RUN go build ginger-result-parser
ENV PATH="${PATH}:/usr/app/result-parser"

# Run the program examples
WORKDIR /usr/app/ginger
RUN bash ./recursive-run.sh "/usr/app/fragments-examples"

# Run the result parser
WORKDIR /usr/app/result-parser
RUN bash ./collect-metrics.sh "/usr/app/fragments-examples"

# Get back to base directory
WORKDIR /usr/app

############################################################################################################

# Run Gomela on motivating example and bundle Gomela with Ginger.
FROM ginger-quick AS gomela-experiment
## Copy Gomela
COPY ./gomela /usr/app/gomela
## Set up Gomela
WORKDIR /usr/app/gomela
RUN go build github.com/nicolasdilley/gomela
ENV PATH="${PATH}:/usr/app/gomela"

# Create Ginger end-to-end
WORKDIR /usr/app

COPY ./ginger-e2e /usr/app/ginger-e2e
ENV ROOT="/usr/app"
ENV PATH="${PATH}:/usr/app"

## Run Gomela and Ginger on the examples
RUN /usr/app/ginger-e2e motivating

############################################################################################################

FROM gomela-experiment AS ginger-benchmark

# Run Ginger on the motivating example
WORKDIR /usr/app/ginger
RUN bash ./recursive-run.sh "/usr/app/gomela-results"

# Get back to base directory
WORKDIR /usr/app

############################################################################################################
# Other tool experiments
FROM experimental-data AS goat-experiment
## Copy Goat
COPY ./goat /usr/app/goat
## Set up Goat
WORKDIR /usr/app/goat
RUN go install golang.org/x/tools/cmd/goimports@v0.2.0
RUN go generate ./...
RUN go build github.com/cs-au-dk/goat
ENV PATH="${PATH}:/usr/app/goat"
## Run Goat on the examples
RUN goat -include-tests -gopath /usr/app/examples -metrics -task collect-primitives -psets gcatch motivating > "/usr/app/goat-results.txt"

FROM experimental-data AS gcatch-experiment
## Copy GCatch
COPY ./gcatch /usr/app/gcatch
## Set up GCatch
WORKDIR /usr/app/gcatch
RUN bash ./installZ3.sh
RUN go build github.com/system-pclub/GCatch/GCatch
ENV PATH="${PATH}:/usr/app/gcatch"
## Run GCatch on the examples
RUN GOPATH="/usr/app/examples" GO111MODULE=off GCatch -timeout 6000000000000 -total-timeout 6000000000000 -pta-timeout 6000000000000 -compile-error -path="/usr/app/examples/src/motivating" -include=motivating -checker=NBMOC:BMOC:unlock:double:structfield:fatal:conflict -r > "/usr/app/gcatch-results.txt"

############################################################################################################

FROM ginger-benchmark AS ginger-complete

# Copy the results from the other tools on the motivating example
COPY --from=goat-experiment /usr/app/goat-results.txt /usr/app/goat-results.txt
COPY --from=gcatch-experiment /usr/app/gcatch-results.txt /usr/app/gcatch-results.txt
