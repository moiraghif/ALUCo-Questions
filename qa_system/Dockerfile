FROM openjdk:8u262-slim
MAINTAINER moiraghif <f.moiraghimotta@campus.unimib.it>

RUN apt update


# localtion of the source code
RUN mkdir /app
WORKDIR /app


# sbt installation
RUN apt install -y curl gnupg

RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list &&\
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt


# udipipe models download
ARG UDPIPE_VERSION=1.2.0
ARG UDPIPE_FILE=udpipe-${UDPIPE_VERSION}-bin
RUN apt install -y wget unzip

RUN wget "https://github.com/ufal/udpipe/releases/download/v${UDPIPE_VERSION}/${UDPIPE_FILE}.zip" && \
    unzip ${UDPIPE_FILE}.zip && \
    rm ${UDPIPE_FILE}.zip && \
    mkdir /app/lib && \
    cp ./${UDPIPE_FILE}/bin-linux64/java/libudpipe_java.so ./ && \
    cp ./${UDPIPE_FILE}/bin-linux64/java/udpipe.jar ./lib/ && \
    rm -r ${UDPIPE_FILE}


ARG MODELS_VERSION=2.5-191206
ARG MODELS_FILE=udpipe-ud-${MODELS_VERSION}
RUN wget "https://lindat.mff.cuni.cz/repository/xmlui/bitstream/handle/11234/1-3131/${MODELS_FILE}.zip" && \
    unzip ${MODELS_FILE}.zip && \
    mv ${MODELS_FILE} models && \
    rm -r ${MODELS_FILE}.zip


# compile the project
RUN sbt compile


EXPOSE 8586
CMD sbt run
