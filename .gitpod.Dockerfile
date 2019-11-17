FROM gitpod/workspace-full

USER root

RUN apt-get update \
    && apt-get install -y libboost-all-dev \
    && apt-get clean && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/* && rm -rf /tmp/*

ARG PFUNIT_VERSION=3.2.9
ARG PFUNIT_FILE=pFUnit-${PFUNIT_VERSION}.tgz
ARG PFUNIT_LINK=https://downloads.sourceforge.net/project/pfunit/Source/${PFUNIT_FILE}
ARG PFUNIT_DIR=pFUnit-${PFUNIT_VERSION}
RUN wget $PFUNIT_LINK && tar xf $PFUNIT_FILE && cd $PFUNIT_DIR && mkdir -p build && cd build \
    && cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local && make -j8 install \
    && cd ../.. && rm $PFUNIT_FILE && rm -rf $PFUNIT_DIR

USER gitpod

RUN pip3 install nose numpy

USER root
