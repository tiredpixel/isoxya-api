#===============================================================================
# FROMFREEZE docker.io/library/haskell:8.8
FROM docker.io/library/haskell@sha256:b6ef32c8805ef8601db1c90fae177c9b8c069fb992660ebc5c2eaae1e71b8ce4

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        daemontools \
        happy \
        hlint \
        jq \
        libpcre3-dev \
        libssl-dev \
        sqlite3 && \
    rm -rf /var/lib/apt/lists/*

RUN useradd ${USER} -d ${HOME} && \
    mkdir -p ${HOME}/repo && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}/repo

COPY --chown=x:x [ \
    "cabal.config", \
    "*.cabal", \
    "./"]

RUN cabal v1-update && \
    cabal v1-install -j --only-dependencies --enable-tests
#-------------------------------------------------------------------------------
ENV PATH=${HOME}/repo/bin:${HOME}/.cabal/bin:$PATH \
    LANG=C.UTF-8 \
    LOG_LEVEL=Info

CMD ["cabal", "v1-run", "isx-ce-api", "--", \
    "-b", "0.0.0.0", "-p", "8000"]

EXPOSE 8000

HEALTHCHECK CMD curl -fs http://localhost:8000 || false
#===============================================================================
