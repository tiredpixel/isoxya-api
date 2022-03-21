# FROMFREEZE docker.io/library/haskell:8.10
FROM docker.io/library/haskell@sha256:5df319ffea19aa4a39db9047f173c5318952995b84092bf14fa014b43df44c46

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        daemontools \
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
    "cabal.project.freeze", \
    "*.cabal", \
    "./"]

RUN cabal update && \
    cabal build --only-dependencies --enable-tests
#-------------------------------------------------------------------------------
ENV PATH=${HOME}/repo/bin:${HOME}/.cabal/bin:$PATH \
    LANG=C.UTF-8 \
    LOG_LEVEL=Info

CMD ["cabal", "run", "isoxya-api", "--", \
    "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
