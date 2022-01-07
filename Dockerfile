# FROMFREEZE docker.io/library/haskell:8.10
FROM docker.io/library/haskell@sha256:6b4948a36e40b66a9cd6bfc279df43958d1aebc67a3909ba0c521d1d1395d26f

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
