FROM docker.io/library/haskell@sha256:a81f289e6ae9beaf53d81a9872b0906c2db2ef48298f6aeb83e2d52e356a5b81 AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        jq \
        libpcre3-dev \
        libssl-dev \
        sqlite3 \
        && \
    rm -rf /var/lib/apt/lists/*

RUN useradd x -m && \
    mkdir /home/x/r && \
    chown -R x:x /home/x
#-------------------------------------------------------------------------------
USER x

WORKDIR /home/x/r

COPY --chown=x:x *.cabal cabal.project.freeze ./

RUN cabal update && \
    cabal build --only-dependencies --enable-tests

COPY --chown=x:x . .

ARG GIT_DESCRIBE

RUN VERSION=$(echo "$GIT_DESCRIBE" | sed 's/-/./') && \
    sed -i -E "s/(version: *)0.0.0/\1$VERSION/" ./*.cabal && \
    cabal install -O2
#-------------------------------------------------------------------------------
ENV PATH=/home/x/r/bin:/home/x/.cabal/bin:$PATH \
    LANG=C.UTF-8

CMD ["run"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
#===============================================================================
FROM docker.io/library/debian@sha256:fb9654aac57319592f1d51497c62001e7033eddf059355408a0b53f7c71f8d5f

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libssl-dev \
        netbase \
        sqlite3 \
        && \
    rm -rf /var/lib/apt/lists/*

RUN useradd x -m && \
    mkdir /home/x/bin && \
    chown -R x:x /home/x
#-------------------------------------------------------------------------------
COPY --from=builder /home/x/.cabal/bin/* /home/x/bin/
#-------------------------------------------------------------------------------
USER x

WORKDIR /home/x

ENV PATH=/home/x/bin:$PATH \
    LANG=C.UTF-8

CMD ["isoxya-api", "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
