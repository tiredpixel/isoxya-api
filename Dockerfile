FROM docker.io/library/haskell@sha256:aaa408ad7e7eff6cd76c39feae223db7ba3550b937e833e78958478334fc9afe AS builder

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

COPY --chown=x:x ["*.cabal", "cabal.project.freeze", "./"]

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

CMD ["cabal", "run", "isoxya-api", "--", \
    "-b", "0.0.0.0", "-p", "80"]

EXPOSE 80

HEALTHCHECK CMD curl -fs http://localhost || false
#===============================================================================
FROM docker.io/library/debian@sha256:ebe4b9831fb22dfa778de4ffcb8ea0ad69b5d782d4e86cab14cc1fded5d8e761

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
