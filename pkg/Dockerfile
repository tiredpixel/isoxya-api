# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:b781e7b3fab05b3c42d559843a17625dafca98a2a11f19d1ca012bf366984d3d

ARG USER=x
ARG HOME=/home/x
#-------------------------------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libssl-dev \
        netbase \
        sqlite3 && \
    rm -rf /var/lib/apt/lists/*

RUN useradd ${USER} -d ${HOME} && \
    mkdir -p ${HOME}/bin && \
    chown -R ${USER}:${USER} ${HOME}
#-------------------------------------------------------------------------------
COPY bin/ ${HOME}/bin/
#-------------------------------------------------------------------------------
USER ${USER}

WORKDIR ${HOME}

ENV PATH=${HOME}/bin:$PATH \
    LANG=C.UTF-8 \
    LOG_LEVEL=Info

CMD ["isoxya-api", "-b", "localhost", "-p", "8000"]

EXPOSE 8000

HEALTHCHECK CMD curl -fs http://localhost:8000 || false
