# FROMFREEZE docker.io/library/debian:10
FROM docker.io/library/debian@sha256:5b57f8c365c40fde437d53b953c436995525be7c481eb0128b1cbf3b49b0df18

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
