FROM debian:stretch

ARG NB_USER="hask"
ARG NB_UID="1000"
ARG NB_GID="100"

USER root

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg2 ca-certificates dirmngr curl git \
    libicu-dev libtinfo-dev libgmp-dev socat bzip2 locales fonts-liberation sudo && \
    echo 'deb http://downloads.haskell.org/debian stretch main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA3CBA3FFE22B574 && \
    apt-get update && \
    apt-get install -y --no-install-recommends ghc-8.6.3 cabal-install-2.4 \
    zlib1g-dev libtinfo-dev libsqlite3-dev g++ netbase xz-utils make libc6-dev libffi-dev libgmp-dev && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.9.3/stack-1.9.3-linux-x86_64.tar.gz -o stack.tar.gz && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.9.3/stack-1.9.3-linux-x86_64.tar.gz.asc -o stack.tar.gz.asc && \
    export GNUPGHOME="$(mktemp -d)" && \
    mkdir ~/.gnupg && \
    echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf && \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys C5705533DA4F78D8664B5DC0575159689BEFB442 && \
    gpg --batch --verify stack.tar.gz.asc stack.tar.gz && \
    tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1 

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen

ENV SHELL=/bin/bash \
    NB_USER=$NB_USER \
    NB_UID=$NB_UID \
    NB_GID=$NB_GID \
    LC_ALL=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8
ENV HOME=/home/$NB_USER

RUN echo "auth requisite pam_deny.so" >> /etc/pam.d/su && \
    sed -i.bak -e 's/^%admin/#%admin/' /etc/sudoers && \
    sed -i.bak -e 's/^%sudo/#%sudo/' /etc/sudoers && \
    useradd -m -s /bin/bash -N -u $NB_UID $NB_USER && \
    chown $NB_USER:$NB_GID /tmp && \
    chmod g+w /etc/passwd

ENV PATH /root/.local/bin:/opt/cabal/2.4/bin:/opt/ghc/8.6.3/bin:$HOME/.local/bin:$(stack path --local-install-root)/bin:$(stack path --snapshot-install-root)/bin:$(stack path --compiler-bin):$PATH

####### Installing biproductions ########

COPY app/ /tmp/biproductions/app/
COPY src/ /tmp/biproductions/src/
COPY package.yaml /tmp/biproductions/
COPY stack.yaml /tmp/biproductions/
COPY entrypoint.sh /home/

RUN chown -R $NB_USER:$NB_GID /tmp/biproductions && \

USER $NB_UID    

RUN cd /tmp/biproductions && \
    stack install

ENV PATH $PATH:/home/$NB_USER/stack_volume/bin:/home/$NB_USER/.local/bin:/home/$NB_USER/.cabal/bin

# Entrypoint and Working Directory
WORKDIR /home
ENTRYPOINT ["/bin/sh"]
CMD ["/home/entrypoint.sh"]


