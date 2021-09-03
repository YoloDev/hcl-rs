ARG RAGEL_VERSION=7.0.4

FROM yolodev/ragel:v${RAGEL_VERSION}

unicode:
  COPY crates/lex/src/unicode2ragel.rb .
  RUN ruby unicode2ragel.rb --url=http://www.unicode.org/Public/9.0.0/ucd/DerivedCoreProperties.txt -m UnicodeDerived -p ID_Start,ID_Continue -o unicode_derived.rl
  SAVE ARTIFACT unicode_derived.rl

ragel:
  COPY +unicode/unicode_derived.rl .
  COPY crates/lex/src/scanner.rl .
  RUN ragel-rust -e -F1 scanner.rl && rustfmt scanner.rs
  SAVE ARTIFACT scanner.rs

scanner:
  COPY +unicode/unicode_derived.rl .
  COPY +ragel/scanner.rs .
  SAVE ARTIFACT unicode_derived.rl AS LOCAL crates/lex/src/unicode_derived.rl
  SAVE ARTIFACT scanner.rs AS LOCAL crates/lex/src/scanner.rs

create-image:
  FROM rust:bullseye
  ARG COLM_VERSION=0.14.7
  ARG RAGEL_VERSION=7.0.4

  ENV DEBIAN_FRONTEND="noninteractive"
  ENV COLM_VERSION=${COLM_VERSION}
  ENV RAGEL_VERSION=${RAGEL_VERSION}

  # Build dependencies we can get from apt.
  RUN apt-get update \
    && apt-get install -y \
      git libtool autoconf automake g++ gcc make \
      curl clang gnupg gdc default-jdk \
      ruby mono-mcs golang ocaml julia \
    && rm -rf /var/lib/apt/lists/* \
    && rustup component add rustfmt

  RUN mkdir /build && cd /build \
    && curl https://www.colm.net/files/thurston.asc | gpg --import - \
    && curl -O https://www.colm.net/files/colm/colm-${COLM_VERSION}.tar.gz \
    && curl -O https://www.colm.net/files/colm/colm-${COLM_VERSION}.tar.gz.asc \
    && curl -O https://www.colm.net/files/ragel/ragel-${RAGEL_VERSION}.tar.gz \
    && curl -O https://www.colm.net/files/ragel/ragel-${RAGEL_VERSION}.tar.gz.asc \
    && gpg --verify colm-${COLM_VERSION}.tar.gz.asc colm-${COLM_VERSION}.tar.gz \
    && gpg --verify ragel-${RAGEL_VERSION}.tar.gz.asc ragel-${RAGEL_VERSION}.tar.gz \
    && tar -zxvf colm-${COLM_VERSION}.tar.gz \
    && tar -zxvf ragel-${RAGEL_VERSION}.tar.gz \
    && cd /build/colm-${COLM_VERSION} \
    && ./configure --prefix=/opt/colm.net/colm --disable-manual \
    && make -j && make install \
    && cd /build/ragel-${RAGEL_VERSION} \
    && ./configure --prefix=/opt/colm.net/ragel --with-colm=/opt/colm.net/colm --disable-manual \
    && make -j && make install \
    && cd / && rm -rf /build

  ENV PATH="/opt/colm.net/ragel/bin:${PATH}"

  WORKDIR /work
  SAVE IMAGE --push yolodev/ragel:v${RAGEL_VERSION}

test-image:
  FROM +create-image

  COPY crates/lex/src/scanner.rl crates/lex/src/unicode_derived.rl .
  RUN ragel-rust -e -F1 scanner.rl
  RUN rustfmt scanner.rs
