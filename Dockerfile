# BUILD
FROM haskell:9.6.6 AS build

## Haskell image uses Debian
RUN apt update
RUN apt upgrade -y

## Need 'pg_config'
RUN apt install -y postgresql
## Need 'pq' library
RUN apt install -y libpq-dev

## App
RUN mkdir -p /opt/sagenda
WORKDIR /opt/sagenda

## Refresh stack dependencies
RUN stack update

## Grab only dependencies first so Docker can cache this as a layer
COPY ./package.yaml /opt/sagenda/package.yaml
COPY ./stack.yaml /opt/sagenda/stack.yaml
COPY ./stack.yaml.lock /opt/sagenda/stack.yaml.lock
RUN stack build --only-dependencies

## Now build app
COPY . /opt/sagenda
RUN stack build --system-ghc \
                --no-install-ghc \
                --local-bin-path /opt/sagenda/build \
                --copy-bins

# RUN
FROM ubuntu:24.04
RUN mkdir -p /opt/sagenda
WORKDIR /opt/sagenda
RUN apt-get update
RUN apt-get install -y libpq-dev
COPY --from=build /opt/sagenda/build/Sagenda-exe /opt/sagenda/Sagenda-exe
CMD ["/opt/sagenda/Sagenda-exe"]
