FROM haskell:9.6.6

# Haskell image uses Debian
RUN apt update
RUN apt upgrade -y

# Need 'pg_config'
RUN apt install -y postgresql
# Need 'pq' library
RUN apt install -y libpq-dev

# App
WORKDIR /opt/sagenda

# Refresh stack dependencies
RUN stack update

# Grab only dependencies first so Docker can cache this as a layer
COPY ./package.yaml /opt/sagenda/package.yaml
COPY ./stack.yaml /opt/sagenda/stack.yaml
COPY ./stack.yaml.lock /opt/sagenda/stack.yaml.lock
RUN stack build --only-dependencies

# Now build app
COPY . /opt/sagenda
RUN stack build

# And run it
# CMD ["/opt/sagenda/.stack-work/dist/x86_64-linux/ghc-9.6.6/build/Sagenda-exe/Sagenda-exe"]
CMD ["stack", "run"]
