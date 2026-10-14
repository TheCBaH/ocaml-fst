ARG VARIANT=13
FROM debian:${VARIANT}
# gcc and zlib build the libfst reference tool that writes the test corpus (test/reference).
# Verilator and g++ build the real-simulator fixture (make verilator-test); they are installed
# for the 64-bit hosts only, where that test runs.
RUN set -eux;\
    apt-get update;\
    case "$(dpkg --print-architecture)" in\
      amd64|arm64) extra='g++ verilator' ;;\
      *) extra='' ;;\
    esac;\
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends\
  ca-certificates\
  curl\
  gcc\
  git\
  libc6-dev\
  make\
  zlib1g-dev\
  $extra\
;\
    apt-get autoremove -y; apt-get clean -y; rm -rf /var/lib/apt/lists/*;\
    echo DONE
