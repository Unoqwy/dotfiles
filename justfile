alias i := install

default: install-all

install-all:
    ./bin/install

install TAGS:
    ./bin/install --tags {{TAGS}}

theme *args='':
    ./bin/theme {{args}}
