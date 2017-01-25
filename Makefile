PROJECT = exomler
PROJECT_DESCRIPTION = Fast XML parser for Erlang
PROJECT_VERSION = 0.5.0

DEPS = parselib

dep_parselib = git https://github.com/kostyushkin/parselib.git v2.1.0

include erlang.mk
