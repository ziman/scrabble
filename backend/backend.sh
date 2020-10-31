#!/bin/sh

stack build \
  && exec stack exec backend -- "$@"
