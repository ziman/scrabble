#!/bin/sh

stack build --fast \
  && exec stack exec backend -- "$@"
