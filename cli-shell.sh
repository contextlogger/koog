#! /bin/sh
exec mzscheme --name "$0" --eval '(require (lib "cli.ss" "koog")) (main)' -- ${1+"$@"}
