set shell := ["sh", "-c"]
set unstable := true

BUILD_DIR := "builddir"
EXE := "astal-bar"

default:
  @just --list

build:
  ghc --make -Wall -outputdir {{BUILD_DIR}} -O -o {{BUILD_DIR}}/{{EXE}} src/{*/*.hs,*.hs}

run: build
  ./{{BUILD_DIR}}/{{EXE}}
