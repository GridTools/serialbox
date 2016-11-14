#!/bin/bash
##===-------------------------------------------------------------------------------*- bash -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## Directly deploy the documentation to the gh-pages branch.
##
##===------------------------------------------------------------------------------------------===##

pushd() {
  command pushd "$@" > /dev/null
}

popd() {
  command popd "$@" > /dev/null
}

ROOT=$(git rev-parse --show-toplevel)
HTML_FOLDER=$ROOT/docs/sphinx/_build/html
GH_PAGES_FOLDER=$ROOT/gh-pages
REMOTE_URL=$(git remote get-url --push origin)

if [ ! -d "$HTML_FOLDER" ]; then
  echo "error: html of documentation does not exist. Run 'make docs' first!"
  exit 1
fi

pushd $(pwd)

if [ ! -d "$GH_PAGES_FOLDER" ]; then
  echo -e "\e[1m>> Cloning branch \"gh-pages\" into $GH_PAGES_FOLDER ... \e[0m"
  git clone -b gh-pages $REMOTE_URL $GH_PAGES_FOLDER
fi

echo -e "\e[1m>> Copying directory to gh-pages ... \e[0m"
cp -r $HTML_FOLDER/* $GH_PAGES_FOLDER

cd $GH_PAGES_FOLDER

echo -e "\e[1m>> Pull remote ...\e[0m"
git pull origin gh-pages

echo -e "\e[1m>> Adding changes ...\e[0m"
git add .

echo -e "\e[1m>> Commiting changes ...\e[0m"
git commit -m "update gh-pages"

echo -e "\e[1m>> Pushing changes to remote ...\e[0m"
git push origin gh-pages

popd

