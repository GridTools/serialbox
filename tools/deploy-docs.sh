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

#
# Copy documentation from `docs/sphinx/_build/html/` to `docs/deploy`
#
ROOT=$(git rev-parse --show-toplevel)
HTML_FOLDER=$ROOT/docs/sphinx/_build/html
DEPLOY_FOLDER=$ROOT/_deploy

# Check existence
if [ ! -d "$HTML_FOLDER" ]; then
  echo "error: html of documentation does not exist. Run 'make docs' first!"
  exit 1
fi

# Remove old deploy folder
rm -rf $DEPLOY_FOLDER

# Copy
cp -r $HTML_FOLDER $DEPLOY_FOLDER

#
# Directly deploy `docs/deploy` to the gh-pages branch
# (http://www.damian.oquanta.info/posts/one-line-deployment-of-your-site-to-gh-pages.html)
#
#git subtree split --prefix docs/deploy -b gh-pages
#git push -f origin gh-pages:gh-pages
#git branch -D gh-pages

#
# Cleanup (remove deploy folder)
#
rm -rf $DEPLOY_FOLDER
