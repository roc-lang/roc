DEST=../mock-repl-deploy

rm -rf $DEST/*

cp dist/* $DEST/

VERSION_SHA=$(git rev-parse HEAD)

cd $DEST
git add .
git commit -m "Deployed from $VERSION_SHA"
git push
