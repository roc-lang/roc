rm -rf dist/

roc run main.roc -- content/ dist/

cp -r static/* dist/

cd dist

simple-http-server -p 8000