elm make Main.elm --output raw-test.js
sh elm-stuff/packages/laszlopandy/elm-console/1.1.0/elm-io.sh raw-test.js test.js
node test.js
rm test.js raw-test.js
