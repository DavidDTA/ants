live:
	elm-live src/elm/Main.elm --open --dir=./build -- --output=build/index.html

publish:
	elm make src/elm/Main.elm --optimize --output=docs/index.html
