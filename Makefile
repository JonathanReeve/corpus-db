compile: 
	nix-shell --run 'cd src/ && ghc Main.hs'

start:
	cd src && sudo ENV=prod ./Main
