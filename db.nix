{ reflex-platform ? import ./reflex-platform {}
, nixpkgs ? import <nixpkgs> {}
}:
let
	# nixpkgs = reflex-platform.nixpkgs; 
  postgres = nixpkgs.postgresql96;
in
nixpkgs.writeScript "run-webdriver-tests" ''
	${postgres}/bin/initdb -D databrary-db
	${postgres}/bin/pg_ctl start -w -D databrary-db
	${postgres}/bin/createdb databrary
''
