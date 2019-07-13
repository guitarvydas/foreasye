all: calc_bounds

% : %.pl
	gplc $< --output $@ --no-top-level

