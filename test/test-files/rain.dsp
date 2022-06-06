import("stdfaust.lib");

rain(density,level) = no.multinoise(2) : par(i, 2, drop) : par(i, 2, *(level))
	with {
		drop = _ <: @(1), (abs < density) : *;
	};

process  = 	rain (
				hslider("v:rain/density", 300, 0, 1000, 1) / 1000,
				hslider("v:rain/volume", 0.5, 0, 1, 0.01)
			);