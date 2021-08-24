# Project setup
PROJ      = blinky
BUILD     = ./build
DEVICE    = 8k
PINMAP    = icoboard.pcf

CLASH = clash

# Files
FILE = Top

.PHONY: all clean verilog bitstream burn

all: bitstream

verilog:
	# if build folder doesn't exist, create it
	mkdir -p $(BUILD)
	# TODO copy native verilog files over
	cabal v2-run clash --  $(FILE) --verilog  -outputdir $(BUILD)

bitstream: verilog
	# synthesize using Yosys
	yosys -p "synth_ice40 -blif $(BUILD)/$(PROJ).blif" $$(find $(BUILD)/verilog/ -type f -iname '*.v' | grep -v testbench.v)
	# Place and route using arachne
	arachne-pnr -d $(DEVICE) -o $(BUILD)/$(PROJ).asc -p $(PINMAP) $(BUILD)/$(PROJ).blif
	# Convert to bitstream using IcePack
	icepack $(BUILD)/$(PROJ).asc $(BUILD)/$(PROJ).bin

burn: bitstream
	iceprog $(BUILD)/$(PROJ).bin

show:
	yosys -p "show" $$(find $(BUILD)/verilog/ -type f -iname '*.v' | grep -v testbench.v)

clean:
	rm ./$(BUILD)/* -rf
