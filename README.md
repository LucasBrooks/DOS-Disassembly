# DOS-Disassembly
Various disassembled 86-DOS/MS-DOS/PC-DOS components. DOS 1.x only.

The main goal right now is to produce identical binaries after re-assembly, not to annotate or to document the source code. Once all files are successfully disassembled, the next step is to switch to the Microsoft/SCP/IBM coding style and remove any remaining hard coded offset references. The final goal is then to add meaningful labels, variable names, and comments.

Currently labels and functions are named `LABxxx`, ASCII strings are named `STRxxx`, and everything else are named `DATxxx`.

For disassembled 86-DOS/MS-DOS components, the `IBMVER` switch is ignored. For disassembled PC-DOS components, the `MSVER` switch is ignored.

# Copyright
This project's goal is to complete the MS-DOS 1.25 source code released by Microsoft by providing disassembly of various other DOS components, so see the official MS-DOS source code repo for the LICENSE file. All code in this repo written by either Microsoft, Seattle Computer Products, or IBM.

# Note
All source files are named `XXX.ASM.BIN` to prevent GitHub from automatically converting `CRLF` to `LF` and appending an empty line to the end. You may wish to remove the `.BIN` extension.
